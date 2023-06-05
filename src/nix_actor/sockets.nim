# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[asyncdispatch, asyncnet, os, sets, strtabs, strutils]
from std/nativesockets import AF_INET, AF_UNIX, SOCK_STREAM, Protocol

import preserves
import ./protocol, ./store

{.pragma: workerProtocol, importc, header: "worker-protocol.hh".}

type Word = uint64
proc `$`(w: Word): string = toHex(w)

const
  WORKER_MAGIC_1 = 0x6E697863
  WORKER_MAGIC_2 = 0x6478696F
  PROTOCOL_VERSION = 256 or 35

  STDERR_NEXT = 0x6F6C6d67
  STDERR_READ = 0x64617461
  STDERR_WRITE = 0x64617416
  STDERR_LAST = 0x616C7473
  STDERR_ERROR = 0x63787470
  STDERR_START_ACTIVITY = 0x53545254
  STDERR_STOP_ACTIVITY = 0x53544F50
  STDERR_RESULT = 0x52534C54

  wopIsValidPath = 1
  wopHasSubstitutes = 3
  wopQueryReferrers = 6
  wopAddToStore = 7
  wopBuildPaths = 9
  wopEnsurePath = 10
  wopAddTempRoot = 11
  wopAddIndirectRoot = 12
  wopSyncWithGC = 13
  wopFindRoots = 14
  wopSetOptions = 19
  wopCollectGarbage = 20
  wopQuerySubstitutablePathInfo = 21
  wopQueryAllValidPaths = 23
  wopQueryFailedPaths = 24
  wopClearFailedPaths = 25
  wopQueryPathInfo = 26
  wopQueryPathFromHashPart = 29
  wopQuerySubstitutablePathInfos = 30
  wopQueryValidPaths = 31
  wopQuerySubstitutablePaths = 32
  wopQueryValidDerivers = 33
  wopOptimiseStore = 34
  wopVerifyStore = 35
  wopBuildDerivation = 36
  wopAddSignatures = 37
  wopNarFromPath = 38
  wopAddToStoreNar = 39
  wopQueryMissing = 40
  wopQueryDerivationOutputMap = 41
  wopRegisterDrvOutput = 42
  wopQueryRealisation = 43
  wopAddMultipleToStore = 44
  wopAddBuildLog = 45
  wopBuildPathsWithResults = 46

type
  ProtocolError = object of IOError
  Version = uint16
  Session = ref object
    client, daemon: AsyncSocket
    buffer: seq[Word]
    version: Version

func major(version: Version): uint16 = version and 0xff00
func minor(version: Version): uint16 = version and 0x00ff

proc daemonSocketPath: string =
  getEnv(
      "NIX_DAEMON_SOCKET_PATH",
      "/nix/var/nix/daemon-socket/socket")

proc send(session: Session; sock: AsyncSocket; words: varargs[Word]): Future[void] =
  for i, word in words: session.buffer[i] = word
  send(sock, addr session.buffer[0], words.len shl 3)

proc send(session: Session; sock: AsyncSocket; s: string): Future[void] =
  let wordCount = (s.len + 7) shr 3
  if wordCount > session.buffer.len: setLen(session.buffer, wordCount)
  session.buffer[0] = Word s.len
  if wordCount > 0:
    session.buffer[wordCount] = 0x00
    copyMem(addr session.buffer[1], unsafeAddr s[0], s.len)
  send(sock, addr session.buffer[0], (1 + wordCount) shl 3)

proc recvWord(sock: AsyncSocket): Future[Word] {.async.} =
  var w: Word
  let n = await recvInto(sock, addr w, sizeof(Word))
  if n != sizeof(Word): raise newException(ProtocolError, "short read of word")
  return w

proc passWord(a, b: AsyncSocket): Future[Word] {.async.} =
  var w = await recvWord(a)
  await send(b, addr w, sizeof(Word))
  return w

proc recvString(sock: AsyncSocket): Future[string] {.async.} =
  let w = await recvWord(sock)
  let stringLen = int w
  var s: string
  if stringLen > 0:
    s.setLen((stringLen + 7) and (not 7))
    let n = await recvInto(sock, addr s[0], s.len)
    if n != s.len:
      raise newException(ProtocolError, "short string read")
    setLen(s, stringLen)
  return s

proc passString(session: Session; a, b: AsyncSocket): Future[string] {.async.} =
  var s = await recvString(a)
  await send(session, b, s)
  return s

proc passStringSeq(session: Session; a, b: AsyncSocket): Future[seq[string]] {.async.} =
  let count = int(await passWord(a, b))
  var strings = newSeq[string](count)
  for i in 0..<count: strings[i] = await passString(session, a, b)
  return strings

proc passStringSet(session: Session; a, b: AsyncSocket): Future[HashSet[string]] {.async.} =
  let count = int(await passWord(a, b))
  var strings = initHashSet[string](count)
  for i in 0..<count: incl(strings, await passString(session, a, b))
  return strings

proc passStringMap(session: Session; a, b: AsyncSocket): Future[StringTableRef] {.async.} =
  var table = newStringTable(modeCaseSensitive)
  let n = await passWord(a, b)
  for i in 1..n:
    var
      key = await passString(session, a, b)
      val = await passString(session, a, b)
    table[key] = val
  return table

proc passClientWord(session: Session): Future[Word] =
  passWord(session.client, session.daemon)

proc passDaemonWord(session: Session): Future[Word] =
  passWord(session.daemon, session.client)

proc passClientString(session: Session): Future[string] =
  passString(session, session.client, session.daemon)

proc passDaemonString(session: Session): Future[string] =
  passString(session, session.daemon, session.client)

proc passClientStringSeq(session: Session): Future[seq[string]] =
  passStringSeq(session, session.client, session.daemon)

proc passDaemonStringSeq(session: Session): Future[seq[string]] =
  passStringSeq(session, session.daemon, session.client)

proc passClientStringSet(session: Session): Future[HashSet[string]] =
  passStringSet(session, session.client, session.daemon)

proc passDaemonStringSet(session: Session): Future[HashSet[string]] =
  passStringSet(session, session.daemon, session.client)

proc passClientStringMap(session: Session): Future[StringTableRef] =
  passStringMap(session, session.client, session.daemon)

proc passDaemonStringMap(session: Session): Future[StringTableRef] =
  passStringMap(session, session.daemon, session.client)

type ValidPathInfo = object
  path: string
  deriver: string
  narHash: string
  references: HashSet[string]
  registrationTime, narSize: BiggestInt
  ultimate: bool
  sigs: HashSet[string]
  ca: string

proc passDaemonValidPathInfo(session: Session; includePath: bool): Future[PathInfo] {.async.} =
  var info: PathInfo
  if includePath:
    info.path = await passDaemonString(session)
  info.deriver = await passDaemonString(session)
  info.narHash = await passDaemonString(session)
  info.references = await passDaemonStringSet(session)
  info.registrationTime = BiggestInt(await passDaemonWord(session))
  info.narSize = BiggestInt(await passDaemonWord(session))
  assert session.version.minor >= 16
  info.ultimate = (await passDaemonWord(session)) != 0
  info.sigs = await passDaemonStringSet(session)
  info.ca = await passDaemonString(session)
  return info

proc passChunks(session: Session; a, b: AsyncSocket): Future[int] {.async.} =
  var total: int
  while true:
    let chunkLen = int(await passWord(a, b))
    if chunkLen == 0:
      break
    else:
      let wordLen = (chunkLen + 7) shr 3
      if session.buffer.len < wordLen: setLen(session.buffer, wordLen)
      let recvLen = await recvInto(a, addr session.buffer[0], chunkLen)
        # each chunk must be recved contiguously
      if recvLen != chunkLen:
          raise newException(ProtocolError, "invalid chunk read")
      await send(b, addr session.buffer[0], recvLen)
      inc(total, recvLen)
  return total

proc passClientChunks(session: Session): Future[int] =
  passChunks(session, session.client, session.daemon)

proc passErrorDaemonError(session: Session) {.async.} =
  let
    typ = await passDaemonString(session)
  assert typ == "Error"
  let
    lvl = await passDaemonWord(session)
    name = await passDaemonString(session)
    msg = passDaemonString(session)
    havePos = await passDaemonWord(session)
  assert havePos == 0
  let
    nrTraces = await passDaemonWord(session)
  for i in 1..nrTraces:
    let havPos = await passDaemonWord(session)
    assert havPos == 0
    let msg = await passDaemonString(session)

proc passDaemonFields(session: Session): Future[Fields] {.async.} =
  let count = await passDaemonWord(session)
  var fields = newSeq[Field](count)
  for i in 0..<count:
    let typ = await passDaemonWord(session)
    case typ
    of 0:
      let num = await passDaemonWord(session)
      fields[i] = Field(orKind: FieldKind.int, int: int num)
    of 1:
      let str = await passDaemonString(session)
      fields[i] = Field(orKind: FieldKind.string, string: str)
    else:
      raiseAssert "unknown field type " & $typ
  return fields

proc passWork(session: Session) {.async.} =
  while true:
    let word = await passDaemonWord(session)
    case word

    of STDERR_WRITE:
      discard await passDaemonString(session)

    of STDERR_READ:
      discard await passClientString(session)

    of STDERR_ERROR:
      assert session.version.minor >= 26
      await passErrorDaemonError(session)

    of STDERR_NEXT:
      let s = await passDaemonString(session)

    of STDERR_START_ACTIVITY:
      var act: ActionStart
      act.id = BiggestInt(await passDaemonWord(session))
      act.level = BiggestInt(await passDaemonWord(session))
      act.`type` = BiggestInt(await passDaemonWord(session))
      act.text = await passDaemonString(session)
      act.fields = await passDaemonFields(session)
      act.parent = BiggestInt(await passDaemonWord(session))

    of STDERR_STOP_ACTIVITY:
      var act: ActionStop
      act.id = BiggestInt(await passDaemonWord(session))

    of STDERR_RESULT:
      var act: ActionResult
      act.id = BiggestInt(await passDaemonWord(session))
      act.`type` = BiggestInt(await passDaemonWord(session))
      act.fields = await passDaemonFields(session)

    of STDERR_LAST:
      break

    else:
      raise newException(ProtocolError, "unknown work verb " & $word)

#[
proc fromClient(miss: var Missing; socket: AsyncSocket) {.async.} =
  result.targets = await passClientStringSet(session)

proc fromDaemon(miss: var Missing; socket: AsyncSocket) {.async.} =
  miss.willBuild = await passDaemonStringSet(session)
  miss.willSubstitute = await passDaemonStringSet(session)
  miss.unknown = await passDaemonStringSet(session)
  miss.downloadSize = BiggestInt await passDaemonWord(session)
  miss.narSize = BiggestInt await passDaemonWord(session)
]#

proc loop(session: Session) {.async.} =
  var chunksTotal: int
  try:
    while not session.client.isClosed:
      let wop = await passClientWord(session)
      case wop
      of wopIsValidPath:
        let path = await passClientString(session)
        stderr.writeLine "wopIsValidPath ", path
        await passWork(session)
        let word = await passDaemonWord(session)

      of wopAddToStore:
        assert session.version.minor >= 25
        let
          name = await passClientString(session)
          caMethod = await passClientString(session)
          refs = await passClientStringSet(session)
          repairBool = await passClientWord(session)
        stderr.writeLine "wopAddToStore ", name
        let n = await passClientChunks(session)
        inc(chunksTotal, n)
        await passWork(session)
        let info = await passDaemonValidPathInfo(session, true)

      of wopAddTempRoot:
        let path = await passClientString(session)
        stderr.writeLine "wopAddTempRoot ", path
        await passWork(session)
        discard await passDaemonWord(session)

      of wopAddIndirectRoot:
        let path = await passClientString(session)
        stderr.writeLine "wopAddIndirectRoot ", path
        await passWork(session)
        discard await passDaemonWord(session)

      of wopSetOptions:
        discard passClientWord(session) # keepFailed
        discard passClientWord(session) # keepGoing
        discard passClientWord(session) # tryFallback
        discard passClientWord(session) # verbosity
        discard passClientWord(session) # maxBuildJobs
        discard passClientWord(session) # maxSilentTime
        discard passClientWord(session) # useBuildHook
        discard passClientWord(session) # verboseBuild
        discard passClientWord(session) # logType
        discard passClientWord(session) # printBuildTrace
        discard passClientWord(session) # buildCores
        discard passClientWord(session) # useSubstitutes
        assert session.version.minor >= 12
        let overrides = await passClientStringMap(session)
        await passWork(session)

      of wopQueryPathInfo:
        assert session.version >= 17
        let path = await passClientString(session)
        stderr.writeLine "wopQueryPathInfo ", path
        await passWork(session)
        let valid = await passDaemonWord(session)
        if valid != 0:
          var info = await passDaemonValidPathInfo(session, false)
          info.path = path
          stderr.writeLine "wopQueryPathInfo ", $info

      of wopQueryMissing:
        assert session.version >= 30
        var miss: Missing
        miss.targets = await passClientStringSet(session)
        await passWork(session)
        miss.willBuild = await passDaemonStringSet(session)
        miss.willSubstitute = await passDaemonStringSet(session)
        miss.unknown = await passDaemonStringSet(session)
        miss.downloadSize = BiggestInt await passDaemonWord(session)
        miss.narSize = BiggestInt await passDaemonWord(session)
        stderr.writeLine "wopQueryMissing ", $miss

      of wopBuildPathsWithResults:
        assert session.version >= 34
        let
          drvs = await passClientStringSeq(session)
          buildMode = await passClientWord(session)
        stderr.writeLine "wopBuildPathsWithResults drvs ", $drvs
        await passWork(session)
        let count = await passDaemonWord(session)
        for _ in 1..count:
          let
            path = await passDaemonString(session)
            status = await passDaemonWord(session)
            errorMsg  = await passDaemonString(session)
            timesBUild = await passDaemonWord(session)
            isNonDeterministic = await passDaemonWord(session)
            startTime = await passDaemonWord(session)
            stopTime = await passDaemonWord(session)
            outputs =  await passDaemonStringMap(session)

      else:
        stderr.writeLine "unknown worker op ", wop.int
        break
  except ProtocolError as err:
    stderr.writeLine "connection terminated"
    stderr.writeLine "chunk bytes transfered: ", formatSize(chunksTotal)
  finally:
    close(session.daemon)
    close(session.client)

proc handshake(listener: AsyncSocket): Future[Session] {.async.} =
  ## Take the next connection from `listener` and return a `Session`.
  let session = Session(buffer: newSeq[Word](1024)) # 8KiB
  session.client = await listener.accept()
  session.daemon = newAsyncSocket(
      domain = AF_UNIX,
      sockType = SOCK_STREAM,
      protocol = cast[Protocol](0),
      buffered = false)
  await connectUnix(session.daemon, daemonSocketPath())
  let clientMagic = await passClientWord(session)
  if clientMagic != WORKER_MAGIC_1:
    raise newException(ProtocolError, "invalid protocol magic")
  let daemonMagic = await passDaemonWord(session)
  let daemonVersion = await passDaemonWord(session)
  session.version = Version(await passClientWord(session))
  if session.version < 0x1_0a:
    raise newException(ProtocolError, "obsolete protocol version")
  assert session.version.minor >= 14
  discard await(passClientWord(session))
    # obsolete CPU affinity
  assert session.version.minor >= 11
  discard await(passClientWord(session))
     # obsolete reserveSpace
  assert session.version.minor >= 33
  let daemonVersionString = await passDaemonString(session)
  assert daemonVersionString == $store.nixVersion
  await passWork(session)
  return session

proc emulateSocket*(path: string) {.async, gcsafe.} =
  let listener = newAsyncSocket(
      domain = AF_UNIX,
      sockType = SOCK_STREAM,
      protocol = cast[Protocol](0),
      buffered = false)
  bindUnix(listener, path)
  listen(listener)
  stderr.writeLine "listening on ", path
  while not listener.isClosed:
    try:
      let session = await handshake(listener)
      assert not session.isNil
      asyncCheck loop(session)
    except ProtocolError as err:
      stderr.writeLine "failed to service client, ", err.msg

when isMainModule:
  const path = "/tmp/worker.nix.socket"
  if fileExists(path): removeFile(path)
  try: waitFor emulateSocket(path)
  finally: removeFile(path)

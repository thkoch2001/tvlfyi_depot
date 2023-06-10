# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[algorithm, asyncdispatch, asyncnet, os, sets, strtabs, strutils]
from std/nativesockets import AF_INET, AF_UNIX, SOCK_STREAM, Protocol

import preserves, syndicate
from syndicate/protocols/dataspace import Observe
import ./protocol

{.pragma: workerProtocol, importc, header: "worker-protocol.hh".}

type Word = uint64
proc `$`(w: Word): string = toHex(w)

const
  WORKER_MAGIC_1 = 0x6E697863
  WORKER_MAGIC_2 = 0x6478696F
  PROTOCOL_VERSION = 0x100 or 35

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
  StringSeq = seq[string]
  StringSet = HashSet[string]
  Session = ref object
    socket: AsyncSocket
    buffer: seq[Word]
    version: Version
  Observe = dataspace.Observe[Ref]

func major(version: Version): uint16 = version and 0xff00
func minor(version: Version): uint16 = version and 0x00ff

proc close(session: Session) =
  close(session.socket)
  reset(session.buffer)

proc send(session: Session; words: varargs[Word]): Future[void] =
  if session.buffer.len < words.len:
    session.buffer.setLen(words.len)
  for i, word in words: session.buffer[i] = word
  send(session.socket, addr session.buffer[0], words.len shl 3)

proc send(session: Session; s: string): Future[void] =
  let wordCount = 1 + ((s.len + 7) shr 3)
  if session.buffer.len < wordCount: setLen(session.buffer, wordCount)
  session.buffer[0] = Word s.len
  if s != "":
    session.buffer[pred wordCount] = 0x00
    copyMem(addr session.buffer[1], unsafeAddr s[0], s.len)
  send(session.socket, addr session.buffer[0], wordCount shl 3)

proc send(session: Session; ss: StringSeq|StringSet): Future[void] =
  ## Send a set of strings. The set is sent as a contiguous buffer.
  session.buffer[0] = Word ss.len
  var off = 1
  for s in ss:
    let
      stringWordLen = (s.len + 7) shr 3
      bufferWordLen = off+1+stringWordLen
    if session.buffer.len < bufferWordLen:
      setLen(session.buffer, bufferWordLen)
    session.buffer[off] = Word s.len
    session.buffer[off+stringWordLen] = 0 # clear the aligning bits
    inc(off)
    copyMem(addr session.buffer[off], unsafeAddr s[0], s.len)
    inc(off, stringWordLen)
  send(session.socket, addr session.buffer[0], off shl 3)

proc recvWord(sock: AsyncSocket): Future[Word] {.async.} =
  var w: Word
  let n = await recvInto(sock, addr w, sizeof(Word))
  if n != sizeof(Word): raise newException(ProtocolError, "short read")
  return w

proc recvWord(session: Session): Future[Word] =
  recvWord(session.socket)

proc discardWords(session: Session; n: int): Future[void] {.async.} =
  if session.buffer.len < n: setLen(session.buffer, n)
  let byteCount = n shl 3
  let n = await recvInto(session.socket, addr session.buffer[0], byteCount)
  if n != byteCount:
      raise newException(ProtocolError, "short read")

proc recvString(socket: AsyncSocket): Future[string] {.async.} =
  let stringLen = int (await recvWord(socket))
  if stringLen > 0:
    var s = newString((stringLen + 7) and (not 7))
    let n = await recvInto(socket, addr s[0], s.len)
    if n != s.len:
      raise newException(ProtocolError, "short read")
    setLen(s, stringLen)
    return s
  return ""

proc recvString(session: Session): Future[string] =
  recvString(session.socket)

proc recvStringSeq(session: Session): Future[StringSeq] {.async.} =
  let count = int(await recvWord(session.socket))
  var strings = newSeq[string](count)
  for i in 0..<count: strings[i] = await recvString(session)
  return strings

proc recvStringSet(session: Session): Future[StringSet] {.async.} =
  let count = int(await recvWord(session.socket))
  var strings = initHashSet[string](count)
  for i in 0..<count: incl(strings, await recvString(session))
  return strings

proc recvError(session: Session) {.async.} =
  discard #[typ]# await recvString(session)
  discard #[lvl]# await recvWord(session)
  discard #[name]# await recvString(session)
  discard #[msg]# await recvString(session)
  discard #[havePos]# await recvWord(session)
  let nrTraces = await recvWord(session)
  for i in 1..nrTraces:
    discard #[havPos]# await recvWord(session)
    discard #[msg]# await recvString(session)

proc recvFields(session: Session) {.async.} =
  let count = await recvWord(session)
  for i in 0..<count:
    let typ = await recvWord(session)
    case typ
    of 0: discard await recvWord(session)
    of 1: discard await  recvString(session)
    else: raiseAssert "unknown field type " & $typ

proc recvWork(session: Session) {.async.} =
  while true:
    let word = await recvWord(session)
    case word
    of STDERR_WRITE:
      discard await recvString(session)
    of STDERR_READ:
      await send(session, "")
    of STDERR_ERROR:
      await recvError(session)
    of STDERR_NEXT:
      discard await recvString(session)
    of STDERR_START_ACTIVITY:
      discard await recvWord(session) # id
      discard await recvWord(session) # level
      discard await recvWord(session) # type
      discard await recvString(session) # text
      await recvFields(session) # fields
      discard await recvWord(session) # parent
    of STDERR_STOP_ACTIVITY:
      discard await recvWord(session) # id
    of STDERR_RESULT:
      var act: ActionResult
      discard await recvWord(session) # id
      discard await recvWord(session) # type
      await recvFields(session) # fields
    of STDERR_LAST:
      break
    else:
      raise newException(ProtocolError, "unknown work verb " & $word)

proc daemonSocketPath: string =
  getEnv(
      "NIX_DAEMON_SOCKET_PATH",
      "/nix/var/nix/daemon-socket/socket")

proc newSession(socket: AsyncSocket): Session =
  Session(socket: socket, buffer: newSeq[Word](512))

proc newSession(): Session =
  newSession(newAsyncSocket(
    domain = AF_UNIX,
    sockType = SOCK_STREAM,
    protocol = cast[Protocol](0),
    buffered = false))

proc send(session: Session; miss: Missing) {.async.} =
  await send(session, STDERR_LAST)
  await send(session, miss.willBuild)
  await send(session, miss.willSubstitute)
  await send(session, miss.unknown)
  await send(session, Word miss.downloadSize)
  await send(session, Word miss.narSize)

proc send(session: Session; info: PathInfo) {.async.} =
  await send(session, STDERR_LAST)
  await send(session, 1)
  if info.path != "":
    await send(session, info.path)
  await send(session, info.deriver)
  await send(session, info.narHash)
  await send(session, info.references)
  await send(session, Word info.registrationTime)
  await send(session, Word info.narSize)
  await send(session, Word info.ultimate)
  await send(session, info.sigs)
  await send(session, info.ca)

proc serveClient(facet: Facet; ds: Ref; session: Session) {.async.} =
  block:
    let clientMagic = await recvWord(session)
    if clientMagic != WORKER_MAGIC_1:
      raise newException(ProtocolError, "invalid protocol magic")
    await send(session, WORKER_MAGIC_2, PROTOCOL_VERSION)
    let clientVersion = Version(await recvWord(session))
    if clientVersion < 0x1_21:
      raise newException(ProtocolError, "obsolete protocol version")
    assert clientVersion.minor >= 14
    discard await(recvWord(session))
      # obsolete CPU affinity
    assert clientVersion.minor >= 11
    discard await(recvWord(session))
       # obsolete reserveSpace
    assert clientVersion.minor >= 33
    await send(session, "0.0.0")
    await send(session, STDERR_LAST)
  while not session.socket.isClosed:
    let wop = await recvWord(session.socket)
    case wop

    of wopQueryPathInfo:
      let
        path = await recvString(session)
        pat = inject(?PathInfo, { 0: ?path })
      await send(session, STDERR_NEXT)
      await send(session, $pat)
      run(facet) do (turn: var Turn):
        onPublish(turn, ds, pat) do (
              deriver: string,
              narHash: string,
              references: StringSet,
              registrationTime: BiggestInt,
              narSize: BiggestInt,
              ultimate: bool,
              sigs: StringSet,
              ca: string
            ):
          var info = PathInfo(
            deriver: deriver,
            narHash: narHash,
            references: references,
            registrationTime: registrationTime,
            narSize: narSize,
            ultimate: ultimate,
            sigs: sigs,
            ca: ca,
          )
          asyncCheck(turn, send(session, info))

    of wopQueryMissing:
      var targets = toPreserve(await recvStringSeq(session))
      sort(targets.sequence)
        # would prefer to use a set but that doesn't translate into a pattern
      let pat = inject(?Missing, { 0: ?targets })
      # TODO send the pattern to the client as a log line
      await send(session, STDERR_NEXT)
      await send(session, $pat)
      run(facet) do (turn: var Turn):
        onPublish(turn, ds, pat) do (
              willBuild: StringSet,
              willSubstitute: StringSet,
              unknown: StringSet,
              downloadSize: BiggestInt,
              narSize: BiggestInt
            ):
          let miss = Missing(
              willBuild: willBuild,
              willSubstitute: willSubstitute,
              unknown: unknown,
              downloadSize: downloadSize,
              narSize: narSize,
            )
          asyncCheck(turn, send(session, miss))

    of wopSetOptions:
      await discardWords(session, 12)
        # 01 keepFailed
        # 02 keepGoing
        # 03 tryFallback
        # 04 verbosity
        # 05 maxBuildJobs
        # 06 maxSilentTime
        # 07 useBuildHook
        # 08 verboseBuild
        # 09 logType
        # 10 printBuildTrace
        # 11 buildCores
        # 12 useSubstitutes
      let overridePairCount = await recvWord(session)
      for _ in 1..overridePairCount:
        discard await (recvString(session))
        discard await (recvString(session))
      await send(session, STDERR_LAST)
      # all options from the client are ingored

    else:
      let msg = "unhandled worker op " & $wop.int
      await send(session, STDERR_NEXT)
      await send(session, msg)
      await send(session, STDERR_LAST)
      close(session.socket)

proc serveClientSide*(facet: Facet; ds: Ref; listener: AsyncSocket) {.async.} =
  while not listener.isClosed:
    let
      client = await accept(listener)
      fut = serveClient(facet, ds, newSession(client))
    addCallback(fut) do ():
      if not client.isClosed:
        close(client)

proc bootClientSide*(facet: Facet; ds: Ref; socketPath: string) =
  let listener = newAsyncSocket(
      domain = AF_UNIX,
      sockType = SOCK_STREAM,
      protocol = cast[Protocol](0),
      buffered = false)
  onStop(facet) do (turn: var Turn):
    close(listener)
    removeFile(socketPath)
  removeFile(socketPath)
  bindUnix(listener, socketPath)
  listen(listener)
  asyncCheck(facet, serveClientSide(facet, ds, listener))

proc connectDaemon(session: Session; socketPath: string) {.async.} =
  await connectUnix(session.socket, socketPath)
  await send(session, WORKER_MAGIC_1)
  let daemonMagic = await recvWord(session)
  if daemonMagic != WORKER_MAGIC_2:
    raise newException(ProtocolError, "bad magic from daemon")
  let daemonVersion = await recvWord(session)
  session.version = min(Version daemonVersion, PROTOCOL_VERSION)
  await send(session, Word session.version)
  await send(session, 0) # CPU affinity
  await send(session, 0) # reserve space
  if session.version.minor >= 33:
    discard await recvString(session) # version
  if session.version.minor >= 35:
    discard await recvWord(session) # remoteTrustsUs
  await recvWork(session)

proc queryMissing(session: Session; targets: StringSeq): Future[Missing] {.async.} =
  var miss = Missing(targets: targets)
  await send(session, wopQueryMissing)
  await send(session, miss.targets)
  await recvWork(session)
  miss.willBuild = await recvStringSet(session)
  miss.willSubstitute = await recvStringSet(session)
  miss.unknown = await recvStringSet(session)
  miss.downloadSize = BiggestInt await recvWord(session)
  miss.narSize = BiggestInt await recvWord(session)
  return miss

proc queryPathInfo(session: Session; path: string): Future[PathInfo] {.async.} =
  var info = PathInfo(path: path)
  await send(session, wopQueryPathInfo)
  await send(session, info.path)
  await recvWork(session)
  let valid = await recvWord(session)
  if valid != 0:
    info.deriver = await recvString(session)
    info.narHash = await recvString(session)
    info.references = await recvStringSet(session)
    info.registrationTime = BiggestInt await recvWord(session)
    info.narSize = BiggestInt await recvWord(session)
    info.ultimate = (await recvWord(session)) != 0
    info.sigs = await recvStringSet(session)
    info.ca = await recvString(session)
  return info

proc bootDaemonSide*(turn: var Turn; ds: Ref; socketPath: string) =

  during(turn, ds, ?Observe(pattern: !Missing) ?? {0: grab()}) do (a: Preserve[Ref]):
      # cannot use `grabLit` here because an array is a compound
    let
      session = newSession()
      fut = connectDaemon(session, socketPath)
    addCallback(fut, turn) do (turn: var Turn):
      read(fut)
      var targets: StringSeq
      doAssert targets.fromPreserve(unpackLiterals(a))
        # unpack <arr [<lit " …">]>
      let missFut = queryMissing(session, targets)
      addCallback(missFut, turn) do (turn: var Turn):
        var miss = read(missFut)
        discard publish(turn, ds, miss)
  do:
    close(session)

  during(turn, ds, ?Observe(pattern: !PathInfo) ?? {0: grabLit()}) do (path: string):
    let
      session = newSession()
      fut = connectDaemon(session, socketPath)
    addCallback(fut, turn) do (turn: var Turn):
      read(fut)
      let infoFut = queryPathInfo(session, path)
      addCallback(infoFut, turn) do (turn: var Turn):
        var info = read(infoFut)
        discard publish(turn, ds, info)
  do:
    close(session)

type
 Snoop = ref object
    client, daemon: AsyncSocket
    buffer: seq[Word]
    version: Version

type ValidPathInfo = object
  path: string
  deriver: string
  narHash: string
  references: StringSet
  registrationTime, narSize: BiggestInt
  ultimate: bool
  sigs: StringSet
  ca: string

proc send(session: Snoop; sock: AsyncSocket; words: varargs[Word]): Future[void] =
  for i, word in words: session.buffer[i] = word
  send(sock, addr session.buffer[0], words.len shl 3)

proc send(session: Snoop; sock: AsyncSocket; s: string): Future[void] =
  let wordCount = (s.len + 7) shr 3
  if wordCount > session.buffer.len: setLen(session.buffer, wordCount)
  session.buffer[0] = Word s.len
  if wordCount > 0:
    session.buffer[wordCount] = 0x00
    copyMem(addr session.buffer[1], unsafeAddr s[0], s.len)
  send(sock, addr session.buffer[0], (succ wordCount) shl 3)

proc passWord(a, b: AsyncSocket): Future[Word] {.async.} =
  var w = await recvWord(a)
  await send(b, addr w, sizeof(Word))
  return w

proc passString(session: Snoop; a, b: AsyncSocket): Future[string] {.async.} =
  var s = await recvString(a)
  await send(session, b, s)
  return s

proc passStringSeq(session: Snoop; a, b: AsyncSocket): Future[seq[string]] {.async.} =
  let count = int(await passWord(a, b))
  var strings = newSeq[string](count)
  for i in 0..<count: strings[i] = await passString(session, a, b)
  return strings

proc passStringSet(session: Snoop; a, b: AsyncSocket): Future[StringSet] {.async.} =
  let count = int(await passWord(a, b))
  var strings = initHashSet[string](count)
  for i in 0..<count: incl(strings, await passString(session, a, b))
  return strings

proc passStringMap(session: Snoop; a, b: AsyncSocket): Future[StringTableCap] {.async.} =
  var table = newStringTable(modeCaseSensitive)
  let n = await passWord(a, b)
  for i in 1..n:
    var
      key = await passString(session, a, b)
      val = await passString(session, a, b)
    table[key] = val
  return table

proc passClientWord(session: Snoop): Future[Word] =
  passWord(session.client, session.daemon)

proc passDaemonWord(session: Snoop): Future[Word] =
  passWord(session.daemon, session.client)

proc passClientString(session: Snoop): Future[string] =
  passString(session, session.client, session.daemon)

proc passDaemonString(session: Snoop): Future[string] =
  passString(session, session.daemon, session.client)

proc passClientStringSeq(session: Snoop): Future[seq[string]] =
  passStringSeq(session, session.client, session.daemon)

proc passDaemonStringSeq(session: Snoop): Future[seq[string]] =
  passStringSeq(session, session.daemon, session.client)

proc passClientStringSet(session: Snoop): Future[StringSet] =
  passStringSet(session, session.client, session.daemon)

proc passDaemonStringSet(session: Snoop): Future[StringSet] =
  passStringSet(session, session.daemon, session.client)

proc passClientStringMap(session: Snoop): Future[StringTableCap] =
  passStringMap(session, session.client, session.daemon)

proc passDaemonStringMap(session: Snoop): Future[StringTableCap] =
  passStringMap(session, session.daemon, session.client)

proc passDaemonValidPathInfo(session: Snoop; includePath: bool): Future[PathInfo] {.async.} =
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

proc passChunks(session: Snoop; a, b: AsyncSocket): Future[int] {.async.} =
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

proc passClientChunks(session: Snoop): Future[int] =
  passChunks(session, session.client, session.daemon)

proc passErrorDaemonError(session: Snoop) {.async.} =
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

proc passDaemonFields(session: Snoop): Future[Fields] {.async.} =
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

proc passWork(session: Snoop) {.async.} =
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

proc loop(session: Snoop) {.async.} =
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
        miss.targets = await passClientStringSeq(session)
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

proc handshake(listener: AsyncSocket): Future[Snoop] {.async.} =
  ## Take the next connection from `listener` and return a `Session`.
  let session = Snoop(buffer: newSeq[Word](1024)) # 8KiB
  session.client = await listener.accept()
  session.daemon = newAsyncSocket(
      domain = AF_UNIX,
      sockType = SOCK_STREAM,
      protocol = cast[Protocol](0),
      buffered = false,
    )
  await connectUnix(session.daemon, daemonSocketPath())
  let clientMagic = await passClientWord(session)
  if clientMagic != WORKER_MAGIC_1:
    raise newException(ProtocolError, "invalid protocol magic")
  let daemonMagic = await passDaemonWord(session)
  let daemonVersion = await passDaemonWord(session)
  session.version = Version(await passClientWord(session))
  if session.version < PROTOCOL_VERSION:
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
    except ProtocolError:
      close(session)
    finally:
      close(session)

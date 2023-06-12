# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[asyncdispatch, asyncnet, sets, streams, strutils]
from std/algorithm import sort

import eris
import preserves, syndicate
from syndicate/protocols/dataspace import Observe
import ./protocol, ./sockets

type Value = Preserve[void]

proc merge(items: varargs[Value]): Value =
  # TODO: just a hack, not a proper imlementation
  # https://preserves.dev/preserves.html#appendix-merging-values
  result = initDictionary()
  for e in items:
    for (key, val) in e.pairs:
      result[key] = val
  cannonicalize(result)

type Observe = dataspace.Observe[Ref]

proc recvError(daemon: Session): Future[string] {.async.} =
  discard #[typ]# await recvString(daemon)
  discard #[lvl]# await recvWord(daemon)
  discard #[name]# await recvString(daemon)
  let msg = #[msg]# await recvString(daemon)
  discard #[havePos]# await recvWord(daemon)
  let nrTraces = await recvWord(daemon)
  for i in 1..nrTraces:
    discard #[havPos]# await recvWord(daemon)
    discard #[msg]# await recvString(daemon)
  return msg

proc recvFields(daemon: Session) {.async.} =
  let count = await recvWord(daemon)
  for i in 0..<count:
    let typ = await recvWord(daemon)
    case typ
    of 0: discard await recvWord(daemon)
    of 1: discard await  recvString(daemon)
    else: raiseAssert "unknown field type " & $typ

proc recvWork(daemon: Session) {.async.} =
  while true:
    let word = await recvWord(daemon)
    case word
    of STDERR_WRITE:
      discard await recvString(daemon)
    of STDERR_READ:
      await send(daemon, "")
    of STDERR_ERROR:
      let err = await recvError(daemon)
      raise newException(ProtocolError, "Nix daemon: " & err)
    of STDERR_NEXT:
      let msg = await recvString(daemon)
      stderr.writeLine("Nix daemon: ", msg)
    of STDERR_START_ACTIVITY:
      discard await recvWord(daemon) # id
      discard await recvWord(daemon) # level
      discard await recvWord(daemon) # type
      discard await recvString(daemon) # text
      await recvFields(daemon) # fields
      discard await recvWord(daemon) # parent
    of STDERR_STOP_ACTIVITY:
      discard await recvWord(daemon) # id
    of STDERR_RESULT:
      discard await recvWord(daemon) # id
      discard await recvWord(daemon) # type
      await recvFields(daemon) # fields
    of STDERR_LAST:
      break
    else:
      raise newException(ProtocolError, "unknown work verb " & $word)

proc connectDaemon(daemon: Session; socketPath: string) {.async.} =
  await connectUnix(daemon.socket, socketPath)
  await send(daemon, WORKER_MAGIC_1)
  let daemonMagic = await recvWord(daemon)
  if daemonMagic != WORKER_MAGIC_2:
    raise newException(ProtocolError, "bad magic from daemon")
  let daemonVersion = await recvWord(daemon)
  daemon.version = min(Version daemonVersion, PROTOCOL_VERSION)
  await send(daemon, Word daemon.version)
  await send(daemon, 0) # CPU affinity
  await send(daemon, 0) # reserve space
  if daemon.version.minor >= 33:
    discard await recvString(daemon) # version
  if daemon.version.minor >= 35:
    discard await recvWord(daemon) # remoteTrustsUs
  await recvWork(daemon)

proc queryMissing(daemon: Session; targets: StringSeq): Future[Missing] {.async.} =
  var miss = Missing(targets: targets)
  await send(daemon, Word wopQueryMissing)
  await send(daemon, miss.targets)
  await recvWork(daemon)
  miss.willBuild = await recvStringSet(daemon)
  miss.willSubstitute = await recvStringSet(daemon)
  miss.unknown = await recvStringSet(daemon)
  miss.downloadSize = BiggestInt await recvWord(daemon)
  miss.narSize = BiggestInt await recvWord(daemon)
  return miss

proc queryPathInfo(daemon: Session; path: string): Future[LegacyPathAttrs] {.async.} =
  var info: LegacyPathAttrs
  await send(daemon, Word wopQueryPathInfo)
  await send(daemon, path)
  await recvWork(daemon)
  let valid = await recvWord(daemon)
  if valid != 0:
    info.deriver = await recvString(daemon)
    info.narHash = await recvString(daemon)
    info.references = await recvStringSeq(daemon)
    sort(info.references)
    info.registrationTime = BiggestInt await recvWord(daemon)
    info.narSize = BiggestInt await recvWord(daemon)
    info.ultimate = (await recvWord(daemon)) != 0
    info.sigs = await recvStringSet(daemon)
    info.ca = await recvString(daemon)
  return info

proc recvLegacyPathAttrs(daemon: Session): Future[AddToStoreAttrs] {.async.} =
  var info: AddToStoreAttrs
  info.deriver = await recvString(daemon)
  info.narHash = await recvString(daemon)
  info.references = await recvStringSeq(daemon)
  sort(info.references)
  info.registrationTime = BiggestInt await recvWord(daemon)
  info.narSize = BiggestInt await recvWord(daemon)
  assert daemon.version.minor >= 16
  info.ultimate = (await recvWord(daemon)) != 0
  info.sigs = await recvStringSet(daemon)
  info.ca = await recvString(daemon)
  return info

proc addToStore(daemon: Session; store: ErisStore; request: AddToStoreClientAttrs): Future[(string, AddToStoreAttrs)] {.async.} =
  let
    erisCap = parseCap(request.eris)
    stream = newErisStream(store, erisCap)
  await send(daemon, Word wopAddToStore)
  await send(daemon, request.name)
  await send(daemon, string request.`ca-method`)
  await send(daemon, request.references)
  await send(daemon, 0) # repair
  await recoverChunks(daemon, store, erisCap)
  await recvWork(daemon)
  let path = await recvString(daemon)
  var info = await recvLegacyPathAttrs(daemon)
  info.eris = request.eris
  info.`ca-method` = request.`ca-method`
  info.name = request.name
  info.references = request.references
  return (path, info)

proc callDaemon(turn: var Turn; path: string; action: proc (daemon: Session; turn: var Turn) {.gcsafe.}): Session =
  let
    daemon = newSession()
    fut = connectDaemon(daemon, path)
  addCallback(fut, turn) do (turn: var Turn):
    read(fut)
    action(daemon, turn)
  return daemon

proc bootDaemonSide*(turn: var Turn; ds: Ref; store: ErisStore; socketPath: string) =

  during(turn, ds, ?Observe(pattern: !Missing) ?? {0: grab()}) do (a: Preserve[Ref]):
      # cannot use `grabLit` here because an array is a compound
      # TODO: unpack to a `Pattern`
    let daemon = callDaemon(turn, socketPath) do (daemon: Session; turn: var Turn):
      var targets: StringSeq
      doAssert targets.fromPreserve(unpackLiterals(a))
        # unpack <arr [<lit " …">]>
      let missFut = queryMissing(daemon, targets)
      addCallback(missFut, turn) do (turn: var Turn):
        close(daemon)
        var miss = read(missFut)
        discard publish(turn, ds, miss)
  do:
    close(daemon)

  during(turn, ds, ?Observe(pattern: !PathInfo) ?? {0: grabLit()}) do (path: string):
    let daemon = callDaemon(turn, socketPath) do (daemon: Session; turn: var Turn):
      let infoFut = queryPathInfo(daemon, path)
      addCallback(infoFut, turn) do (turn: var Turn):
        close(daemon)
        var info = read(infoFut)
        discard publish(turn, ds, initRecord("path", path.toPreserve, info.toPreserve))
  do:
    close(daemon)

  during(turn, ds, ?Observe(pattern: !PathInfo) ?? {1: grabDict()}) do (pat: Value):
    var daemon: Session
    var request: AddToStoreClientAttrs
    if request.fromPreserve(unpackLiterals pat):
      daemon = callDaemon(turn, socketPath) do (daemon: Session; turn: var Turn):
        let fut = addToStore(daemon, store, request)
        addCallback(fut, turn) do (turn: var Turn):
          close(daemon)
          var (path, info) = read(fut)
          discard publish(turn, ds, initRecord("path", path.toPreserve, info.toPreserve))
  do:
    close(daemon)

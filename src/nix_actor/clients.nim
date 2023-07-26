# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

import std/[asyncdispatch, asyncnet, os, sets, strutils, tables]
from std/algorithm import sort

import eris
import preserves, syndicate
import ./protocol, ./sockets

proc sendNext(client: Session; msg: string) {.async.} =
  await send(client, STDERR_NEXT)
  await send(client, msg)

proc sendWorkEnd(client: Session): Future[void] =
  send(client, STDERR_LAST)

proc send(client: Session; miss: Missing) {.async.} =
  await sendWorkEnd(client)
  await send(client, miss.willBuild)
  await send(client, miss.willSubstitute)
  await send(client, miss.unknown)
  await send(client, Word miss.downloadSize)
  await send(client, Word miss.narSize)

proc send(client: Session; info: LegacyPathAttrs) {.async.} =
  await send(client, info.deriver)
  await send(client, info.narHash)
  await send(client, info.references)
  await send(client, Word info.registrationTime)
  await send(client, Word info.narSize)
  await send(client, Word info.ultimate)
  await send(client, info.sigs)
  await send(client, info.ca)

proc sendValidInfo(client: Session; info: LegacyPathAttrs) {.async.} =
  await sendWorkEnd(client)
  await send(client, 1) # valid
  await send(client, info)

proc completeAddToStore(client: Session; path: string; info: LegacyPathAttrs) {.async.} =
  await sendWorkEnd(client)
  await send(client, path)
  await send(client, info)

proc serveClient(facet: Facet; ds: Cap; store: ErisStore; client: Session) {.async.} =
  block:
    let clientMagic = await recvWord(client)
    if clientMagic != WORKER_MAGIC_1:
      raise newException(ProtocolError, "invalid protocol magic")
    await send(client, WORKER_MAGIC_2, PROTOCOL_VERSION)
    let clientVersion = Version(await recvWord(client))
    if clientVersion < 0x1_21:
      raise newException(ProtocolError, "obsolete protocol version")
    assert clientVersion.minor >= 14
    discard await(recvWord(client))
      # obsolete CPU affinity
    assert clientVersion.minor >= 11
    discard await(recvWord(client))
       # obsolete reserveSpace
    assert clientVersion.minor >= 33
    await send(client, "0.0.0")
    await sendWorkEnd(client)
  while not client.socket.isClosed:
    let
      w = await recvWord(client.socket)
      wop = WorkerOperation(w)
    case wop

    of wopAddToStore:
      let
        name = await recvString(client)
        caMethod = await recvString(client)
      var storeRefs = await recvStringSeq(client)
      sort(storeRefs) # sets not valid for patterns so use a sorted list
      discard await recvWord(client) # repair, not implemented
      let cap = await ingestChunks(client, store)
      await sendNext(client, $cap & " " & name)
      let attrsPat = inject(?AddToStoreAttrs, {
          "name".toSymbol(Cap): ?name,
          "ca-method".toSymbol(Cap): ?caMethod.toSymbol,
          "references".toSymbol(Cap): ?storeRefs,
          "eris".toSymbol(Cap): ?cap.bytes,
        })
        # bind AddToStoreAttrs and override with some literal values
      let pat = PathInfo ? { 0: grab(), 1: attrsPat }
      run(facet) do (turn: var Turn):
        onPublish(turn, ds, pat) do (path: string, ca: string, deriver: string, narHash: string, narSize: BiggestInt, regTime: BiggestInt, sigs: StringSet, ultimate: bool):
          asyncCheck(turn, completeAddToStore(client, path, LegacyPathAttrs(
              ca: ca,
              deriver: deriver,
              narHash: narHash,
              narSize: narSize,
              references: storeRefs,
              registrationTime: regTime,
              sigs: sigs,
              ultimate: ultimate,
            )))

    of wopQueryPathInfo:
      let
        path = await recvString(client)
        pat = PathInfo ? { 0: ?path, 1: grab() }
      run(facet) do (turn: var Turn):
        onPublish(turn, ds, pat) do (info: LegacyPathAttrs):
          asyncCheck(turn, sendValidInfo(client, info))

    of wopQueryMissing:
      var targets = toPreserve(await recvStringSeq(client))
      sort(targets.sequence)
        # would prefer to use a set but that doesn't translate into a pattern
      let pat = inject(?Missing, { 0: ?targets })
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
          asyncCheck(turn, send(client, miss))

    of wopSetOptions:
      await discardWords(client, 12)
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
      let overridePairCount = await recvWord(client)
      for _ in 1..overridePairCount:
        discard await (recvString(client))
        discard await (recvString(client))
      await sendWorkEnd(client)
      # all options from the client are ingored

    else:
      let msg = "unhandled worker op " & $wop
      await sendNext(client, msg)
      await sendWorkEnd(client)
      close(client.socket)

proc serveClientSide(facet: Facet; ds: Cap; store: ErisStore; listener: AsyncSocket) {.async.} =
  while not listener.isClosed:
    let
      client = await accept(listener)
      fut = serveClient(facet, ds, store, newSession(client))
    addCallback(fut) do ():
      if not client.isClosed:
        close(client)

proc bootClientSide*(turn: var Turn; ds: Cap; store: ErisStore; socketPath: string) =
  let listener = newUnixSocket()
  onStop(turn.facet) do (turn: var Turn):
    close(listener)
    removeFile(socketPath)
  removeFile(socketPath)
  bindUnix(listener, socketPath)
  listen(listener)
  asyncCheck(turn, serveClientSide(turn.facet, ds, store, listener))

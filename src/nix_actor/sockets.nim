# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

## Common module for communicating with Nix clients and daemons.

import std/[asyncdispatch, asyncnet, sets, strtabs, strutils, tables]
from std/nativesockets import AF_UNIX, SOCK_STREAM, Protocol

import eris
import preserves, syndicate

import ./protocol

{.pragma: workerProtocol, importc, header: "worker-protocol.hh".}

type Word* = uint64

proc `[]=`*[T](attrs: var AttrSet; key: string; val: T) =
  attrs[Symbol key] = val.toPreserve

const
  WORKER_MAGIC_1* = 0x6E697863
  WORKER_MAGIC_2* = 0x6478696F
  PROTOCOL_VERSION* = 0x100 or 35

  STDERR_NEXT* = 0x6F6C6d67
  STDERR_READ* = 0x64617461
  STDERR_WRITE* = 0x64617416
  STDERR_LAST* = 0x616C7473
  STDERR_ERROR* = 0x63787470
  STDERR_START_ACTIVITY* = 0x53545254
  STDERR_STOP_ACTIVITY* = 0x53544F50
  STDERR_RESULT* = 0x52534C54

type WorkerOperation* = enum
  wopIsValidPath = 1,
  wopHasSubstitutes = 3,
  wopQueryPathHash = 4, # obsolete
  wopQueryReferences = 5, # obsolete
  wopQueryReferrers = 6,
  wopAddToStore = 7,
  wopAddTextToStore = 8, # obsolete since 1.25, Nix 3.0. Use wopAddToStore
  wopBuildPaths = 9,
  wopEnsurePath = 10,
  wopAddTempRoot = 11,
  wopAddIndirectRoot = 12,
  wopSyncWithGC = 13,
  wopFindRoots = 14,
  wopExportPath = 16, # obsolete
  wopQueryDeriver = 18, # obsolete
  wopSetOptions = 19,
  wopCollectGarbage = 20,
  wopQuerySubstitutablePathInfo = 21,
  wopQueryDerivationOutputs = 22, # obsolete
  wopQueryAllValidPaths = 23,
  wopQueryFailedPaths = 24,
  wopClearFailedPaths = 25,
  wopQueryPathInfo = 26,
  wopImportPaths = 27, # obsolete
  wopQueryDerivationOutputNames = 28, # obsolete
  wopQueryPathFromHashPart = 29,
  wopQuerySubstitutablePathInfos = 30,
  wopQueryValidPaths = 31,
  wopQuerySubstitutablePaths = 32,
  wopQueryValidDerivers = 33,
  wopOptimiseStore = 34,
  wopVerifyStore = 35,
  wopBuildDerivation = 36,
  wopAddSignatures = 37,
  wopNarFromPath = 38,
  wopAddToStoreNar = 39,
  wopQueryMissing = 40,
  wopQueryDerivationOutputMap = 41,
  wopRegisterDrvOutput = 42,
  wopQueryRealisation = 43,
  wopAddMultipleToStore = 44,
  wopAddBuildLog = 45,
  wopBuildPathsWithResults = 46,

type
  ProtocolError* = object of IOError
  Version* = uint16

  Session* = ref object
    socket*: AsyncSocket
    buffer*: seq[Word]
    version*: Version

func major*(version: Version): uint16 = version and 0xff00
func minor*(version: Version): uint16 = version and 0x00ff

proc close*(session: Session) =
  close(session.socket)
  reset(session.buffer)

proc send*(session: Session; words: varargs[Word]): Future[void] =
  if session.buffer.len < words.len:
    session.buffer.setLen(words.len)
  for i, word in words: session.buffer[i] = word
  send(session.socket, addr session.buffer[0], words.len shl 3)

proc send*(session: Session; s: string): Future[void] =
  let wordCount = 1 + ((s.len + 7) shr 3)
  if session.buffer.len < wordCount: setLen(session.buffer, wordCount)
  session.buffer[0] = Word s.len
  if s != "":
    session.buffer[pred wordCount] = 0x00
    copyMem(addr session.buffer[1], unsafeAddr s[0], s.len)
  send(session.socket, addr session.buffer[0], wordCount shl 3)

proc send*(session: Session; ss: StringSeq|StringSet): Future[void] =
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

proc recvWord*(sock: AsyncSocket): Future[Word] {.async.} =
  var w: Word
  let n = await recvInto(sock, addr w, sizeof(Word))
  if n != sizeof(Word): raise newException(ProtocolError, "short read")
  return w

proc recvWord*(session: Session): Future[Word] =
  recvWord(session.socket)

proc discardWords*(session: Session; n: int): Future[void] {.async.} =
  if session.buffer.len < n: setLen(session.buffer, n)
  let byteCount = n shl 3
  let n = await recvInto(session.socket, addr session.buffer[0], byteCount)
  if n != byteCount:
      raise newException(ProtocolError, "short read")

proc recvString*(socket: AsyncSocket): Future[string] {.async.} =
  let stringLen = int (await recvWord(socket))
  if stringLen > 0:
    var s = newString((stringLen + 7) and (not 7))
    let n = await recvInto(socket, addr s[0], s.len)
    if n != s.len:
      raise newException(ProtocolError, "short read")
    setLen(s, stringLen)
    return s
  return ""

proc recvString*(session: Session): Future[string] =
  recvString(session.socket)

proc recvStringSeq*(session: Session): Future[StringSeq] {.async.} =
  let count = int(await recvWord(session.socket))
  var strings = newSeq[string](count)
  for i in 0..<count: strings[i] = await recvString(session)
  return strings

proc recvStringSet*(session: Session): Future[StringSet] {.async.} =
  let count = int(await recvWord(session.socket))
  var strings = initHashSet[string](count)
  for i in 0..<count: incl(strings, await recvString(session))
  return strings

proc newUnixSocket*(): AsyncSocket =
  newAsyncSocket(
      domain = AF_UNIX,
      sockType = SOCK_STREAM,
      protocol = cast[Protocol](0),
      buffered = false,
    )

proc newSession*(socket: AsyncSocket): Session =
  Session(socket: socket, buffer: newSeq[Word](512))

proc newSession*(): Session =
  newUnixSocket().newSession()

proc ingestChunks*(session: Session; store: ErisStore): Future[ErisCap] {.async.} =
  var ingest: ErisIngest
  while true:
    let chunkLen = int await recvWord(session)
    if ingest.isNil:
      ingest = newErisIngest(
        store, recommendedChunkSize(chunkLen), convergentMode)
    if chunkLen == 0:
      break
    else:
      let wordLen = (chunkLen + 7) shr 3
      if session.buffer.len < wordLen: setLen(session.buffer, wordLen)
      let recvLen = await recvInto(session.socket, addr session.buffer[0], chunkLen)
        # each chunk must be received contiguously
      if recvLen != chunkLen:
        raise newException(ProtocolError, "invalid chunk read")
      await append(ingest, addr session.buffer[0], chunkLen)
  var cap = await cap(ingest)
  return cap

proc recoverChunks*(session: Session; store: ErisStore; cap: ErisCap) {.async.} =
  let stream = newErisStream(store, cap)
  session.buffer.setLen(succ(cap.chunkSize.int shr 3))
  while true:
    let n = await stream.readBuffer(addr session.buffer[1], cap.chunkSize.int)
    session.buffer[0] = Word n
    await send(session.socket, addr session.buffer[0], 8+n)
    if n == 0: break
  close(stream)

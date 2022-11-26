# //tvix/store

This contains the code hosting the tvix-store.

For the local store, Nix realizes files on the filesystem in `/nix/store` (and
maintains some metadata in a SQLite database). For "remote stores", it
communicates this metadata in NAR (Nix ARchive) and NARInfo format.

Compared to the Nix model, `tvix-store` stores data on a much more granular
level than that, which provides more deduplication possibilities, and more
granular copying.

However, enough information is preserved to still render NAR and NARInfo where
needed.

`//tvix/nar-bridge` exposes a HTTP Binary cache interface (GET/HEAD/PUT
requests) that can be used to configure a `tvix-store` as a substitutor for
Nix, or to upload store paths from Nix via `nix copy`, bridging the two worlds.

To actually use this in build contexts, we obviously still need to implement a
way to provide a `/nix/store` filesystem - this could either be done by something
traversing a `tvix-store` and "extracting" to the real filesystem, or by
providing a FUSE filesytem.

## Components

`tvix-store` can be split into three services:

### BlobService
`BlobService` takes care of storing blobs of data, used to host regular file
contents.

It is content-addressed, using [blake3](https://github.com/BLAKE3-team/BLAKE3)
as a hashing function.

Due to its content-addressed nature, we only store files once. Uploading the
same file again is a no-op.

We currently don't go any further than that, but as blake3 is a tree hash,
there's an opportunity to do
[verified streaming](https://github.com/oconnor663/bao) of parts of the file,
which doesn't need to trust any more information than the root hash itself.

Future extensions of the `BlobService` protocol will enable this.

### DirectoryService
`DirectoryService` allows lookups (and uploads) of `Directory` messages, and
whole reference graphs of them.

These are content-addressed, too, but we need to get into more detail how this
is all connected:

#### Store Model

#### Directory message
`Directory` messages use the blake3 hash of their canonical protobuf
serialization as its identifier.

A `Directory` message contains three lists, `directories`, `files` and
`symlinks`, holding `DirectoryNode`, `FileNode` and `SymlinkNode` messages
respectively. They describe all the direct child elements that are contained in
a directory.

All three message types have a `name` field, specifying the (base)name of the
element, and for reproducibility reasons, the lists MUST be sorted by that
name.

In addition to the `name` field, the various *Node messages have the following
fields:

#### DirectoryNode
A `DirectoryNode` message represents a child directory.

It has a `digest` field, which points to the identifier of another `Directory`
message, making a `Directory` a merkle tree (or strictly speaking, a graph, as
two elements pointing to a child directory with the same contents would point
to the same `Directory` message.

There's also a `size` field, containing the (total) number of all child
elements in the referenced `Directory`, which helps for inode calculation.

#### FileNode
A `FileNode` message represents a child (regular) file.

Its `digest` field contains the blake3 hash of the file contents. It can be
looked up in the `BlobService`.

The `size` field contains the size of the blob the `digest` field refers to.

The `executable` field specifies whether the file should be marked as
executable or not.

#### SymlinkNode
A `SymlinkNode` message represents a child symlink.

In addition to the `name` field, the only additional field is the `target`,
which is a string containing the target of the symlink.

### PathInfoService
The PathInfo service provides lookups from an output path hash to a `PathInfo` message.

#### PathInfo message
A `PathInfo` message contains a root node `{Directory,File,Symlink}Node`, as a
Nix Path can be one of these three types.

The root nodes' `name` field is populated with the (base)name inside
`/nix/store`, so `xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-pname-1.2.3`.

The `PathInfo` message also stores references to other store paths, and some
more NARInfo-specific metadata (signatures, narhash, narsize).

## Trust model / Distribution
As already described above, the only non-content-addressed service is the
`PathInfo` service.

This means, all other messages (such as `Blob` and `Directory` messages) can be
substituted from other sources/mirrors, which will make plugging in additional
substitution strategies like IPFS, local network neighbors super simple.

As of now, we don't specify an additional signature mechanism yet, as the only
"real" client so far is Nix, which gets streamed the whole NAR file (and it can
use the NARInfo-based signatures for verification).

A future signature mechanism, that is only signing (parts of) the `PathInfo`
message, which only points to content-addressed data will enable verified
partial access into a store path, opening up opportunities for lazy filesystem
access, which is very useful in remote builder scenarios.

## More Information
Check the `protos/` subfolder for the definition of the exact RPC methods and
messages.


## Interacting with the GRPC service manually
The shell environment in `//tvix` provides `evans`, which is an interactive
REPL-based gPRC client.

You can use it to connect to a `tvix-store` and call the various RPC methods.

```shell
$ cargo run &
$ evans --host localhost --port 8000 -r repl
  ______
 |  ____|
 | |__    __   __   __ _   _ __    ___
 |  __|   \ \ / /  / _. | | '_ \  / __|
 | |____   \ V /  | (_| | | | | | \__ \
 |______|   \_/    \__,_| |_| |_| |___/

 more expressive universal gRPC client


tvix.store.v1@localhost:8000> service BlobService

tvix.store.v1.BlobService@localhost:8000> call Put --bytes-from-file
data (TYPE_BYTES) => /run/current-system/system
{
  "digest": "KOM3/IHEx7YfInAnlJpAElYezq0Sxn9fRz7xuClwNfA="
}

tvix.store.v1.BlobService@localhost:8000> call Get --bytes-as-base64
digest (TYPE_BYTES) => KOM3/IHEx7YfInAnlJpAElYezq0Sxn9fRz7xuClwNfA=
{
  "data": "eDg2XzY0LWxpbnV4"
}

$ echo eDg2XzY0LWxpbnV4 | base64 -d
x86_64-linux
```

Thanks to `tvix-store` providing gRPC Server Reflection (with `reflection`
feature), you don't need to point `evans` to the `.proto` files.

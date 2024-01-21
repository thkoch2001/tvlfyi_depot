package importer

import (
	"bytes"
	"context"
	"crypto/sha256"
	"errors"
	"fmt"
	"io"
	"path"
	"strings"

	castorev1pb "code.tvl.fyi/tvix/castore-go"
	"github.com/nix-community/go-nix/pkg/nar"
	"golang.org/x/sync/semaphore"
)

const (
	// asyncUploadThreshold controls when a file is buffered into memory and uploaded
	// asynchronously. Files must be smaller than the threshold to be uploaded asynchronously.
	asyncUploadThreshold = 1024 * 1024 // 1 MiB

	// maxAsyncUploadBuffer is the maximum amount of memory allowed to be used at once to
	// perform async blob uploads. This _must_ be larger than asyncUploadThreshold.
	maxAsyncUploadBuffer = 10 * 1024 * 1024 // 10 MiB
)

var (
	// A weighted semaphore is used to ensure that we don't use too much memory at once
	// due to too many concurrent async blob uploads.
	asyncUploadSem = semaphore.NewWeighted(maxAsyncUploadBuffer)
)

// An item on the directories stack, contains everything in the directory seen
// so far.
type stackItem struct {
	path        string
	directories []*op[*castorev1pb.DirectoryNode]
	symlinks    []*castorev1pb.SymlinkNode
	files       []*op[*castorev1pb.FileNode]
}

// toDirectory converts the stack item into a directory proto. This should only
// be called once the directory is complete. This will wait for any pending
// file or directory uploads of this dirctory's children.
func (i *stackItem) toDirectory() (*castorev1pb.Directory, error) {
	directory := &castorev1pb.Directory{
		Symlinks: i.symlinks,
	}

	// Collect all pending directory nodes.
	for _, directoryNodeOp := range i.directories {
		directoryNode, err := directoryNodeOp.get()
		if err != nil {
			return nil, err
		}
		directory.Directories = append(directory.Directories, directoryNode)
	}

	// Collect all pending file nodes.
	for _, fileNodeOp := range i.files {
		fileNode, err := fileNodeOp.get()
		if err != nil {
			return nil, err
		}
		directory.Files = append(directory.Files, fileNode)
	}

	return directory, nil
}

// op represents an operation which may or may not be completed yet.
type op[T any] struct {
	doneChan chan struct{}
	err      error
	value    T
}

// doAsync calls the specified function in a goroutine and returns an op to
// retreive the result at a later time.
func doAsync[T any](f func() (T, error)) *op[T] {
	p := &op[T]{
		doneChan: make(chan struct{}),
	}

	go func() {
		p.value, p.err = f()
		close(p.doneChan)
	}()

	return p
}

// doSync calls the specified function synchronously and returns an op to
// retreive the result at a later time.
func doSync[T any](f func() (T, error)) *op[T] {
	p := &op[T]{
		doneChan: make(chan struct{}),
	}

	p.value, p.err = f()
	close(p.doneChan)

	return p
}

// get returns the value or error of the operation.
// Blocks until the operation is complete.
func (p *op[T]) get() (T, error) {
	<-p.doneChan

	return p.value, p.err
}

// Import reads a NAR from a reader, and returns a the root node,
// NAR size and NAR sha256 digest.
func Import(
	// a context, to support cancellation
	ctx context.Context,
	// The reader the data is read from
	r io.Reader,
	// callback function called with each regular file content, may be called concurrently
	blobCb func(fileReader io.Reader) ([]byte, error),
	// callback function called with each finalized directory node, may be called concurrently
	directoryCb func(directory *castorev1pb.Directory) ([]byte, error),
) (*castorev1pb.Node, uint64, []byte, error) {
	// We need to wrap the underlying reader a bit.
	// - we want to keep track of the number of bytes read in total
	// - we calculate the sha256 digest over all data read
	// Express these two things in a MultiWriter, and give the NAR reader a
	// TeeReader that writes to it.
	narCountW := &CountingWriter{}
	sha256W := sha256.New()
	multiW := io.MultiWriter(narCountW, sha256W)
	narReader, err := nar.NewReader(io.TeeReader(r, multiW))
	if err != nil {
		return nil, 0, nil, fmt.Errorf("failed to instantiate nar reader: %w", err)
	}
	defer narReader.Close()

	// If we store a symlink or regular file at the root, these are not nil.
	// If they are nil, we instead have a stackDirectory.
	var rootSymlink *castorev1pb.SymlinkNode
	var rootFileOp *op[*castorev1pb.FileNode]

	var stack = []*stackItem{}

	// popFromStack is used when we transition to a different directory or
	// drain the stack when we reach the end of the NAR.
	// It adds the popped element to the element underneath if any,
	// and passes it to the directoryCb callback.
	// This function may only be called if the stack is not already empty.
	popFromStack := func() (*op[*castorev1pb.DirectoryNode], error) {
		// Keep the top item, and "resize" the stack slice.
		// This will only make the last element unaccessible, but chances are high
		// we're re-using that space anyways.
		toPop := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		directoryOp := doAsync(func() (*castorev1pb.DirectoryNode, error) {
			// Construct the directory proto, this will need to wait on any pending
			// blob uploads of any files in this directory.
			directory, err := toPop.toDirectory()
			if err != nil {
				return nil, fmt.Errorf("failed to build directory: %w", err)
			}

			// call the directoryCb
			directoryDigest, err := directoryCb(directory)
			if err != nil {
				return nil, fmt.Errorf("failed calling directoryCb: %w", err)
			}

			return &castorev1pb.DirectoryNode{
				Name:   []byte(path.Base(toPop.path)),
				Digest: directoryDigest,
				Size:   directory.Size(),
			}, nil
		})

		// if there's still a parent left on the stack, refer to it from there.
		if len(stack) > 0 {
			topOfStack := stack[len(stack)-1]
			topOfStack.directories = append(topOfStack.directories, directoryOp)
		}

		return directoryOp, nil
	}

	getBasename := func(p string) string {
		// extract the basename. In case of "/", replace with empty string.
		basename := path.Base(p)
		if basename == "/" {
			basename = ""
		}
		return basename
	}

	for {
		select {
		case <-ctx.Done():
			return nil, 0, nil, ctx.Err()
		default:
			// call narReader.Next() to get the next element
			hdr, err := narReader.Next()

			// If this returns an error, it's either EOF (when we're done reading from the NAR),
			// or another error.
			if err != nil {
				// if this returns no EOF, bail out
				if !errors.Is(err, io.EOF) {
					return nil, 0, nil, fmt.Errorf("failed getting next nar element: %w", err)
				}

				// The NAR has been read all the way to the end…
				// Make sure we close the nar reader, which might read some final trailers.
				if err := narReader.Close(); err != nil {
					return nil, 0, nil, fmt.Errorf("unable to close nar reader: %w", err)
				}

				// Check the stack. While it's not empty, we need to pop things off the stack.
				// Keep track of the last directory popped as that is the root directory.
				var rootDirectoryOp *op[*castorev1pb.DirectoryNode]
				for len(stack) > 0 {
					var err error
					rootDirectoryOp, err = popFromStack()
					if err != nil {
						return nil, 0, nil, fmt.Errorf("unable to pop from stack: %w", err)
					}
				}

				// Stack is empty.
				// Now either root{File,Symlink,Directory} is not nil,
				// and we can return the root node.
				narSize := narCountW.BytesWritten()
				narSha256 := sha256W.Sum(nil)

				if rootFileOp != nil {
					rootFile, err := rootFileOp.get()
					if err != nil {
						return nil, 0, nil, fmt.Errorf("unable to get root file: %w", err)
					}
					return &castorev1pb.Node{
						Node: &castorev1pb.Node_File{
							File: rootFile,
						},
					}, narSize, narSha256, nil
				} else if rootSymlink != nil {
					return &castorev1pb.Node{
						Node: &castorev1pb.Node_Symlink{
							Symlink: rootSymlink,
						},
					}, narSize, narSha256, nil
				} else if rootDirectoryOp != nil {
					rootDirectory, err := rootDirectoryOp.get()
					if err != nil {
						return nil, 0, nil, fmt.Errorf("unable to get root directory: %w", err)
					}

					return &castorev1pb.Node{
						Node: &castorev1pb.Node_Directory{
							Directory: &castorev1pb.DirectoryNode{
								Name:   []byte{},
								Digest: rootDirectory.Digest,
								Size:   rootDirectory.Size,
							},
						},
					}, narSize, narSha256, nil
				} else {
					return nil, 0, nil, fmt.Errorf("no root set")
				}
			}

			// Check for valid path transitions, pop from stack if needed
			// The nar reader already gives us some guarantees about ordering and illegal transitions,
			// So we really only need to check if the top-of-stack path is a prefix of the path,
			// and if it's not, pop from the stack. We do this repeatedly until the top of the stack is
			// the subdirectory the new entry is in, or we hit the root directory.

			// We don't need to worry about the root node case, because we can only finish the root "/"
			// If we're at the end of the NAR reader (covered by the EOF check)
			for len(stack) > 1 && !strings.HasPrefix(hdr.Path, stack[len(stack)-1].path+"/") {
				_, err := popFromStack()
				if err != nil {
					return nil, 0, nil, fmt.Errorf("unable to pop from stack: %w", err)
				}
			}

			if hdr.Type == nar.TypeSymlink {
				symlinkNode := &castorev1pb.SymlinkNode{
					Name:   []byte(getBasename(hdr.Path)),
					Target: []byte(hdr.LinkTarget),
				}
				if len(stack) > 0 {
					topOfStack := stack[len(stack)-1]
					topOfStack.symlinks = append(topOfStack.symlinks, symlinkNode)
				} else {
					rootSymlink = symlinkNode
				}

			}
			if hdr.Type == nar.TypeRegular {
				uploadBlob := func(r io.Reader) (*castorev1pb.FileNode, error) {
					// wrap reader with a reader counting the number of bytes read
					blobCountW := &CountingWriter{}
					blobReader := io.TeeReader(r, blobCountW)

					blobDigest, err := blobCb(blobReader)
					if err != nil {
						return nil, fmt.Errorf("failure from blobCb: %w", err)
					}

					// ensure blobCb did read all the way to the end.
					// If it didn't, the blobCb function is wrong and we should bail out.
					if blobCountW.BytesWritten() != uint64(hdr.Size) {
						panic("blobCB did not read to end")
					}

					return &castorev1pb.FileNode{
						Name:       []byte(getBasename(hdr.Path)),
						Digest:     blobDigest,
						Size:       uint64(hdr.Size),
						Executable: hdr.Executable,
					}, nil
				}

				// If this is a small enough file, read it off the wire and kick off
				// an asynchronous job to perform the upload.
				// This improves performance when there is a high round trip time
				// uploading individual blobs.
				var nodeOp *op[*castorev1pb.FileNode]
				if hdr.Size < asyncUploadThreshold {
					// Acquire enough space to perform this upload.
					err := asyncUploadSem.Acquire(ctx, hdr.Size)
					if err != nil {
						return nil, 0, nil, fmt.Errorf("unable to acquire blob async upload buffer: %w", err)
					}

					b, err := io.ReadAll(narReader)
					if err != nil {
						asyncUploadSem.Release(hdr.Size)
						return nil, 0, nil, fmt.Errorf("unable to read file: %w", err)
					}

					nodeOp = doAsync(func() (*castorev1pb.FileNode, error) {
						defer asyncUploadSem.Release(hdr.Size)

						return uploadBlob(bytes.NewReader(b))
					})
				} else {
					nodeOp = doSync(func() (*castorev1pb.FileNode, error) {
						return uploadBlob(narReader)
					})
				}

				if len(stack) > 0 {
					topOfStack := stack[len(stack)-1]
					topOfStack.files = append(topOfStack.files, nodeOp)
				} else {
					rootFileOp = nodeOp
				}
			}
			if hdr.Type == nar.TypeDirectory {
				stack = append(stack, &stackItem{
					path: hdr.Path,
				})
			}
		}
	}
}

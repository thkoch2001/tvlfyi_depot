package reader

import (
	"context"
	"crypto/sha256"
	"crypto/sha512"
	"errors"
	"fmt"
	"io"
	"path"
	"strings"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/nix-community/go-nix/pkg/nar"
	"lukechampine.com/blake3"
)

type Reader struct {
	hrSha256 *Hasher
	hrSha512 *Hasher
}

func New(r io.Reader) *Reader {
	// Instead of using the underlying reader itself, wrap the reader
	// with a hasher calculating sha256 and one calculating sha512,
	// and feed that one into the NAR reader.
	hrSha256 := NewHasher(r, sha256.New())
	hrSha512 := NewHasher(hrSha256, sha512.New())

	return &Reader{
		hrSha256: hrSha256,
		hrSha512: hrSha512,
	}
}

// Import reads from the internally-wrapped reader,
// and calls the callback functions whenever regular file contents are
// encountered, or a Directory node is about to be finished.
func (r *Reader) Import(
	ctx context.Context,
	// callback function called with each regular file content
	fileCb func(fileReader io.Reader) error,
	// callback function called with each finalized directory node
	directoryCb func(directory *storev1pb.Directory) error,
) (*storev1pb.PathInfo, error) {

	// construct a NAR reader, by reading through hrSha512 (which will read from hrSha256)
	narReader, err := nar.NewReader(r.hrSha512)
	if err != nil {
		return nil, fmt.Errorf("failed to instantiate nar reader: %w", err)
	}
	defer narReader.Close()

	// if we store a symlink or regular file at the root, these are not nil
	var rootSymlink *storev1pb.SymlinkNode
	var rootFile *storev1pb.FileNode

	var stackPaths = []string{}
	var stackDirectories = []*storev1pb.Directory{}

	// popFromStack returns the current top of the stack,
	// but before returning, adds that element to the element underneath.
	// This function may only be called if the stack is not already empty.
	popFromStack := func() (*storev1pb.Directory, error) {
		toPopDirectory := stackDirectories[len(stackDirectories)-1]
		toPopPath := stackPaths[len(stackPaths)-1]

		// "resize" the two stack slices
		// This will only make the last element unaccessible, but chances are high
		// we're re-using that space anyways.
		stackDirectories = stackDirectories[:len(stackDirectories)-1]
		stackPaths = stackPaths[:len(stackPaths)-1]

		// if there's still a parent left on the stack, refer to it from there.
		if len(stackDirectories) > 0 {
			topOfStack := stackDirectories[len(stackDirectories)-1]

			dgst, err := toPopDirectory.Digest()
			if err != nil {
				return nil, fmt.Errorf("unable to calculate digest: %w", err)
			}

			topOfStack.Directories = append(topOfStack.Directories, &storev1pb.DirectoryNode{
				Name:   path.Base(toPopPath),
				Digest: dgst,
				Size:   toPopDirectory.Size(),
			})
		}
		// In case there's no directory left on the stack, just return toPopDirectory directly.
		return toPopDirectory, nil
	}

	// Assemble PathInfo struct
	// Nodes is populated later.
	assemblePathInfo := func() *storev1pb.PathInfo {
		return &storev1pb.PathInfo{
			Node:       nil,
			References: [][]byte{},
			Narinfo: &storev1pb.NARInfo{
				NarSize: r.hrSha512.BytesWritten(),
				NarHashes: []*storev1pb.NARInfo_NarHash{
					{
						Algo:   storev1pb.NARInfo_SHA256,
						Digest: r.hrSha256.Sum(nil),
					},
					{
						Algo:   storev1pb.NARInfo_SHA512,
						Digest: r.hrSha512.Sum(nil),
					},
				},
				Signatures:     []*storev1pb.NARInfo_Signature{},
				ReferenceNames: []string{},
			},
		}
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
			return nil, ctx.Err()
		default:
			hdr, err := narReader.Next()
			if err != nil {
				// If the NAR has been read all the way to the end…
				if errors.Is(err, io.EOF) {
					// Make sure we read all the way to the end, closing the narReader.
					if err := narReader.Close(); err != nil {
						return nil, fmt.Errorf("unable to close nar reader: %w", err)
					}

					// Check he stack. While it's not empty, we need to pop things off the stack.
					for len(stackDirectories) > 0 {
						directory, err := popFromStack()
						if err != nil {
							return nil, fmt.Errorf("unable to pop from stack: %w", err)
						}

						// call the directoryCb
						if err := directoryCb(directory); err != nil {
							return nil, fmt.Errorf("failed calling directoryCb: %w", err)
						}

						// If the stack now is empty, we popped off the last directory element, which
						// will become the root node in the PathInfo struct.
						if len(stackDirectories) == 0 {
							pi := assemblePathInfo()

							// calculate digest
							dgst, err := directory.Digest()
							if err != nil {
								return nil, fmt.Errorf("unable to calculate root directory digest: %w", err)
							}

							pi.Node = &storev1pb.PathInfo_Directory{
								Directory: &storev1pb.DirectoryNode{
									Name:   "",
									Digest: dgst,
									Size:   directory.Size(),
								},
							}

							// return PathInfo, done!
							return pi, nil
						}
					}

					// Stack is empty. We now either have a regular or symlink root node, 					// so
					// assemble pathInfo with these and return.
					pi := assemblePathInfo()
					if rootFile != nil {
						pi.Node = &storev1pb.PathInfo_File{
							File: rootFile,
						}
					} else if rootSymlink != nil {
						pi.Node = &storev1pb.PathInfo_Symlink{
							Symlink: rootSymlink,
						}
					}
					return pi, nil

				} else {
					return nil, fmt.Errorf("failed getting next nar element: %w", err)
				}
			}

			// Check for valid path transitions, pop from stack if needed
			// The nar reader already gives us some guarantees about ordering and illegal transitions,
			// So we really only need to check if the top-of-stack path is a prefix of the path,
			// and if it's not, pop from the stack.

			// We don't need to worry about the root node case, because we can only finish the root "/"
			// If we're at the end of the NAR reader (covered by the EOF check)
			if len(stackPaths) > 0 && !strings.HasPrefix(hdr.Path, stackPaths[len(stackPaths)-1]) {
				directory, err := popFromStack()
				if err != nil {
					return nil, fmt.Errorf("unable to pop from stack: %w", err)
				}

				// call the directoryCb
				if err := directoryCb(directory); err != nil {
					return nil, fmt.Errorf("failed calling directoryCb: %w", err)
				}
			}

			if hdr.Type == nar.TypeSymlink {
				symlinkNode := &storev1pb.SymlinkNode{
					Name:   getBasename(hdr.Path),
					Target: hdr.LinkTarget,
				}
				if len(stackDirectories) > 0 {
					topOfStack := stackDirectories[len(stackDirectories)-1]
					topOfStack.Symlinks = append(topOfStack.Symlinks, symlinkNode)
					continue
				}

				rootSymlink = symlinkNode
				continue

			}
			if hdr.Type == nar.TypeRegular {
				// wrap reader with a reader calculating the blake3 hash
				fileReader := NewHasher(narReader, blake3.New(32, nil))

				err := fileCb(fileReader)
				if err != nil {
					return nil, fmt.Errorf("failure from fileCb: %w", err)
				}

				// drive the file reader to the end, in case the CB function doesn't read
				// all the way to the end on its own
				if fileReader.BytesWritten() != uint32(hdr.Size) {
					_, err := io.ReadAll(fileReader)
					if err != nil {
						return nil, fmt.Errorf("unable to read until the end of the file content: %w", err)
					}
				}

				// read the blake3 hash
				dgst := fileReader.Sum(nil)

				fileNode := &storev1pb.FileNode{
					Name:       getBasename(hdr.Path),
					Digest:     dgst,
					Size:       uint32(hdr.Size),
					Executable: hdr.Executable,
				}
				if len(stackDirectories) > 0 {
					topOfStack := stackDirectories[len(stackDirectories)-1]
					topOfStack.Files = append(topOfStack.Files, fileNode)
					continue
				}

				rootFile = fileNode
				continue
			}
			if hdr.Type == nar.TypeDirectory {
				stackDirectories = append(stackDirectories, &storev1pb.Directory{
					Directories: []*storev1pb.DirectoryNode{},
					Files:       []*storev1pb.FileNode{},
					Symlinks:    []*storev1pb.SymlinkNode{},
				})
				stackPaths = append(stackPaths, hdr.Path)
				continue
			}
		}
	}
}

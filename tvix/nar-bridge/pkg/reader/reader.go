package reader

import (
	"errors"
	"fmt"
	"io"
	"io/ioutil"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/nix-community/go-nix/pkg/nar"
)

type Reader struct {
	r io.Reader
}

// TODO: caclculate sha256/sha512 of that
func New(r io.Reader) *Reader {
	return &Reader{
		r: r,
	}
}

// Import reads from the internally-wrapped reader,
// and calls the callback functions whenever regular file contents are
// encountered, or a Directory node is about to be finished.
func (r *Reader) Import(
	// callback function called with each regular file content
	fileCb func(fileReader io.Reader) error,
) (*storev1pb.PathInfo, error) {

	// construct a NAR reader
	narReader, err := nar.NewReader(r.r)
	if err != nil {
		return nil, fmt.Errorf("failed to instantiate nar reader: %w", err)
	}

	// TODO: context and cancellation?

	for {
		hdr, err := narReader.Next()
		if err != nil {
			if errors.Is(err, io.EOF) {
				// TODO: we're done, emit pathinfo
				return nil, nil
			} else {
				return nil, fmt.Errorf("failed getting next nar element: %w", err)
			}
		}

		if hdr.Type == nar.TypeSymlink {
			pathInfo := &storev1pb.PathInfo{
				Node: &storev1pb.PathInfo_Symlink{
					Symlink: &storev1pb.SymlinkNode{
						Name:   "",
						Target: hdr.LinkTarget,
					},
				},
				References: [][]byte{}, // TODO: reference scanning?
				Narinfo:    &storev1pb.NARInfo{},
			}

			// TODO: don't emit, but add to stack (ish)
			return pathInfo, nil
		} else if hdr.Type == nar.TypeRegular {
			fmt.Printf("hdr: %v+\n", hdr)
			// call fileCb with a reader to the contents.
			// TODO: wrap this with a blake3 hasher

			// TODO: drive the file reader to the end, in case the CB function doesn't read
			// all the way to the end
			_, err := ioutil.ReadAll(narReader)
			if err != nil {
				return nil, fmt.Errorf("unable to read until the end of the file content: %w", err)
			}

			// TODO: extract hash from the blake3 hasher

			pathInfo := &storev1pb.PathInfo{
				Node: &storev1pb.PathInfo_File{
					File: &storev1pb.FileNode{
						Name:       "",       // TODO: use name here
						Digest:     []byte{}, // TODO: use blake3 hasher here
						Size:       uint32(hdr.Size),
						Executable: hdr.Executable,
					},
				},
				References: [][]byte{},
				Narinfo:    &storev1pb.NARInfo{},
			}
		}
	}
}

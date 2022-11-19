package storev1

import (
	"fmt"

	"google.golang.org/protobuf/proto"
	"lukechampine.com/blake3"
)

// The size of a directory is the number of all regular and symlink elements,
// the number of directory elements, and their size fields.
func (d *Directory) Size() uint32 {
	var size uint32
	size = uint32(len(d.Files) + len(d.Symlinks))
	for _, d := range d.Directories {
		size += 1 + d.Size
	}
	return size
}

func (d *Directory) Digest() ([]byte, error) {
	b, err := proto.MarshalOptions{
		Deterministic: true,
	}.Marshal(d)

	if err != nil {
		return nil, fmt.Errorf("error while marshalling directory: %w", err)
	}

	h := blake3.New(32, nil)

	_, err = h.Write(b)
	if err != nil {
		return nil, fmt.Errorf("error writing to hasher: %w", err)
	}

	return h.Sum(nil), nil
}

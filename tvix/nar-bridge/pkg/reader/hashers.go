package reader

import (
	"fmt"
	"hash"
	"io"
)

var _ io.Reader = &Hasher{}

// Hasher wraps io.Reader.
// You can ask it for the digest of the hash function used internally, and the
// number of bytes written.
type Hasher struct {
	r         io.Reader
	h         hash.Hash
	bytesRead uint32
}

func NewHasher(r io.Reader, h hash.Hash) *Hasher {
	return &Hasher{
		r:         r,
		h:         h,
		bytesRead: 0,
	}
}

func (h *Hasher) Read(p []byte) (int, error) {
	nRead, err := h.r.Read(p)

	if err != nil {
		if err == io.EOF {
			return 0, err
		}
		return nRead, fmt.Errorf("error from underlying reader: %w", err)
	}

	// write data to the hash
	// We assume here the hash function accepts the whole p in one Go,
	// and doesn't early-return on the Write.
	// We compare it with nRead and bail out if that was not the case.
	nHash, err := h.h.Write(p[0:nRead])
	if err != nil {
		return nRead, fmt.Errorf("unable to write to hash: %w", err)
	}

	if nHash != nRead {
		return nRead, fmt.Errorf("hash didn't accept the full write")
	}

	// update bytesWritten
	h.bytesRead += uint32(len(p))

	return nRead, err
}

func (h *Hasher) BytesWritten() uint32 {
	return h.bytesRead
}

func (h *Hasher) Sum(b []byte) []byte {
	return h.h.Sum(b)
}
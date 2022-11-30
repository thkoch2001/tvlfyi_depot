package server

import (
	"bytes"
	"context"
	"fmt"
	"io"

	storev1pb "code.tvl.fyi/tvix/store/protos"
)

// BlobReader reads a blob from a BlobService.
type BlobReader struct {
	ctx    context.Context
	stream storev1pb.BlobService_GetClient
	buf    bytes.Buffer
}

// BlobReader implements io.ReadCloser
var _ io.ReadCloser = &BlobReader{}

func NewBlobReader(ctx context.Context, blobServiceClient storev1pb.BlobServiceClient, blobRef []byte) (*BlobReader, error) {
	stream, err := blobServiceClient.Get(ctx, &storev1pb.GetBlobRequest{
		Digest: blobRef,
	})
	if err != nil {
		return nil, fmt.Errorf("unable to get blob: %w", err)
	}

	return &BlobReader{
		ctx:    ctx,
		stream: stream,
	}, nil

}

// Read implements io.ReadCloser
func (br *BlobReader) Read(p []byte) (n int, err error) {
	// if the buffer is empty, fetch the next block
	if br.buf.Len() == 0 {
		chunk, err := br.stream.Recv()
		if err != nil {
			return 0, err
		}

		// copy received chunk contents to buffer
		_, err = io.Copy(&br.buf, bytes.NewReader(chunk.GetData()))
		if err != nil {
			return 0, fmt.Errorf("unable to copy chunk data to buffer: %w", err)
		}
	}

	// check the size of p, and drain our buffer by that
	return br.buf.Read(p)

}

// Close implements io.ReadCloser
// On a close, we close the stream, so the server can stop sending.
func (br *BlobReader) Close() error {
	return br.stream.CloseSend()
}

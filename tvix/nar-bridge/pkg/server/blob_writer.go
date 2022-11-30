package server

import (
	"bytes"
	"context"
	"encoding/base64"
	"fmt"
	"io"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	log "github.com/sirupsen/logrus"
)

// BlobWriter uploads a blob to a BlobService.
type BlobWriter struct {
	ctx             context.Context
	stream          storev1pb.BlobService_PutClient
	buf             bytes.Buffer
	putBlobResponse *storev1pb.PutBlobResponse
}

// BlobWriter implements io.WriteCloser
var _ io.WriteCloser = &BlobWriter{}

func NewBlobWriter(ctx context.Context, blobServiceClient storev1pb.BlobServiceClient) (*BlobWriter, error) {
	stream, err := blobServiceClient.Put(ctx)
	if err != nil {
		return nil, fmt.Errorf("unable to put blob: %w", err)
	}

	return &BlobWriter{
		ctx:    ctx,
		stream: stream,
	}, nil
}

// Write implements io.WriteCloser
// On an error, the stream is closed, discarding the result.
func (bw *BlobWriter) Write(p []byte) (n int, err error) {
	bw.stream.Send(&storev1pb.BlobChunk{
		Data: p,
	})

	if err != nil {
		// error during sending, close the stream
		_ = bw.stream.CloseSend()

		return 0, fmt.Errorf("unable to send blob chunk: %w", err)
	}

	return len(p), nil
}

// Close implements io.WriteCloser
func (bw *BlobWriter) Close() error {
	putBlobResponse, err := bw.stream.CloseAndRecv()
	if err != nil {
		return fmt.Errorf("unable to close stream: %w", err)
	}

	bw.putBlobResponse = putBlobResponse
	log.WithField("digest", base64.StdEncoding.EncodeToString(putBlobResponse.GetDigest())).Info("uploaded blob")

	return nil
}

// GetResponse returns the response received after closing the stream, or nil.
func (bw *BlobWriter) GetResponse() *storev1pb.PutBlobResponse {
	return bw.putBlobResponse
}

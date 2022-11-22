package server

import (
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"context"
	"encoding/base64"
	"fmt"
	log "github.com/sirupsen/logrus"
	"io"
)

// this returns a callback function that can be used as fileCb
// for the reader.Import function call
func genBlobServiceWriteCb(ctx context.Context, blobServiceClient storev1pb.BlobServiceClient) func(io.Reader) error {
	return func(fileReader io.Reader) error {
		// Read from fileReader into a buffer.
		// We currently buffer all contents and send them to blobServiceClient at once,
		// but that's about to change.
		contents, err := io.ReadAll(fileReader)
		if err != nil {
			return fmt.Errorf("unable to read all contents from file reader: %w", err)
		}
		resp, err := blobServiceClient.Put(
			ctx,
			&storev1pb.PutBlobRequest{
				Data: contents,
			},
		)
		if err != nil {
			// return error to the importer
			return fmt.Errorf("error from blob service: %w", err)
		}

		log.WithField("digest", base64.StdEncoding.EncodeToString(resp.GetDigest())).Info("uploaded blob")

		return nil
	}
}

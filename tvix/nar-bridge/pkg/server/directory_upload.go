package server

import (
	"encoding/hex"
	"fmt"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	log "github.com/sirupsen/logrus"
)

// this returns a callback function that can be used as directoryCb
// for the reader.Import function call
func genDirectoryUploadCb(directoryServicePutStream storev1pb.DirectoryService_PutClient) func(*storev1pb.Directory) error {
	return func(directory *storev1pb.Directory) error {
		directoryDgst, err := directory.Digest()
		if err != nil {
			return fmt.Errorf("failed calculating directory digest: %w", err)
		}
		// Send the directory to the directory service
		err = directoryServicePutStream.Send(directory)
		if err != nil {
			return fmt.Errorf("error sending directory: %w", err)
		}
		log.WithField("digest", hex.EncodeToString(directoryDgst)).Info("uploaded directory")

		return nil
	}
}

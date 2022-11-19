package server

import (
	"bufio"
	"bytes"
	"fmt"
	"net/http"

	"code.tvl.fyi/tvix/nar-bridge/pkg/reader"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/v5"
	nixhash "github.com/nix-community/go-nix/pkg/hash"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	log "github.com/sirupsen/logrus"
	"github.com/ulikunitz/xz"
)

func registerNarPut(s *Server) {
	s.handler.Put(narUrl, func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()

		// parse the narhash sent in the request URL
		narHashFromUrl, err := parseNarHashFromUrl(chi.URLParamFromCtx(ctx, "narhash"))
		if err != nil {
			log.WithError(err).WithField("url", r.URL).Error("unable to decode nar hash from url")
			w.WriteHeader(http.StatusBadRequest)
			_, err := w.Write([]byte("unable to decode nar hash from url"))
			if err != nil {
				log.WithError(err).Error("unable to write error message to client")
			}

			return
		}

		log := log.WithField("narhash_url", narHashFromUrl.SRIString())

		directoriesUploader := NewDirectoriesUploader(ctx, s.directoryServiceClient)
		defer directoriesUploader.Done() //nolint:errcheck

		uncompressed, err := xz.NewReader(r.Body)
		if err != nil {
			log.Errorf("error during NAR import: %v", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		rd := reader.New(bufio.NewReader(uncompressed))
		pathInfo, err := rd.Import(
			ctx,
			genBlobServiceWriteCb(ctx, s.blobServiceClient),
			func(directory *storev1pb.Directory) error {
				return directoriesUploader.Put(directory)
			},
		)

		if err != nil {
			log.Errorf("error during NAR import: %v", err)
			w.WriteHeader(http.StatusInternalServerError)
			_, err := w.Write([]byte(fmt.Sprintf("error during NAR import: %v", err)))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		log.Infof("closing the stream")

		// Close the stream
		putResponse, err := directoriesUploader.Done()
		if err != nil {
			log.WithError(err).Error("error receiving put response")
		}
		log.Infof("closed the stream")

		// check the root digest received back matches what we refer to in our pathInfo
		if !bytes.Equal(
			pathInfo.GetNode().GetDirectory().GetDigest(),
			putResponse.GetRootDigest(),
		) {
			log.WithField("root_digest_pathinfo", pathInfo.GetNode().GetDirectory().GetDigest()).WithField("root_digest_directory_service", putResponse.GetRootDigest()).Errorf("returned root digest doesn't match what's calculated")
			return
		}

		narHash, err := nixhash.ParseNixBase32(
			"sha256:" + nixbase32.EncodeToString(pathInfo.GetNarinfo().NarSha256),
		)
		if err != nil {
			panic(err)
		}

		// Compare the nar hash specified in the URL with the one that has been
		// calculated while processing the NAR file
		// We know the reader populates both fields in the pathinfo struct, so receivedNarHashSha{256,512} are not null.

		// if narHashFromUrl.SRIString() != piNarHashSha256.SRIString() {
		// 	log := log.WithField("narhash_received_sha256", piNarHashSha256.SRIString())

		// 	log.WithField("narsize", pathInfo.Narinfo.NarSize).Error("received bytes don't match narhash from URL")

		// 	w.WriteHeader(http.StatusBadRequest)
		// 	_, err := w.Write([]byte("received bytes don't match narHash specified in URL"))
		// 	if err != nil {
		// 		log.WithError(err).Errorf("unable to write error message to client")
		// 	}

		// 	return

		// }

		// Insert the partial pathinfo structs into our lookup map,
		// so requesting the NAR file will be possible.
		// The same  might exist already, but it'll have the same contents (so
		// replacing will be a no-op), except maybe the root node Name field value, which
		// is safe to ignore (as not part of the NAR).
		s.narHashToPathInfoMu.Lock()
		s.narHashToPathInfo[narHash.SRIString()] = pathInfo
		s.narHashToPathInfoMu.Unlock()

		// Done!
	})

}

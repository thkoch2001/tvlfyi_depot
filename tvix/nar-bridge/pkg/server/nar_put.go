package server

import (
	"bytes"
	"code.tvl.fyi/tvix/nar-bridge/pkg/reader"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"encoding/hex"
	"fmt"
	"github.com/go-chi/chi/v5"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	log "github.com/sirupsen/logrus"
	"net/http"
)

func registerNarPut(s *Server) {
	s.handler.Put("/nar/{narhash:^(["+nixbase32.Alphabet+"]{52}|["+nixbase32.Alphabet+"]{103})$}.nar", func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()
		log := log.WithField("narhash_url", chi.URLParamFromCtx(ctx, "narhash"))

		// parse the narhash sent in the request URL, and keep it for later.
		narHashFromUrl, err := nixbase32.DecodeString(chi.URLParamFromCtx(ctx, "narhash"))
		if err != nil {
			log.Errorf("unable to decode nar hash from url: %v", err)
			w.WriteHeader(http.StatusBadRequest)
			_, err := w.Write([]byte("unable to decode nar hash from url"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		// We are already certain it's 32 or 64 bytes, because our regex on the
		// nixbase32 representation of it ensures it.

		directoriesUploader := NewDirectoriesUploader(ctx, s.directoryServiceClient)
		defer directoriesUploader.Done()

		rd := reader.New(r.Body)
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
			pathInfo.GetDirectory().GetDigest(),
			putResponse.GetRootDigest(),
		) {
			log.WithField("root_digest_pathinfo", pathInfo.GetDirectory().GetDigest()).WithField("root_digest_directory_service", putResponse.GetRootDigest()).Errorf("returned root digest doesn't match what's calculated")
			return
		}

		// extract the narhash (sha256 and sha512)
		var narHashSha256Dgst []byte
		var narHashSha512Dgst []byte

		for _, narHash := range pathInfo.Narinfo.NarHashes {
			if narHash.GetAlgo() == storev1pb.NARInfo_SHA256 {
				narHashSha256Dgst = narHash.GetDigest()
			}
		}

		for _, narHash := range pathInfo.Narinfo.NarHashes {
			if narHash.GetAlgo() == storev1pb.NARInfo_SHA512 {
				narHashSha512Dgst = narHash.GetDigest()
			}
		}

		// Compare the nar hash specified in the URL with the calculated one.
		// Depending on the size, we need to either compare with the sha256 or sha512.
		if len(narHashFromUrl) == 32 {
			if !bytes.Equal(narHashSha256Dgst, narHashFromUrl) {
				log.Errorf("received bytes don't match narHash specified in URL")
				log.WithField("narhash", hex.EncodeToString(narHashSha256Dgst)).Error("received bytes don't match narhash from URL")

				w.WriteHeader(http.StatusBadRequest)
				_, err := w.Write([]byte("received bytes don't match narHash specified in URL"))
				if err != nil {
					log.WithError(err).Errorf("unable to write error message to client")
				}

				return
			}
		} else if len(narHashFromUrl) == 64 {
			if !bytes.Equal(narHashSha512Dgst, narHashFromUrl) {
				log.Errorf("received bytes don't match narHash specified in URL")
				log.WithField("narhash", hex.EncodeToString(narHashSha256Dgst)).Error("received bytes don't match narhash from URL")
				w.WriteHeader(http.StatusBadRequest)

				_, err := w.Write([]byte("received bytes don't match narHash specified in URL"))
				if err != nil {
					log.WithError(err).Errorf("unable to write error message to client")
				}

				return
			}
		}

		// Insert the partial pathinfo structs into our lookup map.
		// The same  might exist already, but it'll have the same contents (so replacing will be a no-op)
		s.narHashToPathInfoMu.Lock()
		s.narHashSha256ToPathInfo[hex.EncodeToString(narHashSha256Dgst)] = pathInfo
		s.narHashSha512ToPathInfo[hex.EncodeToString(narHashSha512Dgst)] = pathInfo
		s.narHashToPathInfoMu.Unlock()

		// Done!
	})

}

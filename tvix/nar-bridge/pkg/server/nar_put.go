package server

import (
	"code.tvl.fyi/tvix/nar-bridge/pkg/reader"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"fmt"
	"github.com/go-chi/chi/v5"
	nixhash "github.com/nix-community/go-nix/pkg/hash"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	log "github.com/sirupsen/logrus"
	"net/http"
)

func registerNarPut(s *Server) {
	s.handler.Put("/nar/{narhash:^(["+nixbase32.Alphabet+"]{52}|["+nixbase32.Alphabet+"]{103})$}.nar", func(w http.ResponseWriter, r *http.Request) {
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

		// Initialize directoryServicePutStream
		// This should not send any data over the wire, only prepare client-side plumbing..
		directoryServicePutStream, err := s.directoryServiceClient.Put(ctx)
		if err != nil {
			log.Errorf("unable to initialize directory service put stream: %v", err)
			w.WriteHeader(http.StatusInternalServerError)
			_, err := w.Write([]byte("unable to initialize directory put stream"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}
		// We want to make sure it gets closed.
		defer func() {
			if err := directoryServicePutStream.CloseSend(); err != nil {
				log.WithError(err).Errorf("unable to close directory service put stream: %v", err)
			}
		}()

		rd := reader.New(r.Body)
		pathInfo, err := rd.Import(
			ctx,
			genBlobServiceWriteCb(ctx, s.blobServiceClient),
			genDirectoryUploadCb(directoryServicePutStream),
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

		// FIXME: do something with the root hash
		putResponse, err := directoryServicePutStream.CloseAndRecv()
		if err != nil {
			log.WithError(err).Error("error receiving put response")
		}

		log.Infof("%v+", putResponse)

		// extract the narhash (sha256 and sha512)
		var piNarHashSha256 *nixhash.Hash
		var piNarHashSha512 *nixhash.Hash

		// TOOD: this is a bit silly right now. We should be able to construct a nixhash
		// by passing its bytes along, without having to encode and decode again.
		for _, piNarHash := range pathInfo.Narinfo.NarHashes {
			if piNarHash.GetAlgo() == storev1pb.NARInfo_SHA256 {
				piNarHashSha256, err = nixhash.ParseNixBase32(
					"sha256:" + nixbase32.EncodeToString(piNarHash.GetDigest()),
				)
				if err != nil {
					panic(err)
				}
			}
			if piNarHash.GetAlgo() == storev1pb.NARInfo_SHA512 {
				piNarHashSha512, err = nixhash.ParseNixBase32(
					"sha512:" + nixbase32.EncodeToString(piNarHash.GetDigest()),
				)
				if err != nil {
					panic(err)
				}
			}
		}

		// Compare the nar hash specified in the URL with the one that has been
		// calculated while processing the NAR file
		// We know the reader populates both fields in the pathinfo struct, so receivedNarHashSha{256,512} are not null.

		if narHashFromUrl.SRIString() != piNarHashSha256.SRIString() && narHashFromUrl.SRIString() != piNarHashSha512.SRIString() {
			log := log.WithField("narhash_received_sha256", piNarHashSha256.SRIString())
			log = log.WithField("narhash_received_sha512", piNarHashSha512.SRIString())

			log.WithField("narsize", pathInfo.Narinfo.NarSize).Error("received bytes don't match narhash from URL")

			w.WriteHeader(http.StatusBadRequest)
			_, err := w.Write([]byte("received bytes don't match narHash specified in URL"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return

		}

		// Insert the partial pathinfo structs into our lookup map,
		// so requesting the NAR file will be possible.
		// The same  might exist already, but it'll have the same contents (so
		// replacing will be a no-op), except maybe the root node Name field value, which
		// is safe to ignore (as not part of the NAR).
		s.narHashToPathInfoMu.Lock()
		s.narHashToPathInfo[piNarHashSha256.SRIString()] = pathInfo
		s.narHashToPathInfo[piNarHashSha512.SRIString()] = pathInfo
		s.narHashToPathInfoMu.Unlock()

		// Done!
	})

}

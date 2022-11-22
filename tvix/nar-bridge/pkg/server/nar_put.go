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
			w.Write([]byte("unable to decode nar hash from url"))

			return
		}

		// We are already certain it's 32 or 64 bytes, because our regex on the
		// nixbase32 representation of it ensures it.

		// Initialize directoryServicePutStream
		// This should not send any data over the wire, only prepare client-side plumbing..
		directoryServicePutStream, err := s.directoryServiceClient.Put(ctx)
		if err != nil {
			log.Errorf("unable to initialize directory service put stream: %v", err)
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte("unable to initialize directory put stream"))

			return
		}
		// We want to make sure it gets closed.
		defer func() {
			if directoryServicePutStream.CloseSend(); err != nil {
				log.Errorf("unable to close directory service put stream: %v", err)
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
			w.Write([]byte(fmt.Sprintf("error during NAR import: %v", err)))

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
				w.Write([]byte("received bytes don't match narHash specified in URL"))

				return
			}
		} else if len(narHashFromUrl) == 64 {
			if !bytes.Equal(narHashSha512Dgst, narHashFromUrl) {
				log.Errorf("received bytes don't match narHash specified in URL")
				log.WithField("narhash", hex.EncodeToString(narHashSha256Dgst)).Error("received bytes don't match narhash from URL")
				w.WriteHeader(http.StatusBadRequest)
				w.Write([]byte("received bytes don't match narHash specified in URL"))

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

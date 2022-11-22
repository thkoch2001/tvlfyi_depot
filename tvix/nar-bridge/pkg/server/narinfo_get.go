package server

import (
	"encoding/hex"
	"net/http"
	"path"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/v5"
	nixhash "github.com/nix-community/go-nix/pkg/hash"
	"github.com/nix-community/go-nix/pkg/narinfo"
	"github.com/nix-community/go-nix/pkg/narinfo/signature"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	"github.com/nix-community/go-nix/pkg/nixpath"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func registerNarinfoGet(s *Server) {
	// GET $outHash.narinfo looks up the PathInfo from the tvix-store,
	// and then render a .narinfo file to the client.
	// It will keep the PathInfo in the lookup map,
	// so a subsequent GET /nar/ $narhash.nar request can find it.
	s.handler.Get("/{outputhash:^["+nixbase32.Alphabet+"]{32}}.narinfo", func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()
		log := log.WithField("outputhash", chi.URLParamFromCtx(ctx, "outputhash"))

		// parse the output hash sent in the request URL
		outputHash, err := nixbase32.DecodeString(chi.URLParamFromCtx(ctx, "outputhash"))
		if err != nil {
			log.WithError(err).Error("unable to decode output hash from url")
			w.WriteHeader(http.StatusBadRequest)
			_, err := w.Write([]byte("unable to decode output hash from url"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		pathInfo, err := s.pathInfoServiceClient.Get(ctx, &storev1pb.GetPathInfoRequest{
			ByWhat: &storev1pb.GetPathInfoRequest_ByOutputHash{
				ByOutputHash: outputHash,
			},
		})
		if err != nil {
			// TODO: how does a 404 look like?
			st, ok := status.FromError(err)
			if ok {
				if st.Code() == codes.NotFound {
					w.WriteHeader(http.StatusNotFound)
				}
				return

			}
			log.WithError(err).Error("Unable to get pathinfo from store")
			w.WriteHeader(http.StatusInternalServerError)
			_, err := w.Write([]byte("Unable to get pathinfo from store"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		// lookup nar hashes
		narHashSha256 := make([]byte, 32)
		s.narHashToPathInfoMu.Lock()
		for _, narHash := range pathInfo.Narinfo.GetNarHashes() {
			if narHash.Algo == storev1pb.NARInfo_SHA256 {
				s.narHashSha256ToPathInfo[hex.EncodeToString(narHash.Digest)] = pathInfo
				narHashSha256 = narHash.Digest
			} else if narHash.Algo == storev1pb.NARInfo_SHA512 {
				s.narHashSha512ToPathInfo[hex.EncodeToString(narHash.Digest)] = pathInfo
			}
		}
		s.narHashToPathInfoMu.Unlock()

		// TODO: go-nix should have better constructor if you already have the bytes
		narNh, err := nixhash.ParseNixBase32("sha256:" + nixbase32.EncodeToString(narHashSha256))
		if err != nil {
			log.WithError(err).Error("Unable to construct nar hash for .narinfo")
			w.WriteHeader(http.StatusInternalServerError)
			_, err := w.Write([]byte("Unable to construct nar hash for .narinfo"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		// convert the signatures from storev1pb signatures to narinfo signatures
		narinfoSignatures := make([]signature.Signature, len(pathInfo.Narinfo.Signatures))
		for _, pathInfoSignature := range pathInfo.Narinfo.Signatures {
			narinfoSignatures = append(narinfoSignatures, signature.Signature{
				Name: pathInfoSignature.GetName(),
				Data: pathInfoSignature.GetData(),
			})
		}

		// extract the name of the node in the pathInfo structure
		nodeName := ""
		switch v := (pathInfo.Node).(type) {
		case *storev1pb.PathInfo_File:
			nodeName = v.File.GetName()
		case *storev1pb.PathInfo_Symlink:
			nodeName = v.Symlink.GetName()
		case *storev1pb.PathInfo_Directory:
			nodeName = v.Directory.GetName()
		}

		narInfo := narinfo.NarInfo{
			StorePath:   path.Join(nixpath.StoreDir, nodeName),
			URL:         "nar/" + nixbase32.EncodeToString(narHashSha256) + ".nar",
			Compression: "none", // TODO: implement zstd compression
			NarHash:     narNh,
			NarSize:     uint64(pathInfo.Narinfo.NarSize),
			References:  pathInfo.Narinfo.GetReferenceNames(),
			Signatures:  narinfoSignatures,
		}

		// render .narinfo from pathInfo
		_, err = w.Write([]byte(narInfo.String()))
		if err != nil {
			log.WithError(err).Errorf("unable to write narinfo to client")
		}
	})
}

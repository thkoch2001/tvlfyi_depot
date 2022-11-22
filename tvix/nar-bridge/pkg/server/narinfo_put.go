package server

import (
	"encoding/hex"
	"net/http"
	"path"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/v5"
	mh "github.com/multiformats/go-multihash/core"
	"github.com/nix-community/go-nix/pkg/narinfo"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	"github.com/nix-community/go-nix/pkg/nixpath"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
)

func registerNarinfoPut(s *Server) {
	s.handler.Put("/{outputhash:^["+nixbase32.Alphabet+"]{32}}.narinfo", func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()
		log := log.WithField("outputhash", chi.URLParamFromCtx(ctx, "outputhash"))

		// parse the output hash sent in the request URL
		outputHash, err := nixbase32.DecodeString(chi.URLParamFromCtx(ctx, "outputhash"))
		if err != nil {
			log.WithError(err).Error("unable to decode output hash from url")
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("unable to decode output hash from url"))

			return
		}

		// TODO: decide on merging behaviour.
		// Maybe it's fine to add if contents are the same, but more sigs can be added?
		// Right now, just fail if we try to upload a .narinfo for a path that already exists?

		_, err = s.pathInfoServiceClient.Get(ctx, &storev1pb.GetPathInfoRequest{
			ByWhat: &storev1pb.GetPathInfoRequest_ByOutputHash{
				ByOutputHash: outputHash,
			},
		})
		if err != nil {
			// TODO: how does a 404 look like?
			log.WithError(err).Error("Unable to get pathinfo from store")
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte("Unable to get pathinfo from store"))
		}

		// read and parse the .narinfo file
		narInfo, err := narinfo.Parse(r.Body)
		if err != nil {
			log.WithError(err).Error("unable to parse narinfo")
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("unable to parse narinfo"))

			return
		}

		log = log.WithFields(logrus.Fields{
			"narhash":     narInfo.NarHash.NixString(),
			"output_path": narInfo.StorePath,
		})

		var pathInfo *storev1pb.PathInfo

		// look up the narHash in our temporary map(s) - sha256
		if narInfo.NarHash.HashType == mh.SHA2_256 {
			foundPathInfo, found := s.narHashSha256ToPathInfo[hex.EncodeToString(narInfo.NarHash.Digest())]
			if found {
				pathInfo = foundPathInfo
			}
		} else if narInfo.NarHash.HashType == mh.SHA2_512 {
			foundPathInfo, found := s.narHashSha512ToPathInfo[hex.EncodeToString(narInfo.NarHash.Digest())]
			if found {
				pathInfo = foundPathInfo
			}
		}

		if pathInfo == nil {
			log.Error("unable to find referred NAR")
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("unable to find referred NAR"))

			return
		}

		// compare fields with what we computed while receiving the NAR file

		// NarSize needs to match
		if pathInfo.Narinfo.NarSize != uint32(narInfo.NarSize) {
			log.Error("narsize mismatch")
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("unable to parse narinfo"))

			return
		}
		// We know the narhash in the .narinfo matches one of the two narhashes in the partial pathInfo,
		// because that's how we found it.

		// FUTUREWORK: We can't compare References yet, but it'd be a good idea to
		// do reference checking on .nar files server-side during upload.
		// We however still need to be parse them, because we store
		// the bytes in pathInfo.References, and the full strings in pathInfo.Narinfo.ReferenceNames.
		referencesBytes := make([][]byte, len(narInfo.References))
		for _, reference := range narInfo.References {
			np, err := nixpath.FromString(reference)
			if err != nil {
				log.WithField("reference", reference).WithError(err).Error("unable to parse reference")
			}
			referencesBytes = append(referencesBytes, np.Digest)
		}

		// assemble the []*storev1pb.NARInfo_Signature{} from narinfo.Signatures.
		pbNarinfoSignatures := make([]*storev1pb.NARInfo_Signature, len(narInfo.Signatures))
		for _, narinfoSig := range narInfo.Signatures {

			pbNarinfoSignatures = append(pbNarinfoSignatures, &storev1pb.NARInfo_Signature{
				Name: narinfoSig.Name,
				Data: narinfoSig.Data,
			})
		}

		// If everything matches, We will add References, NAR signatures and the
		// output path name, and then upload to the pathinfo service.
		// We want a copy here, because we don't want to mutate the contents in the lookup table
		// until we get things back from the remote store.
		pathInfoToUpload := &storev1pb.PathInfo{
			Node:       nil, // set below
			References: referencesBytes,
			Narinfo: &storev1pb.NARInfo{
				NarSize:        pathInfo.Narinfo.NarSize,
				NarHashes:      pathInfo.Narinfo.NarHashes,
				Signatures:     pbNarinfoSignatures,
				ReferenceNames: narInfo.References,
			},
		}

		// We need to add the basename of the storepath from the .narinfo
		// to the pathInfo to be sent.
		switch v := (pathInfo.Node).(type) {
		case *storev1pb.PathInfo_File:
			pathInfoToUpload.Node = &storev1pb.PathInfo_File{
				File: &storev1pb.FileNode{
					Name:       path.Base(narInfo.StorePath),
					Digest:     v.File.Digest,
					Size:       v.File.Size,
					Executable: v.File.Executable,
				},
			}
		case *storev1pb.PathInfo_Symlink:
			pathInfoToUpload.Node = &storev1pb.PathInfo_Symlink{
				Symlink: &storev1pb.SymlinkNode{
					Name:   path.Base(narInfo.StorePath),
					Target: v.Symlink.Target,
				},
			}
		case *storev1pb.PathInfo_Directory:
			pathInfoToUpload.Node = &storev1pb.PathInfo_Directory{
				Directory: &storev1pb.DirectoryNode{
					Name:   path.Base(narInfo.StorePath),
					Digest: v.Directory.Digest,
					Size:   v.Directory.Size,
				},
			}
		}

		receivedPathInfo, err := s.pathInfoServiceClient.Put(ctx, pathInfoToUpload)
		if err != nil {
			log.Error("unable to upload pathinfo to service")
			w.WriteHeader(http.StatusInternalServerError)
			log.WithError(err).Error("unable to upload pathinfo to service")

			return
		}

		log.Infof("received new pathInfo: %v+", receivedPathInfo)

		// TODO: update the local temporary pathinfo with this?
	})
}

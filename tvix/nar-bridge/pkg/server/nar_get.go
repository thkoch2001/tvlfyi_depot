package server

import (
	"bytes"
	"encoding/hex"
	"fmt"
	"io"
	"io/fs"
	"net/http"

	"code.tvl.fyi/tvix/nar-bridge/pkg/writer"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/v5"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	log "github.com/sirupsen/logrus"
)

const (
	narUrl = "/nar/{narhash:^([" + nixbase32.Alphabet + "]{52}|[" + nixbase32.Alphabet + "]{103})$}.nar"
)

func registerNarGet(s *Server) {
	// TODO: properly compose this
	s.handler.Head(narUrl, func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()

		// parse the narhash sent in the request URL
		narHash, err := parseNarHashFromUrl(chi.URLParamFromCtx(ctx, "narhash"))
		if err != nil {
			log.WithError(err).WithField("url", r.URL).Error("unable to decode nar hash from url")
			w.WriteHeader(http.StatusBadRequest)
			_, err := w.Write([]byte("unable to decode nar hash from url"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		log := log.WithField("narhash_url", narHash.SRIString())

		// look in the lookup table
		s.narHashToPathInfoMu.Lock()
		_, found := s.narHashToPathInfo[narHash.SRIString()]
		s.narHashToPathInfoMu.Unlock()

		// if we didn't find anything, return 404.
		if !found {
			w.WriteHeader(http.StatusNotFound)
			_, err := w.Write([]byte("narHash not found"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}
		w.WriteHeader(http.StatusNoContent)

	})
	s.handler.Get(narUrl, func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()

		// parse the narhash sent in the request URL
		narHash, err := parseNarHashFromUrl(chi.URLParamFromCtx(ctx, "narhash"))
		if err != nil {
			log.WithError(err).WithField("url", r.URL).Error("unable to decode nar hash from url")
			w.WriteHeader(http.StatusBadRequest)
			_, err := w.Write([]byte("unable to decode nar hash from url"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		log := log.WithField("narhash_url", narHash.SRIString())

		var pathInfo *storev1pb.PathInfo

		// look in the lookup table
		s.narHashToPathInfoMu.Lock()
		pathInfo, found := s.narHashToPathInfo[narHash.SRIString()]
		s.narHashToPathInfoMu.Unlock()

		// if we didn't find anything, return 404.
		if !found {
			w.WriteHeader(http.StatusNotFound)
			_, err := w.Write([]byte("narHash not found"))
			if err != nil {
				log.WithError(err).Errorf("unable to write error message to client")
			}

			return
		}

		directories := make(map[string]*storev1pb.Directory)

		// If the root node is a directory, ask the directory service for all directories
		if pathInfoDirectory, isDirectory := pathInfo.GetNode().(*storev1pb.PathInfo_Directory); isDirectory {
			rootDirectoryDigest := pathInfoDirectory.Directory.GetDigest()
			log = log.WithField("root_directory_digest", rootDirectoryDigest)
			log.Infof("GET Directory recursive")

			directoryStream, err := s.directoryServiceClient.Get(ctx, &storev1pb.GetDirectoryRequest{
				ByWhat: &storev1pb.GetDirectoryRequest_Digest{
					Digest: rootDirectoryDigest,
				},
				Recursive: true,
			})
			if err != nil {
				log.WithError(err).Error("unable to query directory stream")
				w.WriteHeader(http.StatusInternalServerError)
				_, err := w.Write([]byte("unable to query directory stream"))
				if err != nil {
					log.WithError(err).Errorf("unable to write error message to client")
				}

				return
			}

			// For now, we just stream all of these locally and put them into a hashmap,
			// which is used in the lookup function below.
			for {
				directory, err := directoryStream.Recv()
				if err != nil {
					if err == io.EOF {
						break
					}
					log.WithError(err).Error("unable to receive from directory stream")
					w.WriteHeader(http.StatusInternalServerError)
					_, err := w.Write([]byte("unable to receive from directory stream"))
					if err != nil {
						log.WithError(err).Errorf("unable to write error message to client")
					}

					return
				}

				// calculate directory digest
				// TODO: do we need to do any more validation?
				directoryDgst, err := directory.Digest()
				if err != nil {
					log.WithError(err).Error("unable to calculate directory digest")
					w.WriteHeader(http.StatusInternalServerError)
					_, err := w.Write([]byte("unable to calculate directory digest"))
					if err != nil {
						log.WithError(err).Errorf("unable to write error message to client")
					}

					return
				}

				log.Info("received directory node: %x", directoryDgst)

				directories[hex.EncodeToString(directoryDgst)] = directory
			}

		}

		// render the NAR file
		err = writer.Export(
			w,
			pathInfo,
			func(directoryRef []byte) (*storev1pb.Directory, error) {
				directoryRefStr := hex.EncodeToString(directoryRef)
				directory, found := directories[directoryRefStr]
				if !found {
					return nil, fmt.Errorf(
						"directory with hash %v does not exist: %w",
						directoryRef,
						fs.ErrNotExist,
					)
				}

				return directory, nil
			},
			func(blobRef []byte) (io.ReadCloser, error) {
				log.Infof("Get blob with digest: %x", blobRef)
				resp, err := s.blobServiceClient.Get(ctx, &storev1pb.GetBlobRequest{
					Digest: blobRef,
				})
				if err != nil {
					return nil, fmt.Errorf("unable to get blob: %w", err)

				}
				return io.NopCloser(bytes.NewReader(resp.GetData())), nil
			},
		)
		if err != nil {
			log.WithError(err).Error("unable to export nar")
		}
	})
}

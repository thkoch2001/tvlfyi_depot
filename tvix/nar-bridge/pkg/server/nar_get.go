package server

import (
	"context"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"net/http"
	"sync"

	"code.tvl.fyi/tvix/nar-bridge/pkg/writer"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/v5"
	nixhash "github.com/nix-community/go-nix/pkg/hash"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	log "github.com/sirupsen/logrus"
)

const (
	narUrl = "/nar/{narhash:^([" + nixbase32.Alphabet + "]{52}|[" + nixbase32.Alphabet + "]{103})$}.nar"
)

func renderNar(
	ctx context.Context,
	log *log.Entry,
	directoryServiceClient storev1pb.DirectoryServiceClient,
	blobServiceClient storev1pb.BlobServiceClient,
	narHashToPathInfoMu *sync.Mutex,
	narHashToPathInfo map[string]*storev1pb.PathInfo,
	w io.Writer,
	narHash *nixhash.Hash,
	headOnly bool,
) error {
	// look in the lookup table
	narHashToPathInfoMu.Lock()
	pathInfo, found := narHashToPathInfo[narHash.SRIString()]
	narHashToPathInfoMu.Unlock()

	// if we didn't find anything, return 404.
	if !found {
		return fmt.Errorf("narHash not found: %w", fs.ErrNotExist)
	}

	// if this was only a head request, we're done.
	if headOnly {
		return nil
	}

	directories := make(map[string]*storev1pb.Directory)

	// If the root node is a directory, ask the directory service for all directories
	if pathInfoDirectory, isDirectory := pathInfo.GetNode().(*storev1pb.PathInfo_Directory); isDirectory {
		rootDirectoryDigest := pathInfoDirectory.Directory.GetDigest()
		log = log.WithField("root_directory_digest", rootDirectoryDigest)

		directoryStream, err := directoryServiceClient.Get(ctx, &storev1pb.GetDirectoryRequest{
			ByWhat: &storev1pb.GetDirectoryRequest_Digest{
				Digest: rootDirectoryDigest,
			},
			Recursive: true,
		})
		if err != nil {
			return fmt.Errorf("unable to query directory stream: %w", err)
		}

		// For now, we just stream all of these locally and put them into a hashmap,
		// which is used in the lookup function below.
		for {
			directory, err := directoryStream.Recv()
			if err != nil {
				if err == io.EOF {
					break
				}
				return fmt.Errorf("unable to receive from directory stream: %w", err)
			}

			// calculate directory digest
			// TODO: do we need to do any more validation?
			directoryDgst, err := directory.Digest()
			if err != nil {
				return fmt.Errorf("unable to calculate directory digest: %w", err)
			}

			log.WithField("directory", base64.StdEncoding.EncodeToString(directoryDgst)).Info("received directory node")

			directories[hex.EncodeToString(directoryDgst)] = directory
		}

	}

	// render the NAR file
	err := writer.Export(
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

			blobReader, err := NewBlobReader(ctx, blobServiceClient, blobRef)
			if err != nil {
				return nil, fmt.Errorf("unable to get blob: %w", err)
			}
			return blobReader, nil
		},
	)
	if err != nil {
		return fmt.Errorf("unable to export nar: %w", err)
	}
	return nil
}

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

		err = renderNar(ctx, log, s.directoryServiceClient, s.blobServiceClient, &s.narHashToPathInfoMu, s.narHashToPathInfo, w, narHash, true)
		if err != nil {
			log.WithError(err).Info("unable to render nar")
			if errors.Is(err, fs.ErrNotExist) {
				w.WriteHeader(http.StatusNotFound)
			} else {
				w.WriteHeader(http.StatusInternalServerError)
			}
		}

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

		err = renderNar(ctx, log, s.directoryServiceClient, s.blobServiceClient, &s.narHashToPathInfoMu, s.narHashToPathInfo, w, narHash, false)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				w.WriteHeader(http.StatusNotFound)
			} else {
				w.WriteHeader(http.StatusInternalServerError)
			}
		}
	})
}

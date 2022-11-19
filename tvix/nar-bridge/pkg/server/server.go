package server

import (
	"bytes"
	"encoding/hex"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"sync"
	"time"

	"code.tvl.fyi/tvix/nar-bridge/pkg/reader"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/chi/v5"
	"github.com/nix-community/go-nix/pkg/nixbase32"
	log "github.com/sirupsen/logrus"
)

type Server struct {
	handler chi.Router

	directoryServiceClient storev1pb.DirectoryServiceClient
	blobServiceClient      storev1pb.BlobServiceClient
	pathInfoServiceClient  storev1pb.PathInfoServiceClient

	// When uploading NAR files to a HTTP binary cache, the .nar
	// files are uploaded before the .narinfo files.
	// We need *both* to be able to fully construct a PathInfo object.
	// Keep a in-memory map of narhash to sparse PathInfo.
	narHashToPathInfoMu     sync.Mutex
	narHashSha256ToPathInfo map[string]*storev1pb.PathInfo
	narHashSha512ToPathInfo map[string]*storev1pb.PathInfo
}

func registerNarPut(s *Server) {
	s.handler.Put("/nar/{narhash:^(["+nixbase32.Alphabet+"]{52}|["+nixbase32.Alphabet+"]{103})$}.nar", func(w http.ResponseWriter, r *http.Request) {
		//s.handler.Put("/nar/{narhash:^["+nixbase32.Alphabet+"]{52}$}.nar", func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()

		ctx := r.Context()

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
			func(fileReader io.Reader) error {
				// Read from fileReader into a buffer.
				// We currently buffer all contents and send them to blobServiceClient at once,
				// but that's about to change.
				contents, err := ioutil.ReadAll(fileReader)
				resp, err := s.blobServiceClient.Put(
					ctx,
					&storev1pb.PutBlobRequest{
						Data: contents,
					},
				)
				if err != nil {
					// return error to the importer
					return fmt.Errorf("error from blob service: %w", err)
				}

				log.WithFields(log.Fields{
					"digest": hex.EncodeToString(resp.GetDigest()),
				}).Debug("uploaded blob")

				return nil
			},
			func(directory *storev1pb.Directory) error {
				// If the client doesn't exist yet, set it up
				if directoryServicePutStream == nil {
					directoryServicePutStream, err = s.directoryServiceClient.Put(ctx)
					if err != nil {
						return fmt.Errorf("error initializing directory service put stream: %w", err)
					}
				}

				directoryDgst, err := directory.Digest()
				if err != nil {
					return fmt.Errorf("failed calculating directory digest: %w", err)
				}
				// Send the directory to the directory service
				err = directoryServicePutStream.Send(directory)
				if err != nil {
					return fmt.Errorf("error sending directory: %w", err)
				}
				log.WithFields(log.Fields{
					"digest": hex.EncodeToString(directoryDgst),
				}).Debug("uploaded directory")

				return nil
			},
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
				log.WithFields(log.Fields{
					"narHashFromUrl":  hex.EncodeToString(narHashFromUrl),
					"narHashReceived": hex.EncodeToString(narHashSha256Dgst),
				}).Error("received bytes don't match narhash from URL")
				w.WriteHeader(http.StatusBadRequest)
				w.Write([]byte("received bytes don't match narHash specified in URL"))

				return
			}
		} else if len(narHashFromUrl) == 64 {
			if !bytes.Equal(narHashSha512Dgst, narHashFromUrl) {
				log.Errorf("received bytes don't match narHash specified in URL")
				log.WithFields(log.Fields{
					"narHashFromUrl":  hex.EncodeToString(narHashFromUrl),
					"narHashReceived": hex.EncodeToString(narHashSha512Dgst),
				}).Error("received bytes don't match narhash from URL")
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

func New(
	directoryServiceClient storev1pb.DirectoryServiceClient,
	blobServiceClient storev1pb.BlobServiceClient,
	pathInfoServiceClient storev1pb.PathInfoServiceClient,
	enableAccessLog bool,
	priority int,
) *Server {
	r := chi.NewRouter()

	if enableAccessLog {
		r.Use(middleware.Logger)
	}

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		_, err := w.Write([]byte("nar-bridge"))
		if err != nil {
			log.Errorf("Unable to write response: %v", err)
		}
	})

	r.Get("/nix-cache-info", func(w http.ResponseWriter, r *http.Request) {
		_, err := w.Write([]byte(fmt.Sprintf("StoreDir: /nix/store\nWantMassQuery: 1\nPriority: %d\n", priority)))
		if err != nil {
			log.Errorf("Unable to write response: %v", err)
		}
	})

	s := &Server{
		handler:                r,
		directoryServiceClient: directoryServiceClient,
		blobServiceClient:      blobServiceClient,
		pathInfoServiceClient:  pathInfoServiceClient,
	}

	registerNarPut(s)

	return s
}

func (s *Server) ListenAndServe(addr string) error {
	srv := &http.Server{
		Addr:         addr,
		Handler:      s.handler,
		ReadTimeout:  50 * time.Second,
		WriteTimeout: 100 * time.Second,
		IdleTimeout:  150 * time.Second,
	}

	return srv.ListenAndServe()
}

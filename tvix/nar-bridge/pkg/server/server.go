package server

import (
	"fmt"
	"net/http"
	"time"

	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/chi/v5"
	log "github.com/sirupsen/logrus"
)

type Server struct {
	handler chi.Router
}

func New(directoryServiceClient storev1pb.DirectoryServiceClient,
	blobServiceClient storev1pb.BlobServiceClient,
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

	return &Server{
		handler: r,
	}
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

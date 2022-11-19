package main

import (
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"os"
	"os/signal"

	"code.tvl.fyi/tvix/nar-bridge/pkg/server"
	storev1pb "code.tvl.fyi/tvix/store/protos"
	"github.com/alecthomas/kong"
	log "github.com/sirupsen/logrus"
)

var CLI struct {
	ListenAddr      string `name:"listen-addr" help:"The address this service listens on" type:"string" default:"[::]:9000"` //nolint:lll
	EnableAccessLog bool   `name:"access-log" help:"Enable access logging" type:"bool" default:"true" negatable:""`          //nolint:lll
	StoreAddr       string `name:"store-addr" help:"The address to the tvix-store RPC interface this will connect to"`
}

// `help:"Expose a tvix-store RPC interface as NAR/NARInfo"`

func main() {
	retcode := 0

	defer func() { os.Exit(retcode) }()

	_ = kong.Parse(&CLI)

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)

	go func() {
		for range c {
			log.Info("Received Signal, shutting down…")
			//s.Close()
			os.Exit(1)
		}
	}()

	// connect to tvix-store
	log.Debugf("Dialing to %v", CLI.StoreAddr)
	conn, err := grpc.Dial(CLI.StoreAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	log.Printf("Starting nar-bridge at %v", CLI.ListenAddr)
	s := server.New(
		storev1pb.NewDirectoryServiceClient(conn), storev1pb.NewBlobServiceClient(conn), CLI.EnableAccessLog, 30,
	)

	err = s.ListenAndServe(CLI.ListenAddr)
	if err != nil {
		log.Error("Server failed: %w", err)
	}
}

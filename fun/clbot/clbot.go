package main

import (
	"context"
	"flag"
	"io/ioutil"
	"os"
	"os/signal"
	"time"

	"code.tvl.fyi/fun/clbot/gerrit"
	"github.com/davecgh/go-spew/spew"
	log "github.com/golang/glog"
	"golang.org/x/crypto/ssh"
)

var (
	gerritAddr       = flag.String("gerrit_host", "cl.tvl.fyi:29418", "Gerrit SSH host:port")
	gerritSSHHostKey = flag.String("gerrit_ssh_pubkey", "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIUNYBYPCCBNDFSd0BuCR+8kgeuJ7IA5S2nTNQmkQUYNyXK+ot5os7rHtCk96+grd5+J8jFCuFBWisUe8h8NC0Q=", "Gerrit SSH public key")
	gerritSSHTimeout = flag.Duration("gerrit_tcp_timeout", 5*time.Second, "Gerrit SSH TCP connect timeout")

	required           = flag.NewFlagSet("required flags", flag.ExitOnError)
	gerritAuthUsername = required.String("gerrit_ssh_auth_username", "", "Gerrit SSH username")
	gerritAuthKeyPath  = required.String("gerrit_ssh_auth_key", "", "Gerrit SSH private key path")
)

func mustFixedHostKey(f string) ssh.HostKeyCallback {
	pk, _, _, _, err := ssh.ParseAuthorizedKey([]byte(f))
	if err != nil {
		log.Exitf("ParseAuthorizedKey(%q): %v", f, err)
	}
	return ssh.FixedHostKey(pk)
}

func mustPrivateKey(p string) ssh.AuthMethod {
	pkBytes, err := ioutil.ReadFile(p)
	if err != nil {
		log.Exitf("reading SSH private key from %q: %v", p, err)
	}
	pk, err := ssh.ParsePrivateKey(pkBytes)
	if err != nil {
		log.Exitf("parsing private key from %q: %v", p, err)
	}
	return ssh.PublicKeys(pk)
}

func checkRequired(fs *flag.FlagSet) {
	missing := map[string]bool{}
	fs.VisitAll(func(f *flag.Flag) { missing[f.Name] = true })
	fs.Visit(func(f *flag.Flag) { delete(missing, f.Name) })
	for f := range missing {
		log.Errorf("flag %q was unset but is required", f)
	}
	if len(missing) > 0 {
		os.Exit(1)
	}
}

var shutdownFuncs []func()

func callOnShutdown(f func()) {
	shutdownFuncs = append(shutdownFuncs, f)
}

func main() {
	flag.Parse()
	checkRequired(required)

	shutdownCh := make(chan os.Signal)
	signal.Notify(shutdownCh, os.Interrupt)
	go func() {
		<-shutdownCh
		for n := len(shutdownFuncs); n >= 0; n-- {
			shutdownFuncs[n]()
		}
	}()

	ctx, cancel := context.WithCancel(context.Background())
	callOnShutdown(cancel)
	cfg := &ssh.ClientConfig{
		User:            *gerritAuthUsername,
		Auth:            []ssh.AuthMethod{mustPrivateKey(*gerritAuthKeyPath)},
		HostKeyCallback: mustFixedHostKey(*gerritSSHHostKey),
		Timeout:         *gerritSSHTimeout,
	}
	cfg.SetDefaults()

	gw, err := gerrit.New(ctx, "tcp", *gerritAddr, cfg)
	if err != nil {
		log.Errorf("gerrit.New(%q): %v", *gerritAddr, err)
	}
	callOnShutdown(func() {
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		gw.Close(ctx)
	})

	for e := range gw.Events() {
		log.Infof("hello: %v", spew.Sdump(e))
	}
}

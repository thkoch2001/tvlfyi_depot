package main

import (
	"context"
	"flag"
	"io/ioutil"
	"os"
	"time"

	"code.tvl.fyi/fun/clbot/gerrit"
	"github.com/davecgh/go-spew/spew"
	log "github.com/golang/glog"
	"golang.org/x/crypto/ssh"
)

var (
	gerritAddr       = flag.String("gerrit_host", "cl.tvl.fyi:29418", "Gerrit SSH host:port")
	gerritSSHKey     = flag.String("gerrit_ssh_pubkey", "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIUNYBYPCCBNDFSd0BuCR+8kgeuJ7IA5S2nTNQmkQUYNyXK+ot5os7rHtCk96+grd5+J8jFCuFBWisUe8h8NC0Q=", "Gerrit SSH public key")
	gerritSSHTimeout = flag.Duration("gerrit_tcp_timeout", 5*time.Second, "Gerrit SSH TCP connect timeout")

	gerritAuthUsername = flag.String("gerrit_ssh_auth_username", "", "Gerrit SSH username")
	gerritAuthKeyPath  = flag.String("gerrit_ssh_auth_key", "", "Gerrit SSH private key path")
	_                  = markRequired("gerrit_ssh_auth_username", "gerrit_ssh_auth_key")
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

var requiredFlags = map[string]bool{}

func markRequired(fs ...string) error {
	for _, f := range fs {
		requiredFlags[f] = true
	}
	return nil
}

func checkRequired() {
	seen := make(map[string]bool)
	flag.Visit(func(f *flag.Flag) { seen[f.Name] = true })
	failed := false
	for f := range requiredFlags {
		if !seen[f] {
			log.Errorf("flag %q was unset but is required", f)
			failed = true
		}
	}
	if failed {
		os.Exit(1)
	}
}

func main() {
	flag.Parse()
	checkRequired()

	ctx := context.Background()
	cfg := &ssh.ClientConfig{
		User:            *gerritAuthUsername,
		Auth:            []ssh.AuthMethod{mustPrivateKey(*gerritAuthKeyPath)},
		HostKeyCallback: mustFixedHostKey(*gerritSSHKey),
		Timeout:         *gerritSSHTimeout,
	}
	cfg.SetDefaults()
	gw, err := gerrit.New(ctx, "tcp", *gerritAddr, cfg)
	if err != nil {
		log.Errorf("gerrit.New(%q): %v", *gerritAddr, err)
	}

	for e := range gw.Events() {
		log.Infof("hello: %v", spew.Sdump(e))
	}
}

package gerrit

import (
	"context"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/subtle"
	"fmt"
	"net"
	"testing"
	"time"

	"code.tvl.fyi/fun/clbot/gerrit/gerritevents"
	log "github.com/golang/glog"
	"github.com/google/go-cmp/cmp"
	"golang.org/x/crypto/ssh"
)

var (
	sshServerSigner, sshServerPublicKey = mustNewKey()
	sshClientSigner, sshClientPublicKey = mustNewKey()
)

func mustNewKey() (ssh.Signer, ssh.PublicKey) {
	key, err := ecdsa.GenerateKey(elliptic.P384(), rand.Reader)
	if err != nil {
		panic(err)
	}
	signer, err := ssh.NewSignerFromKey(key)
	if err != nil {
		panic(err)
	}
	publicKey, err := ssh.NewPublicKey(key.Public())
	if err != nil {
		panic(err)
	}
	return signer, publicKey
}

func newSSHServer(lines string) (addr string, cleanup func(), err error) {
	config := &ssh.ServerConfig{
		PublicKeyCallback: func(c ssh.ConnMetadata, pubKey ssh.PublicKey) (*ssh.Permissions, error) {
			pkBytes := pubKey.Marshal()
			wantPKBytes := sshClientPublicKey.Marshal()
			if subtle.ConstantTimeCompare(pkBytes, wantPKBytes) == 0 {
				return nil, fmt.Errorf("unauthorized")
			}
			return &ssh.Permissions{}, nil
		},
	}
	config.AddHostKey(sshServerSigner)

	ln, err := net.Listen("tcp", ":0")
	if err != nil {
		log.Fatalf("Listen on tcp/:0: %v", err)
	}
	handle := func(conn net.Conn) {
		defer conn.Close()

		sc, newchch, newreqch, err := ssh.NewServerConn(conn, config)
		if err != nil {
			log.Fatalf("NewServerConn: %v", err)
		}
		go ssh.DiscardRequests(newreqch)
		for newCh := range newchch {
			if newCh.ChannelType() != "session" {
				newCh.Reject(ssh.UnknownChannelType, "unknown channel type")
				continue
			}

			channel, reqs, err := newCh.Accept()
			if err != nil {
				log.Fatalf("Could not accept channel: %v", err)
			}
			go func(in <-chan *ssh.Request) {
				for req := range in {
					req.Reply(req.Type == "exec", nil)
				}
			}(reqs)
			channel.Write([]byte(lines))
			sc.SendRequest("goaway", false, nil)
		}
	}
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			go handle(conn)
		}
	}()

	cleanup = func() {
		ln.Close()
	}
	return ln.Addr().String(), cleanup, err
}

func ts(s string) gerritevents.Time {
	t, err := time.Parse("2006-01-02 15:04:05 -0700 MST", s)
	if err != nil {
		panic(err)
	}
	return gerritevents.Time{t}
}

func optStr(s string) *string { return &s }

func TestWatcher(t *testing.T) {
	tcs := []struct {
		name  string
		lines string
		want  []gerritevents.Event
	}{{
		name: "no events",
		lines: `{"author":{"name":"tazjin","email":"mail@tazj.in","username":"tazjin"},"approvals":[{"type":"Code-Review","description":"Code-Review","value":"2","oldValue":"0"}],"comment":"Patch Set 3: Code-Review+2","patchSet":{"number":3,"revision":"6fe272d3f82c6efdfe1167fab98bf918efc03fe5","parents":["d984b6018cf68c7e8b7169b475d90134fbcee767"],"ref":"refs/changes/44/244/3","uploader":{"name":"tazjin","email":"mail@tazj.in","username":"tazjin"},"createdOn":1592081910,"author":{"name":"tazjin","email":"mail@tazj.in","username":"tazjin"},"kind":"REWORK","sizeInsertions":83,"sizeDeletions":-156},"change":{"project":"depot","branch":"master","id":"I546c701145fa204b7ba7518a8a56a783588629e0","number":244,"subject":"refactor(ops/nixos): Move my NixOS configurations to //users/tazjin","owner":{"name":"tazjin","email":"mail@tazj.in","username":"tazjin"},"url":"https://cl.tvl.fyi/c/depot/+/244","commitMessage":"refactor(ops/nixos): Move my NixOS configurations to //users/tazjin\n\nNixOS modules move one level up because it\u0027s unlikely that //ops/nixos\nwill contain actual systems at this point (they\u0027re user-specific).\n\nThis is the first users folder, so it is also added to the root\nreadTree invocation for the repository.\n\nChange-Id: I546c701145fa204b7ba7518a8a56a783588629e0\n","createdOn":1592081577,"status":"NEW"},"project":"depot","refName":"refs/heads/master","changeKey":{"id":"I546c701145fa204b7ba7518a8a56a783588629e0"},"type":"comment-added","eventCreatedOn":1592081929}
`,
		want: []gerritevents.Event{
			&gerritevents.CommentAdded{
				Type: "comment-added",
				Change: gerritevents.Change{
					Project:       "depot",
					Branch:        "master",
					ID:            "I546c701145fa204b7ba7518a8a56a783588629e0",
					Number:        244,
					Subject:       "refactor(ops/nixos): Move my NixOS configurations to //users/tazjin",
					Owner:         gerritevents.Account{Name: "tazjin", Email: "mail@tazj.in", Username: "tazjin"},
					URL:           "https://cl.tvl.fyi/c/depot/+/244",
					CommitMessage: "refactor(ops/nixos): Move my NixOS configurations to //users/tazjin\n\nNixOS modules move one level up because it's unlikely that //ops/nixos\nwill contain actual systems at this point (they're user-specific).\n\nThis is the first users folder, so it is also added to the root\nreadTree invocation for the repository.\n\nChange-Id: I546c701145fa204b7ba7518a8a56a783588629e0\n",
					CreatedOn:     ts("2020-06-13 21:52:57 +0100 BST"),
					Status:        "NEW",
				},
				PatchSet: gerritevents.PatchSet{
					Number:         3,
					Revision:       "6fe272d3f82c6efdfe1167fab98bf918efc03fe5",
					Parents:        []string{"d984b6018cf68c7e8b7169b475d90134fbcee767"},
					Ref:            "refs/changes/44/244/3",
					Uploader:       gerritevents.Account{Name: "tazjin", Email: "mail@tazj.in", Username: "tazjin"},
					Author:         gerritevents.Account{Name: "tazjin", Email: "mail@tazj.in", Username: "tazjin"},
					CreatedOn:      ts("2020-06-13 21:58:30 +0100 BST"),
					Kind:           "REWORK",
					SizeInsertions: 83,
					SizeDeletions:  -156,
				},
				Author:         gerritevents.Account{Name: "tazjin", Email: "mail@tazj.in", Username: "tazjin"},
				Approvals:      []gerritevents.Approval{{Type: "Code-Review", Description: "Code-Review", Value: "2", OldValue: optStr("0")}},
				Comment:        "Patch Set 3: Code-Review+2",
				EventCreatedOn: ts("2020-06-13 21:58:49 +0100 BST"),
			},
		},
	}}
	for _, tc := range tcs {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			serverAddr, cleanup, err := newSSHServer(tc.lines)
			if err != nil {
				t.Fatalf("newSSHServer: %v", err)
			}
			t.Cleanup(cleanup)

			config := &ssh.ClientConfig{
				User:            "bert",
				Auth:            []ssh.AuthMethod{ssh.PublicKeys(sshClientSigner)},
				HostKeyCallback: ssh.FixedHostKey(sshServerPublicKey),
				Timeout:         10 * time.Millisecond,
			}
			w, err := New(ctx, "tcp", serverAddr, config)
			if err != nil {
				t.Fatalf("New: %v", err)
			}

			var gotEvents []gerritevents.Event
			for ev := range w.Events() {
				gotEvents = append(gotEvents, ev)
			}
			if diff := cmp.Diff(gotEvents, tc.want); diff != "" {
				t.Errorf("got events != want events: diff:\n%v", diff)
			}
		})
	}
}

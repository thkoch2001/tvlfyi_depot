package main

import (
	"context"
	"crypto/tls"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/signal"
	"strings"
	"time"

	"code.tvl.fyi/fun/clbot/backoffutil"
	"code.tvl.fyi/fun/clbot/gerrit"
	"code.tvl.fyi/fun/clbot/gerrit/gerritevents"
	log "github.com/golang/glog"
	"golang.org/x/crypto/ssh"
	"gopkg.in/irc.v3"
)

var (
	gerritAddr       = flag.String("gerrit_host", "cl.tvl.fyi:29418", "Gerrit SSH host:port")
	gerritSSHHostKey = flag.String("gerrit_ssh_pubkey", "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIUNYBYPCCBNDFSd0BuCR+8kgeuJ7IA5S2nTNQmkQUYNyXK+ot5os7rHtCk96+grd5+J8jFCuFBWisUe8h8NC0Q=", "Gerrit SSH public key")
	gerritSSHTimeout = flag.Duration("gerrit_tcp_timeout", 5*time.Second, "Gerrit SSH TCP connect timeout")

	gerritAuthUsername = flag.String("gerrit_ssh_auth_username", "", "Gerrit SSH username")
	gerritAuthKeyPath  = flag.String("gerrit_ssh_auth_key", "", "Gerrit SSH private key path")

	ircServer    = flag.String("irc_server", "chat.freenode.net:7000", "IRC server to connect to")
	ircNick      = flag.String("irc_nick", "clbot", "Nick to use when connecting to IRC")
	ircUser      = flag.String("irc_user", "clbot", "User string to use for IRC")
	ircName      = flag.String("irc_name", "clbot", "Name string to use for IRC")
	ircChannel   = flag.String("irc_channel", "##tvl", "Channel to send messages to")
	ircPassword  = flag.String("irc_pass", "", "Password to use for IRC")
	ircSendLimit = flag.Duration("irc_send_limit", 100*time.Millisecond, "Delay between messages")
	ircSendBurst = flag.Int("irc_send_burst", 10, "Number of messages which can be sent in a burst")
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

var shutdownFuncs []func()

func callOnShutdown(f func()) {
	shutdownFuncs = append(shutdownFuncs, f)
}

const ignoreBotChar = "\xe2\x80\x8b"

func runIRC(ctx context.Context, ircCfg irc.ClientConfig, sendMsg <-chan string) {
	bo := backoffutil.NewDefaultBackOff()
	ircCfg.Handler = irc.HandlerFunc(func(c *irc.Client, m *irc.Message) {
		if m.Command == "NOTICE" && m.Prefix.Name == "NickServ" && strings.Contains(m.Trailing(), "dentified") {
			// We're probably identified now, go join the channel.
			c.Writef("JOIN %s", *ircChannel)
		}
	})
	for {
		timer := time.NewTimer(bo.NextBackOff())
		select {
		case <-ctx.Done():
			timer.Stop()
			return
		case <-timer.C:
			break
		}

		(func() {
			connectedStart := time.Now()
			ircConn, err := tls.Dial("tcp", *ircServer, nil)
			if err != nil {
				log.Errorf("connecting to IRC at tcp/%s: %v", *ircServer, err)
				return
			}
			ircClient := irc.NewClient(ircConn, ircCfg)
			ircClientCtx, cancel := context.WithCancel(ctx)
			defer cancel()
			go func() {
				for {
					select {
					case <-ircClientCtx.Done():
						return
					case msg := <-sendMsg:
						log.Infof("sending message %q to %v", msg, *ircChannel)
						ircClient.Writef("PRIVMSG %s :%s%s", *ircChannel, ignoreBotChar, msg)
					}
				}
			}()
			log.Infof("connecting to IRC on tcp/%s", *ircServer)
			if err := ircClient.RunContext(ircClientCtx); err != nil {
				connectedEnd := time.Now()
				connectedFor := connectedEnd.Sub(connectedStart)
				if connectedFor > 60*time.Second {
					bo.Reset()
				}
				log.Errorf("IRC RunContext: %v", err)
				return
			}
		})()
	}
}

func username(p gerritevents.PatchSet) string {
	options := []string{
		p.Uploader.Username,
		p.Uploader.Name,
		p.Uploader.Email,
	}
	for _, opt := range options {
		if opt != "" {
			return opt
		}
	}
	return "UNKNOWN USER"
}

func patchSetURL(c gerritevents.Change, p gerritevents.PatchSet) string {
	return fmt.Sprintf("https://cl.tvl.fyi/%d", c.Number)
}

func main() {
	flag.Parse()
	failed := false
	if *gerritAuthUsername == "" {
		log.Errorf("gerrit_ssh_auth_username must be set")
		failed = true
	}
	if *gerritAuthKeyPath == "" {
		log.Errorf("gerrit_ssh_auth_key must be set")
		failed = true
	}
	if failed {
		os.Exit(2)
	}

	shutdownCh := make(chan os.Signal)
	signal.Notify(shutdownCh, os.Interrupt)
	go func() {
		<-shutdownCh
		signal.Reset(os.Interrupt)
		for n := len(shutdownFuncs) - 1; n >= 0; n-- {
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
		log.Exitf("gerrit.New(%q): %v", *gerritAddr, err)
	}
	callOnShutdown(func() {
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer cancel()
		gw.Close(ctx)
	})

	sendMsgChan := make(chan string, 5)
	go func() {
		for e := range gw.Events() {
			var parsedMsg string
			switch e := e.(type) {
			case *gerritevents.PatchSetCreated:
				if e.Change.Project != "depot" || e.Change.Branch != "master" || e.PatchSet.Number != 1 {
					continue
				}
				parsedMsg = fmt.Sprintf("CL/%d: %q proposed by %s - %s", e.Change.Number, e.Change.Subject, username(e.PatchSet), patchSetURL(e.Change, e.PatchSet))
			case *gerritevents.ChangeMerged:
				if e.Change.Project != "depot" || e.Change.Branch != "master" {
					continue
				}
				parsedMsg = fmt.Sprintf("CL/%d: %q submitted by %s - %s", e.Change.Number, e.Change.Subject, username(e.PatchSet), patchSetURL(e.Change, e.PatchSet))
			}
			if parsedMsg != "" {
				sendMsgChan <- parsedMsg
			}
		}
	}()

	ircCtx, ircCancel := context.WithCancel(ctx)
	callOnShutdown(ircCancel)
	go runIRC(ircCtx, irc.ClientConfig{
		Nick: *ircNick,
		User: *ircUser,
		Name: *ircName,
		Pass: *ircPassword,

		SendLimit: *ircSendLimit,
		SendBurst: *ircSendBurst,
	}, sendMsgChan)

	<-ctx.Done()
}

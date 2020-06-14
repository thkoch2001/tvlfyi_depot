// Package gerrit implements a watcher for Gerrit events.
package gerrit

import (
	"context"
	"errors"
	"fmt"
	"net"
	"strings"
	"time"

	"code.tvl.fyi/fun/clbot/backoffutil"
	"code.tvl.fyi/fun/clbot/gerrit/gerritevents"
	log "github.com/golang/glog"
	"golang.org/x/crypto/ssh"
)

// closer provides an embeddable implementation of Close which awaits a main loop acknowledging it has stopped.
type closer struct {
	stop    chan struct{}
	stopped chan struct{}
}

// newCloser returns a closer with the channels initialised.
func newCloser() closer {
	return closer{
		stop:    make(chan struct{}),
		stopped: make(chan struct{}),
	}
}

// Close stops the main loop, waiting for the main loop to stop until it stops or the context is cancelled, whichever happens first.
func (c *closer) Close(ctx context.Context) error {
	select {
	case <-c.stopped:
		return nil
	case <-c.stop:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	default:
	}
	close(c.stop)
	select {
	case <-c.stopped:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// lineWriter is an io.Writer which splits on \n and outputs each line (with no trailing newline) to its output channel.
type lineWriter struct {
	buf string
	out chan string
}

// Write accepts a slice of bytes containing zero or more new lines.
// If the contained channel is non-buffering or is full, this will block.
func (w *lineWriter) Write(p []byte) (n int, err error) {
	w.buf += string(p)
	pieces := strings.Split(w.buf, "\n")
	w.buf = pieces[len(pieces)-1]
	for n := 0; n < len(pieces)-1; n++ {
		w.out <- pieces[n]
	}
	return len(p), nil
}

// restartingClient is a simple SSH client that repeatedly connects to an SSH server, runs a command, and outputs the lines output by it on stdout onto a channel.
type restartingClient struct {
	closer

	network string
	addr    string
	cfg     *ssh.ClientConfig

	exec     string
	output   chan string
	shutdown func()
}

var (
	errStopConnect = errors.New("gerrit: told to stop reconnecting by remote server")
)

func (c *restartingClient) runOnce() error {
	netConn, err := net.Dial(c.network, c.addr)
	if err != nil {
		return fmt.Errorf("connecting to %v/%v: %w", c.network, c.addr, err)
	}
	defer netConn.Close()

	sshConn, newCh, newReq, err := ssh.NewClientConn(netConn, c.addr, c.cfg)
	if err != nil {
		return fmt.Errorf("creating SSH connection to %v/%v: %w", c.network, c.addr, err)
	}
	defer sshConn.Close()

	goAway := false
	passedThroughReqs := make(chan *ssh.Request)
	go func() {
		defer close(passedThroughReqs)
		for req := range newReq {
			if req.Type == "goaway" {
				goAway = true
				log.Warningf("remote end %v/%v told me to go away!", c.network, c.addr)
				sshConn.Close()
				netConn.Close()
			}
			passedThroughReqs <- req
		}
	}()

	cl := ssh.NewClient(sshConn, newCh, passedThroughReqs)

	sess, err := cl.NewSession()
	if err != nil {
		return fmt.Errorf("NewSession on %v/%v: %w", c.network, c.addr, err)
	}
	defer sess.Close()

	sess.Stdout = &lineWriter{out: c.output}

	if err := sess.Start(c.exec); err != nil {
		return fmt.Errorf("Start(%q) on %v/%v: %w", c.exec, c.network, c.addr, err)
	}

	log.Infof("connected to %v/%v", c.network, c.addr)

	done := make(chan struct{})
	go func() {
		sess.Wait()
		close(done)
	}()
	go func() {
		select {
		case <-c.stop:
			sess.Close()
		case <-done:
		}
		return
	}()
	<-done

	if goAway {
		return errStopConnect
	}
	return nil
}

func (c *restartingClient) run() {
	defer close(c.stopped)
	bo := backoffutil.NewDefaultBackOff()
	for {
		timer := time.NewTimer(bo.NextBackOff())
		select {
		case <-c.stop:
			timer.Stop()
			return
		case <-timer.C:
			break
		}
		if err := c.runOnce(); err == errStopConnect {
			if c.shutdown != nil {
				c.shutdown()
				return
			}
		} else if err != nil {
			log.Errorf("SSH: %v", err)
		} else {
			bo.Reset()
		}
	}
}

// Output returns the channel on which each newline-delimited string output by the executed command's stdout can be received.
func (c *restartingClient) Output() <-chan string {
	return c.output
}

// dialRestartingClient creates a new restartingClient.
func dialRestartingClient(network, addr string, config *ssh.ClientConfig, exec string, shutdown func()) (*restartingClient, error) {
	c := &restartingClient{
		closer:   newCloser(),
		network:  network,
		addr:     addr,
		cfg:      config,
		exec:     exec,
		output:   make(chan string),
		shutdown: shutdown,
	}
	go c.run()
	return c, nil
}

// Watcher watches
type Watcher struct {
	closer
	c *restartingClient

	output chan gerritevents.Event
}

// Close shuts down the SSH client connection, if any, and closes the output channel.
// It blocks until shutdown is complete or until the context is cancelled, whichever comes first.
func (w *Watcher) Close(ctx context.Context) {
	w.c.Close(ctx)
	w.closer.Close(ctx)
}

func (w *Watcher) run() {
	defer close(w.stopped)
	defer close(w.output)
	for {
		select {
		case <-w.stop:
			return
		case o := <-w.c.Output():
			ev, err := gerritevents.Parse([]byte(o))
			if err != nil {
				log.Errorf("failed to parse event %v: %v", o, err)
				continue
			}
			w.output <- ev
		}
	}
}

// Events returns the channel upon which parsed Gerrit events can be received.
func (w *Watcher) Events() <-chan gerritevents.Event {
	return w.output
}

// New returns a running Watcher from which events can be read.
// It will begin connecting to the provided address immediately.
func New(ctx context.Context, network, addr string, cfg *ssh.ClientConfig) (*Watcher, error) {
	wc := newCloser()
	rc, err := dialRestartingClient(network, addr, cfg, "gerrit stream-events", func() {
		wc.Close(context.Background())
	})
	if err != nil {
		return nil, fmt.Errorf("dialRestartingClient: %w", err)
	}
	w := &Watcher{
		closer: wc,
		c:      rc,
		output: make(chan gerritevents.Event),
	}
	go w.run()
	return w, nil
}

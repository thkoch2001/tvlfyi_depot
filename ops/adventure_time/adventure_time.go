package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync"
	"time"

	"github.com/davecgh/go-spew/spew"
	"go.lsp.dev/jsonrpc2"
	"go.lsp.dev/protocol"
	"go.uber.org/zap"
)

var (
	logger   *zap.SugaredLogger
	logRawIO = flag.Bool("log_raw_io", false, "If true, logs all messages sent to and received from child LSP.")
)

type client struct {
	Diagnostics []*protocol.PublishDiagnosticsParams
}

var _ protocol.ClientInterface = (*client)(nil)

func (client) Run(ctx context.Context) error {
	logger.Info("RUN")
	return nil
}
func (client) LogMessage(ctx context.Context, params *protocol.LogMessageParams) error {
	switch params.Type {
	case protocol.Error:
		logger.Error(params.Message)
	case protocol.Warning:
		logger.Warn(params.Message)
	case protocol.Info:
		logger.Info(params.Message)
	case protocol.Log:
		logger.Info(params.Message)
	}
	return nil
}

func (c *client) PublishDiagnostics(ctx context.Context, params *protocol.PublishDiagnosticsParams) (err error) {
	c.Diagnostics = append(c.Diagnostics, params)
	return nil
}
func (client) ShowMessage(ctx context.Context, params *protocol.ShowMessageParams) (err error) {
	return nil
}
func (client) ShowMessageRequest(ctx context.Context, params *protocol.ShowMessageRequestParams) (result *protocol.MessageActionItem, err error) {
	return nil, nil
}
func (client) Telemetry(ctx context.Context, params interface{}) (err error) { return nil }
func (client) RegisterCapability(ctx context.Context, params *protocol.RegistrationParams) (err error) {
	return nil
}
func (client) UnregisterCapability(ctx context.Context, params *protocol.UnregistrationParams) (err error) {
	return nil
}
func (client) WorkspaceApplyEdit(ctx context.Context, params *protocol.ApplyWorkspaceEditParams) (result bool, err error) {
	return false, nil
}
func (client) WorkspaceConfiguration(ctx context.Context, params *protocol.ConfigurationParams) (result []interface{}, err error) {
	return nil, nil
}
func (client) WorkspaceFolders(ctx context.Context) (result []protocol.WorkspaceFolder, err error) {
	return nil, nil
}

type progressHandler struct {
	progress    map[string]struct{}
	workCounter chan int
	mu          sync.Mutex
	counter     int
}

type progressNotify struct {
	Done bool   `json:"done"`
	ID   string `json:"id"`
}

func (p *progressHandler) Deliver(ctx context.Context, r *jsonrpc2.Request, delivered bool) bool {
	if r.Method == "window/progress" {
		var pn progressNotify
		if err := json.Unmarshal(*r.Params, &pn); err != nil {
			r.Reply(ctx, nil, err)
			return true
		}
		if !pn.Done {
			if _, ok := p.progress[pn.ID]; !ok {
				p.progress[pn.ID] = struct{}{}
				p.mu.Lock()
				p.counter++
				p.workCounter <- p.counter
				p.mu.Unlock()
			}
		} else {
			if _, ok := p.progress[pn.ID]; ok {
				delete(p.progress, pn.ID)
				p.mu.Lock()
				p.counter--
				p.workCounter <- p.counter
				p.mu.Unlock()
			}
		}

		return true
	}
	return false
}
func (progressHandler) Cancel(ctx context.Context, conn *jsonrpc2.Conn, id jsonrpc2.ID, canceled bool) bool {
	return false
}
func (progressHandler) Request(ctx context.Context, conn *jsonrpc2.Conn, direction jsonrpc2.Direction, r *jsonrpc2.WireRequest) context.Context {
	return ctx
}
func (progressHandler) Response(ctx context.Context, conn *jsonrpc2.Conn, direction jsonrpc2.Direction, r *jsonrpc2.WireResponse) context.Context {
	return ctx
}
func (progressHandler) Done(ctx context.Context, err error)                {}
func (progressHandler) Read(ctx context.Context, n int64) context.Context  { return ctx }
func (progressHandler) Write(ctx context.Context, n int64) context.Context { return ctx }
func (progressHandler) Error(ctx context.Context, err error)               {}

type logWriter struct {
	io.Writer
}

func isInteresting(ps string) bool {
	switch {
	case strings.HasPrefix(ps, "Content-Length:"):
		return false
	case strings.TrimSpace(ps) == "":
		return false
	}
	return true
}

func (w *logWriter) Write(p []byte) (n int, err error) {
	if *logRawIO {
		ps := strings.Split(strings.TrimSpace(string(p)), "\r\n")
		for _, ln := range ps {
			if isInteresting(ln) {
				logger.Infof("[->] %v", ln)
			}
		}
	}
	return w.Writer.Write(p)
}

type logReader struct {
	io.Reader
}

func (r *logReader) Read(p []byte) (n int, err error) {
	n, err = r.Reader.Read(p)
	if err != nil {
		return n, err
	}
	if *logRawIO {
		ps := strings.Split(strings.TrimSpace(string(p[:n])), "\r\n")
		for _, ln := range ps {
			if isInteresting(ln) {
				logger.Infof("[<-] %v", ln)
			}
		}
	}
	return n, err
}

// launchLSP launches a lumpy space princess.
func launchLSP(ctx context.Context, logger *zap.Logger, client protocol.ClientInterface, cmd []string) (*jsonrpc2.Conn, protocol.ServerInterface, error) {
	c := exec.CommandContext(ctx, cmd[0], cmd[1:]...)
	c.Stderr = os.Stderr
	stdin, err := c.StdinPipe()
	if err != nil {
		return nil, nil, fmt.Errorf("getting stdin pipe: %w", err)
	}
	stdout, err := c.StdoutPipe()
	if err != nil {
		return nil, nil, fmt.Errorf("getting stdout pipe: %w", err)
	}
	if err := c.Start(); err != nil {
		return nil, nil, fmt.Errorf("launching %v: %w", cmd[0], err)
	}
	st := jsonrpc2.NewStream(&logReader{stdout}, &logWriter{stdin})
	_, conn, server := protocol.NewClient(ctx, client, st, logger)
	return conn, server, nil
}

type InitializeParams map[string]interface{}

func main() {
	dlogger, err := zap.NewDevelopment()
	if err != nil {
		fmt.Fprintf(os.Stderr, "initializing logger: %v\n", err)
		os.Exit(1)
	}
	logger = dlogger.Sugar()
	defer logger.Sync()

	flag.Parse()
	if flag.NArg() < 1 {
		logger.Fatalf("need command line to LSP to run")
	}

	ctx := context.Background()
	cl := &client{}
	conn, lspClient, err := launchLSP(ctx, logger.Desugar(), cl, flag.Args())
	if err != nil {
		logger.Fatalf("launching lumpy space princess: %v", err)
	}
	workOutstanding := make(chan int)
	ph := &progressHandler{
		workCounter: workOutstanding,
		progress:    make(map[string]struct{}),
	}
	conn.AddHandler(ph)
	go func() {
		_ = conn
		if err := conn.Run(ctx); err != nil {
			logger.Fatalf("conn.Run: %v", err)
		}
	}()

	_, err = lspClient.Initialize(ctx, &protocol.InitializeParams{
		ProcessID: float64(os.Getpid()),
		RootURI:   "file:///home/lukegb/Projects/tvldepot/tools/cheddar",
		Capabilities: protocol.ClientCapabilities{
			TextDocument: &protocol.TextDocumentClientCapabilities{
				PublishDiagnostics: &protocol.TextDocumentClientCapabilitiesPublishDiagnostics{
					RelatedInformation: true,
					TagSupport:         true,
				},
			},
		},
		InitializationOptions: InitializeParams{
			"settings": InitializeParams{
				"rust": InitializeParams{
					"clippy_preference": "on",
				},
			},
		},
	})
	if err != nil {
		logger.Fatalf("Initialize: %v", err)
	}

	const waitForWork = 1 * time.Second
	idleTimer := time.NewTimer(waitForWork)
	currentWork := 0
waitSettled:
	for {
		select {
		case <-idleTimer.C:
			logger.Info("Done waiting for diagnostics")
			break waitSettled
		case n := <-workOutstanding:
			logger.Infof("LSP reports %d things happening", n)
			if n == 0 && currentWork != 0 {
				idleTimer.Reset(waitForWork)
			} else if n != 0 && currentWork == 0 {
				if !idleTimer.Stop() {
					<-idleTimer.C
				}
			}
			currentWork = n
		}
	}

	if err := lspClient.Shutdown(ctx); err != nil {
		logger.Fatalf("Shutdown: %v", err)
	}
	if err := lspClient.Exit(ctx); err != nil {
		logger.Fatalf("Exit: %v", err)
	}

	logger.Infof("Diagnostics: %s", spew.Sdump(cl.Diagnostics))
}

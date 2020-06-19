package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"go.lsp.dev/jsonrpc2"
	"go.lsp.dev/protocol"
	"go.lsp.dev/uri"
	"go.uber.org/zap"
)

var (
	logger           *zap.SugaredLogger
	logRawIO         = flag.Bool("log_raw_io", false, "If true, logs all messages sent to and received from child LSP.")
	lspWorkers       = flag.Int("lsp_workers", 10, "Number of concurrent LSP workers to run.")
	lspAwaitActivity = flag.Duration("lsp_await_activity", 10*time.Second, "Period of inactivity after which to stop waiting for new results.")
	lspTimeout       = flag.Duration("lsp_timeout", 5*time.Minute, "Timeout on awaiting LSP results.")
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
	} else {
		p.mu.Lock()
		p.workCounter <- p.counter
		p.mu.Unlock()
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

type CommentRange struct {
	StartLine      int `json:"start_line"`
	StartCharacter int `json:"start_character"`

	EndLine      int `json:"end_line"`
	EndCharacter int `json:"end_character"`
}

type RobotCommentInput struct {
	RobotID    string `json:"robot_id"`
	RobotRunID string `json:"robot_run_id"`

	Path    string
	Line    *int          `json:"line,omitempty"`
	Range   *CommentRange `json:"range,omitempty"`
	Message string        `json:"message"`
}

type ReviewInput struct {
	RobotComments map[string][]RobotCommentInput `json:"robot_comments"`
}

func Int(x int) *int { return &x }

type LSPConfiguration struct {
	Enabled            bool
	Command            []string
	RootFiles          []string
	FileExtensions     []string
	SupportsWorkspaces bool
}

type dirStruct struct {
	Children   map[string]*dirStruct
	RootFor    map[string]bool
	HasType    map[string]bool
	HasSubRoot bool
	ChildFiles map[string][]string
	IsFile     bool
}

func newDirStruct() *dirStruct {
	return &dirStruct{
		Children:   map[string]*dirStruct{},
		RootFor:    map[string]bool{},
		HasType:    map[string]bool{},
		ChildFiles: map[string][]string{},
	}
}

func (ds *dirStruct) getBits(path []string) *dirStruct {
	if len(path) == 0 {
		return ds
	}
	if nextChild := ds.Children[path[0]]; nextChild == nil {
		ds.Children[path[0]] = newDirStruct()
	}
	return ds.Children[path[0]].getBits(path[1:])
}

func (ds *dirStruct) Get(path string) *dirStruct {
	if path == "." {
		return ds
	}
	return ds.getBits(strings.Split(filepath.ToSlash(path), "/"))
}

func (ds *dirStruct) FlattenHasType() {
	var removeChildren []string
	for c, cds := range ds.Children {
		cds.FlattenHasType()
		if cds.HasSubRoot {
			ds.HasSubRoot = true
		}
		needsRoot := map[string]bool{}
		for ht := range cds.HasType {
			needsRoot[ht] = true
		}
		for rf := range cds.RootFor {
			delete(needsRoot, rf)
			ds.HasSubRoot = true
		}
		for rf := range needsRoot {
			ds.HasType[rf] = true
		}
		if !cds.HasSubRoot && len(cds.RootFor) == 0 {
			removeChildren = append(removeChildren, c)
		}

		if len(needsRoot) > 0 {
			if cds.IsFile {
				for ht := range cds.HasType {
					ds.ChildFiles[ht] = append(ds.ChildFiles[ht], c)
				}
			}
			for ht, cfs := range cds.ChildFiles {
				for _, cf := range cfs {
					ds.ChildFiles[ht] = append(ds.ChildFiles[ht], filepath.Join(c, cf))
				}
			}
			cds.ChildFiles = nil
		}
	}
	for _, c := range removeChildren {
		delete(ds.Children, c)
	}
}

type Workspace struct {
	Path  string
	Files []string
}

func (ds *dirStruct) Roots(path string, m map[string][]Workspace) {
	for ht, cfs := range ds.ChildFiles {
		m[ht] = append(m[ht], Workspace{
			Path:  path,
			Files: cfs,
		})
	}
	for n, cds := range ds.Children {
		cds.Roots(filepath.Join(path, n), m)
	}
}

func FindLSPRoots(root string, lsps map[string]LSPConfiguration) (map[string][]Workspace, error) {
	rootFiles := map[string][]string{}
	fileExts := map[string][]string{}
	for lspName, lsp := range lsps {
		for _, f := range lsp.RootFiles {
			rootFiles[f] = append(rootFiles[f], lspName)
		}
		for _, f := range lsp.FileExtensions {
			fileExts[f] = append(fileExts[f], lspName)
		}
	}
	rootDS := newDirStruct()
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		path, err = filepath.Rel(root, path)
		if err != nil {
			return err
		}

		if rs := rootFiles[info.Name()]; len(rs) > 0 {
			ds := rootDS.Get(filepath.Dir(path))
			for _, r := range rs {
				ds.RootFor[r] = true
			}
		}
		if fes := fileExts[filepath.Ext(info.Name())]; len(fes) > 0 {
			ds := rootDS.Get(path)
			ds.IsFile = !info.IsDir()
			for _, fe := range fes {
				ds.HasType[fe] = true
			}
		}

		if info.Name()[0] == '.' && info.IsDir() {
			return filepath.SkipDir
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	rootDS.FlattenHasType()
	m := map[string][]Workspace{}
	rootDS.Roots("", m)
	return m, nil
}

func runLSPForWorkspaces(ctx context.Context, logger *zap.SugaredLogger, diagnosticPrefix, depotRoot string, cfg LSPConfiguration, workspaces []Workspace) ([]*protocol.PublishDiagnosticsParams, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	cl := &client{}
	conn, lspClient, err := launchLSP(ctx, logger.Desugar(), cl, cfg.Command)
	if err != nil {
		logger.Fatalf("launching lumpy space princess: %v", err)
	}
	workOutstanding := make(chan int)
	ph := &progressHandler{
		workCounter: workOutstanding,
		progress:    make(map[string]struct{}),
	}
	conn.AddHandler(ph)
	go conn.Run(ctx)

	root := uri.File(depotRoot)
	if len(workspaces) == 1 {
		root = uri.File(filepath.Join(depotRoot, workspaces[0].Path))
	}

	var wsFolders []protocol.WorkspaceFolder
	for n, ws := range workspaces {
		wsFolders = append(wsFolders, protocol.WorkspaceFolder{
			URI:  string(uri.File(filepath.Join(depotRoot, ws.Path))),
			Name: fmt.Sprintf("workspace-%d", n),
		})
	}

	_, err = lspClient.Initialize(ctx, &protocol.InitializeParams{
		ProcessID: float64(os.Getpid()),
		RootURI:   root,
		Capabilities: protocol.ClientCapabilities{
			TextDocument: &protocol.TextDocumentClientCapabilities{
				PublishDiagnostics: &protocol.TextDocumentClientCapabilitiesPublishDiagnostics{
					RelatedInformation: true,
				},
			},
			Workspace: &protocol.WorkspaceClientCapabilities{
				WorkspaceFolders: true,
			},
			Window: &protocol.WindowClientCapabilities{
				WorkDoneProgress: true,
			},
		},
		InitializationOptions: InitializeParams{
			"settings": InitializeParams{
				"rust": InitializeParams{
					"clippy_preference": "on",
				},
			},
		},
		WorkspaceFolders: wsFolders,
	})
	if err != nil {
		logger.Fatalf("Initialize: %v", err)
	}

	if err := lspClient.Initialized(ctx, &protocol.InitializedParams{}); err != nil {
		logger.Fatalf("Initialized: %v", err)
	}

	for _, ws := range workspaces {
		for _, f := range ws.Files {
			fc, err := ioutil.ReadFile(filepath.Join(depotRoot, ws.Path, f))
			if err != nil {
				logger.Errorf("ReadFile(%v/%v/%v): %v", depotRoot, ws.Path, f, err)
				continue
			}
			err = lspClient.DidOpen(ctx, &protocol.DidOpenTextDocumentParams{
				TextDocument: protocol.TextDocumentItem{
					URI:     uri.File(filepath.Join(depotRoot, ws.Path, f)),
					Version: 1,
					Text:    string(fc),
				},
			})
			if err != nil {
				logger.Errorf("DidOpen(%v/%v): %v", ws.Path, f, err)
			}
		}
	}

	idleTimer := time.NewTimer(*lspAwaitActivity)
	currentWork := 0
waitSettled:
	for {
		select {
		case <-idleTimer.C:
			logger.Info("Done waiting for diagnostics")
			break waitSettled
		case n := <-workOutstanding:
			if n >= 0 {
				logger.Infof("LSP reports %d things happening", n)
			} else if n < 0 {
				logger.Infof("LSP still active...")
			}
			if n <= 0 {
				idleTimer.Reset(*lspAwaitActivity)
			} else if n != 0 && currentWork == 0 {
				if !idleTimer.Stop() {
					<-idleTimer.C
				}
			}
			currentWork = n
		}
	}

	for n, dh := range cl.Diagnostics {
		for n, d := range dh.Diagnostics {
			if d.Source != "" {
				d.Source = fmt.Sprintf("%s:%s", diagnosticPrefix, d.Source)
			} else {
				d.Source = diagnosticPrefix
			}
			dh.Diagnostics[n] = d
		}
		cl.Diagnostics[n] = dh
	}

	return cl.Diagnostics, nil
}

var config = map[string]LSPConfiguration{"go": {
	Enabled:            true,
	Command:            []string{"/home/lukegb/go/bin/gopls", "serve", "--debug", ":12315"},
	FileExtensions:     []string{".go"},
	RootFiles:          []string{".git", ".hg", "go.mod", "go.sum"},
	SupportsWorkspaces: true,
}, "rust": {
	Enabled:            true,
	Command:            []string{"/nix/store/pmy43h3i8wccivxc27avx0pixdr7wvpj-rustup-1.21.1/bin/rls"},
	FileExtensions:     []string{".rs"},
	RootFiles:          []string{"Cargo.toml"},
	SupportsWorkspaces: false,
}}

func runLSPsForWorkspaces(ctx context.Context, root string, workspaces map[string][]Workspace) ([]*protocol.PublishDiagnosticsParams, error) {
	// Populate work queue with work.
	var work [](func(ctx context.Context) ([]*protocol.PublishDiagnosticsParams, error))
	for wsLang, wss := range workspaces {
		lsp := config[wsLang]
		if !lsp.Enabled {
			continue
		}
		if lsp.SupportsWorkspaces {
			wss := wss
			work = append(work, func(ctx context.Context) ([]*protocol.PublishDiagnosticsParams, error) {
				return runLSPForWorkspaces(ctx, logger.Named(wsLang), wsLang, root, lsp, wss)
			})
		} else {
			for _, ws := range wss {
				ws := ws
				work = append(work, func(ctx context.Context) ([]*protocol.PublishDiagnosticsParams, error) {
					return runLSPForWorkspaces(ctx, logger.Named(wsLang).Named(ws.Path), wsLang, root, lsp, []Workspace{ws})
				})
			}
		}
	}

	type result struct {
		res []*protocol.PublishDiagnosticsParams
		err error
	}
	resultCh := make(chan result)
	workCh := make(chan (func(ctx context.Context) ([]*protocol.PublishDiagnosticsParams, error)))
	var wg sync.WaitGroup
	go func() {
		defer close(workCh)
		for _, w := range work {
			select {
			case workCh <- w:
			case <-ctx.Done():
				return
			}
		}
	}()
	for n := 0; n < *lspWorkers; n++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			pctx := ctx
			for work := range workCh {
				ctx, cancel := context.WithTimeout(pctx, *lspTimeout)
				diagnostics, err := work(ctx)
				resultCh <- result{res: diagnostics, err: err}
				cancel()
			}
		}()
	}
	go func() {
		wg.Wait()
		close(resultCh)
	}()
	var diagnostics []*protocol.PublishDiagnosticsParams
	for res := range resultCh {
		if res.err != nil {
			logger.Errorf("getting diagnostics: %v", res.err)
			continue
		}
		diagnostics = append(diagnostics, res.res...)
	}
	return diagnostics, nil
}

func main() {
	dlogger, err := zap.NewDevelopment()
	if err != nil {
		fmt.Fprintf(os.Stderr, "initializing logger: %v\n", err)
		os.Exit(1)
	}
	logger = dlogger.Sugar()
	defer logger.Sync()

	root := "/home/lukegb/Projects/tvldepot"
	workspaces, err := FindLSPRoots(root, config)
	if err != nil {
		logger.Fatalf("FindLSPRoots: %v", err)
	}

	flag.Parse()

	ctx := context.Background()

	diagnostics, err := runLSPsForWorkspaces(ctx, root, workspaces)
	if err != nil {
		logger.Fatalf("runLSPsForWorkspaces: %v", err)
	}

	ri := ReviewInput{
		RobotComments: make(map[string][]RobotCommentInput),
	}
	for _, dh := range diagnostics {
		doc := uri.URI(dh.URI).Filename()
		if !strings.HasPrefix(string(doc), string(root)) {
			logger.Warnf("Diagnostic received for %q, which isn't under %q", doc, root)
			continue
		}
		docPath, err := filepath.Rel(root, doc)
		if err != nil {
			logger.Errorf("Rel(%q, %q): %v", root, doc, err)
			continue
		}
		for _, d := range dh.Diagnostics {
			rci := RobotCommentInput{
				RobotID: d.Source,
				Message: d.Message,
				Line:    Int(int(d.Range.End.Line) + 1),
				Range: &CommentRange{
					StartLine:      int(d.Range.Start.Line) + 1,
					StartCharacter: int(d.Range.Start.Character),

					EndLine:      int(d.Range.End.Line) + 1,
					EndCharacter: int(d.Range.End.Character),
				},
			}

			ri.RobotComments[docPath] = append(ri.RobotComments[docPath], rci)
		}
	}
	if err := json.NewEncoder(os.Stdout).Encode(ri); err != nil {
		logger.Fatalf("json encoding: %v", err)
	}
}

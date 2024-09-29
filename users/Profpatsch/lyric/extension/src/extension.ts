import * as vscode from 'vscode';
import * as net from 'net';

export function activate(context: vscode.ExtensionContext) {
  context.subscriptions.push(...registerCheckLineTimestamp(context));
  context.subscriptions.push(
    vscode.commands.registerCommand('extension.jumpToLrcPosition', jumpToLrcPosition),
  );
}

function jumpToLrcPosition() {
  const editor = vscode.window.activeTextEditor;

  if (!editor) {
    vscode.window.showInformationMessage('No active editor found.');
    return;
  }

  const ext = new Ext(editor);
  const position = editor.selection.active;
  const res = ext.getTimestampFromLine(position);

  if (!res) {
    return;
  }
  const { milliseconds, seconds } = res;

  // Prepare JSON command to send to the socket
  const jsonCommand = {
    command: ['seek', seconds, 'absolute'],
  };

  const socket = new net.Socket();

  const socketPath = process.env.HOME + '/tmp/mpv-socket';
  socket.connect(socketPath, () => {
    socket.write(JSON.stringify(jsonCommand));
    socket.write('\n');
    vscode.window.showInformationMessage(
      `Sent command to jump to [${formatTimestamp(milliseconds)}].`,
    );
    socket.end();
  });

  socket.on('error', err => {
    vscode.window.showErrorMessage(`Failed to send command: ${err.message}`);
  });
}

// If the difference to the timestamp on the next line is larger than 10 seconds, underline the next line and show a warning message on hover
export function registerCheckLineTimestamp(_context: vscode.ExtensionContext) {
  const changesToCheck: Set<vscode.TextDocument> = new Set();
  const everSeen = new Set<vscode.TextDocument>();

  return [
    vscode.workspace.onDidChangeTextDocument(e => {
      changesToCheck.add(e.document);
      if (vscode.window.activeTextEditor?.document === e.document) {
        doEditorChecks(vscode.window.activeTextEditor, everSeen, changesToCheck);
      }
    }),
    vscode.workspace.onDidOpenTextDocument(e => {
      changesToCheck.add(e);
      everSeen.add(e);
      if (vscode.window.activeTextEditor?.document === e) {
        doEditorChecks(vscode.window.activeTextEditor, everSeen, changesToCheck);
      }
    }),

    vscode.window.onDidChangeActiveTextEditor(editor => {
      if (editor) {
        doEditorChecks(editor, everSeen, changesToCheck);
      }
    }),
    vscode.window.onDidChangeVisibleTextEditors(editors => {
      for (const editor of editors) {
        doEditorChecks(editor, everSeen, changesToCheck);
      }
    }),
  ];
}

function doEditorChecks(
  editor: vscode.TextEditor,
  everSeen: Set<vscode.TextDocument>,
  changesToCheck: Set<vscode.TextDocument>,
) {
  const ext = new Ext(editor);
  const document = editor.document;

  if (!everSeen.has(editor.document)) {
    changesToCheck.add(editor.document);
    everSeen.add(editor.document);
  }

  if (!changesToCheck.has(document)) {
    return;
  }
  changesToCheck.delete(document);

  const from = 0;
  const to = document.lineCount - 1;
  for (let line = from; line <= to; line++) {
    const warnings: string[] = [];
    const timeDiff = timeDifferenceTooLarge(ext, line);
    if (timeDiff !== undefined) {
      warnings.push(timeDiff);
    }
    const nextTimestampSmaller = nextLineTimestampSmallerThanCurrent(ext, line);
    if (nextTimestampSmaller !== undefined) {
      warnings.push(nextTimestampSmaller);
    }
    for (const warning of warnings) {
      global_manageWarnings.setWarning(document, line, warning);
    }
    // unset any warnings if this doesn’t apply anymore
    if (warnings.length === 0) {
      global_manageWarnings.setWarning(document, line);
    }
  }
}

/** Warn if the difference to the timestamp on the next line is larger than 10 seconds */
function timeDifferenceTooLarge(ext: Ext, line: number): string | undefined {
  const timeDifference = ext.getTimeDifferenceToNextLineTimestamp(
    new vscode.Position(line, 0),
  );
  if (
    !timeDifference ||
    timeDifference.thisLineIsEmpty ||
    timeDifference.difference <= 10000
  ) {
    return;
  }
  return `Time difference to next line is ${formatTimestamp(timeDifference.difference)}`;
}

/** Warn if the timestamp on the next line is smaller or equal to the current timestamp */
function nextLineTimestampSmallerThanCurrent(ext: Ext, line: number): string | undefined {
  const timeDifference = ext.getTimeDifferenceToNextLineTimestamp(
    new vscode.Position(line, 0),
  );
  if (!timeDifference) {
    return;
  }
  if (timeDifference.difference == 0) {
    return `The timestamp to the next line is not increasing`;
  }
  if (timeDifference.difference < 0) {
    return `The timestamp to the next line is decreasing`;
  }
}

class Ext {
  constructor(public editor: vscode.TextEditor) {}

  getTimeDifferenceToNextLineTimestamp(position: vscode.Position) {
    const thisLineTimestamp = this.getTimestampFromLine(position);
    const nextLineTimestamp = this.getTimestampFromLine(
      position.with({ line: position.line + 1 }),
    );
    if (!thisLineTimestamp || !nextLineTimestamp) {
      return;
    }
    return {
      difference: nextLineTimestamp.milliseconds - thisLineTimestamp.milliseconds,
      thisLineIsEmpty: thisLineTimestamp.text.trim() === '',
    };
  }

  getTimestampFromLine(position: vscode.Position) {
    const document = this.editor.document;
    const lineText = document.lineAt(position.line).text;

    // Extract timestamp [mm:ss.ms] from the current line
    const match = lineText.match(/\[(\d+:\d+\.\d+)\](.*)/);
    if (!match) {
      return;
    }
    const [, timestamp, text] = match!;
    const milliseconds = parseTimestamp(timestamp);
    const seconds = milliseconds / 1000;
    return { milliseconds, seconds, text };
  }
}

function parseTimestamp(timestamp: string): number {
  // Parse [mm:ss.ms] format into milliseconds
  const [min, sec] = timestamp.split(':');

  const minutes = parseInt(min, 10);
  const seconds = parseFloat(sec);

  return minutes * 60 * 1000 + seconds * 1000;
}

function formatTimestamp(ms: number): string {
  // Format milliseconds back into [mm:ss.ms]
  const minutes = Math.floor(ms / 60000);
  ms %= 60000;
  const seconds = (ms / 1000).toFixed(2);

  return `${String(minutes).padStart(2, '0')}:${seconds}`;
}

class ManageWarnings {
  private warnings: Map<number, string> = new Map();
  private diagnostics: vscode.DiagnosticCollection;

  constructor() {
    this.diagnostics = vscode.languages.createDiagnosticCollection();
  }

  /** Set a warning message on a line in a document, if null then unset */
  setWarning(document: vscode.TextDocument, line: number, message?: string) {
    if (message !== undefined) {
      this.warnings.set(line, message);
    } else {
      this.warnings.delete(line);
    }
    this.updateDiagnostics(document);
  }

  private updateDiagnostics(document: vscode.TextDocument) {
    const mkWarning = (line: number, message: string) => {
      const lineRange = document.lineAt(line).range;
      return new vscode.Diagnostic(lineRange, message, vscode.DiagnosticSeverity.Warning);
    };
    const diagnostics: vscode.Diagnostic[] = [];
    for (const [line, message] of this.warnings) {
      diagnostics.push(mkWarning(line, message));
    }
    this.diagnostics.delete(document.uri);
    this.diagnostics.set(document.uri, diagnostics);
  }
}

const global_manageWarnings = new ManageWarnings();

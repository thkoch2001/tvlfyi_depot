import * as vscode from 'vscode';
import * as net from 'net';
import { adjustTimestampToEighthNote, bpmToEighthNoteDuration } from './quantize-lrc';
import { start } from 'repl';

export function activate(context: vscode.ExtensionContext) {
  context.subscriptions.push(...registerCheckLineTimestamp(context));
  context.subscriptions.push(
    vscode.commands.registerCommand('extension.jumpToLrcPosition', jumpToLrcPosition),
    vscode.commands.registerCommand('extension.shiftLyricsDown', shiftLyricsDown),
    vscode.commands.registerCommand('extension.shiftLyricsUp', shiftLyricsUp),
    vscode.commands.registerCommand('extension.quantizeToEigthNote', quantizeLrc),
  );
}

/**
 * Jumps to the position in the lyric file corresponding to the current cursor position in the active text editor.
 * Sends a command to a socket to seek to the specified position in mpv at the socket path `~/tmp/mpv-socket`.
 * @remarks
 * This function requires the following dependencies:
 * - `vscode` module for accessing the active text editor and displaying messages.
 * - `net` module for creating a socket connection.
 * @throws {Error} If there is an error sending the command to the socket.
 */
function jumpToLrcPosition() {
  const editor = vscode.window.activeTextEditor;

  if (!editor) {
    vscode.window.showInformationMessage('No active editor found.');
    return;
  }

  const ext = new Ext(editor.document);
  const position = editor.selection.active;
  const res = ext.getTimestampFromLine(position.line);

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

/**
 * Shifts the lyrics down by one line starting from the current cursor position in the active text editor.
 * @remarks
 * This function requires the following dependencies:
 * - `vscode` module for accessing the active text editor and displaying messages.
 */
async function shiftLyricsDown() {
  const editor = vscode.window.activeTextEditor;

  if (!editor) {
    vscode.window.showInformationMessage('No active editor found.');
    return;
  }

  const ext = new Ext(editor.document);

  const getLine = (line: number) => ({
    number: line,
    range: editor.document.lineAt(line),
  });

  // get the document range from the beginning of the current line to the end of the file
  const documentRange = new vscode.Range(
    getLine(editor.selection.active.line).range.range.start,
    editor.document.lineAt(editor.document.lineCount - 1).range.end,
  );

  let newLines: string = '';
  // iterate through all lines under the current line, save the lyric text from the current line, and replace it with the lyric text from the previous line
  let previousLineText = '';
  for (
    // get the current line range
    let line = getLine(editor.selection.active.line);
    line.number < editor.document.lineCount - 1;
    // next line as position from line number
    line = getLine(line.number + 1)
  ) {
    const timestamp = ext.getTimestampFromLine(line.number);
    if (timestamp === undefined) {
      newLines += line.range.text + '\n';
      continue;
    }
    newLines += `[${formatTimestamp(timestamp.milliseconds)}]` + previousLineText + '\n';
    previousLineText = timestamp.text;
  }
  // replace documentRange with newLines
  await editor.edit(editBuilder => {
    editBuilder.replace(documentRange, newLines);
  });
}

/**
 * Shifts the lyrics up by one line starting from the current cursor position in the active text editor.
 * @remarks
 * This function requires the following dependencies:
 * - `vscode` module for accessing the active text editor and displaying messages.
 */
async function shiftLyricsUp() {
  const editor = vscode.window.activeTextEditor;

  if (!editor) {
    vscode.window.showInformationMessage('No active editor found.');
    return;
  }

  const ext = new Ext(editor.document);

  const getLine = (line: number) => ({
    number: line,
    range: editor.document.lineAt(line),
  });

  // get the document range from the beginning of the current line to the end of the file
  const documentRange = new vscode.Range(
    getLine(editor.selection.active.line).range.range.start,
    editor.document.lineAt(editor.document.lineCount - 1).range.end,
  );

  let newLines: string = '';
  // iterate through all lines under the current line, save the lyric text from the current line, and replace it with the lyric text from the next line
  for (
    // get the current line range
    let line = getLine(editor.selection.active.line);
    line.number < editor.document.lineCount - 2;
    // next line as position from line number
    line = getLine(line.number + 1)
  ) {
    const nextLineText =
      ext.getTimestampFromLine(line.number + 1)?.text ??
      ext.document.lineAt(line.number + 1).text;
    const timestamp = ext.getTimestampFromLine(line.number);
    if (timestamp === undefined) {
      continue;
    }
    newLines += `[${formatTimestamp(timestamp.milliseconds)}]` + nextLineText + '\n';
  }
  // replace documentRange with newLines
  await editor.edit(editBuilder => {
    editBuilder.replace(documentRange, newLines);
  });
}

/** first ask the user for the BPM of the track, then quantize the timestamps in the active text editor to the closest eighth note based on the given BPM */
async function quantizeLrc() {
  const editor = vscode.window.activeTextEditor;

  if (!editor) {
    vscode.window.showInformationMessage('No active editor found.');
    return;
  }

  const ext = new Ext(editor.document);

  const startBpmStr = ext.findHeader('bpm')?.value;
  let startBpm;
  if (startBpmStr !== undefined) {
    startBpm = parseInt(startBpmStr, 10);
    if (isNaN(startBpm)) {
      startBpm = undefined;
    }
  }
  const bpm = await timeInputBpm(startBpm);

  if (bpm === undefined) {
    return;
  }

  await ext.writeHeader('bpm', bpm.toString());

  const getLine = (line: number) => ({
    number: line,
    range: editor.document.lineAt(line),
  });

  const documentRange = new vscode.Range(
    getLine(0).range.range.start,
    editor.document.lineAt(editor.document.lineCount - 1).range.end,
  );

  const eighthNoteDuration = bpmToEighthNoteDuration(bpm);

  let newLines: string = '';
  for (
    let line = getLine(0);
    line.number < editor.document.lineCount - 1;
    line = getLine(line.number + 1)
  ) {
    const timestamp = ext.getTimestampFromLine(line.number);
    if (timestamp === undefined) {
      newLines += line.range.text + '\n';
      continue;
    }
    const adjustedMs = adjustTimestampToEighthNote(
      timestamp.milliseconds,
      eighthNoteDuration,
    );
    newLines += `[${formatTimestamp(adjustedMs)}]${timestamp.text}\n`;
  }

  await editor.edit(editBuilder => {
    editBuilder.replace(documentRange, newLines);
  });
}

// convert the given bpm to miliseconds
function bpmToMs(bpm: number) {
  return Math.floor((60 / bpm) * 1000);
}

// Show input boxes in a loop, and record the time between each input, averaging the last 5 inputs over a sliding window, then calculate the BPM of the average
async function timeInputBpm(startBpm?: number) {
  const startBpmMs = bpmToMs(startBpm ?? 120);
  const timeDifferences: number[] = [
    startBpmMs,
    startBpmMs,
    startBpmMs,
    startBpmMs,
    startBpmMs,
  ];
  // assign a weight to the time differences, so that the most recent time differences have more weight
  const weights = [0.1, 0.1, 0.2, 0.3, 0.3];

  const calculateBPM = () => {
    // use a weighted average here
    let avg = 0;
    for (let i = 0; i < timeDifferences.length; i++) {
      avg += timeDifferences[i] * weights[i];
    }

    return Math.floor(60000 / avg);
  };

  let lastPressTime = Date.now();
  while (true) {
    const res = await vscode.window.showInputBox({
      prompt: `Press enter to record BPM (current BPM: ${calculateBPM()}), enter the final BPM once you know, or press esc to finish`,
      placeHolder: 'BPM',
      value: startBpm !== undefined ? startBpm.toString() : undefined,
    });
    if (res === undefined) {
      return undefined;
    }
    if (res !== '') {
      const resBpm = parseInt(res, 10);
      if (isNaN(resBpm)) {
        vscode.window.showErrorMessage('Invalid BPM');
        continue;
      }
      return resBpm;
    }

    const now = Date.now();
    const timeDiff = now - lastPressTime;
    // Add the time difference to the array (limit to last 5 key presses)
    timeDifferences.shift(); // Remove the oldest time difference
    timeDifferences.push(timeDiff);

    lastPressTime = now;
  }
}

// If the difference to the timestamp on the next line is larger than 10 seconds, underline the next line and show a warning message on hover
export function registerCheckLineTimestamp(_context: vscode.ExtensionContext) {
  const changesToCheck: Set<vscode.TextDocument> = new Set();
  const everSeen = new Set<vscode.TextDocument>();

  return [
    vscode.workspace.onDidChangeTextDocument(e => {
      changesToCheck.add(e.document);
      if (vscode.window.activeTextEditor?.document === e.document) {
        doEditorChecks(vscode.window.activeTextEditor.document, everSeen, changesToCheck);
      }
    }),
    vscode.workspace.onDidOpenTextDocument(e => {
      changesToCheck.add(e);
      everSeen.add(e);
      if (vscode.window.activeTextEditor?.document === e) {
        doEditorChecks(vscode.window.activeTextEditor.document, everSeen, changesToCheck);
      }
    }),

    vscode.window.onDidChangeActiveTextEditor(editor => {
      if (editor) {
        doEditorChecks(editor.document, everSeen, changesToCheck);
      }
    }),
    vscode.window.onDidChangeVisibleTextEditors(editors => {
      for (const editor of editors) {
        doEditorChecks(editor.document, everSeen, changesToCheck);
      }
    }),
  ];
}

function doEditorChecks(
  document: vscode.TextDocument,
  everSeen: Set<vscode.TextDocument>,
  changesToCheck: Set<vscode.TextDocument>,
) {
  const ext = new Ext(document);

  if (!everSeen.has(document)) {
    changesToCheck.add(document);
    everSeen.add(document);
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
  constructor(public document: vscode.TextDocument) {}

  getTimeDifferenceToNextLineTimestamp(position: vscode.Position) {
    const thisLineTimestamp = this.getTimestampFromLine(position.line);
    const nextLineTimestamp = this.getTimestampFromLine(position.line + 1);
    if (!thisLineTimestamp || !nextLineTimestamp) {
      return;
    }
    return {
      difference: nextLineTimestamp.milliseconds - thisLineTimestamp.milliseconds,
      thisLineIsEmpty: thisLineTimestamp.text.trim() === '',
    };
  }

  /**
   * Retrieves the timestamp and text from the line at the given position in the active text editor.
   *
   * @param position - The position of the line in the editor.
   * @returns An object containing the milliseconds, seconds, and text extracted from the line.
   */
  getTimestampFromLine(line: number) {
    const lineText = this.document.lineAt(line).text;
    return this.getTimestampFromLineText(lineText);
  }

  getTimestampFromLineText(lineText: string) {
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

  // Find a header line of the format
  // [header:value]
  // at the beginning of the lrc file (before the first empty line)
  findHeader(headerName: string) {
    for (let line = 0; line < this.document.lineCount; line++) {
      const text = this.document.lineAt(line).text;
      if (text.trim() === '') {
        return;
      }
      const match = text.match(/^\[(\w+):(.*)\]$/);
      if (match && match[1] === headerName) {
        return { key: match[1], value: match[2], line: line };
      }
    }
  }

  // check if the given line is a header line
  isHeaderLine(line: string) {
    return (
      line.trim() !== '' &&
      line.match(/^\[(\w+):(.*)\]$/) !== null &&
      line.match(/^\[\d\d:\d\d.\d+\]/) === null
    );
  }

  // write the given header to the lrc file, if the header already exists, update the value
  async writeHeader(headerName: string, value: string) {
    const header = this.findHeader(headerName);
    const editor = findActiveEditor(this.document);
    if (!editor) {
      return;
    }
    if (header) {
      const lineRange = this.document.lineAt(header.line).range;
      await editor.edit(editBuilder => {
        editBuilder.replace(lineRange, `[${headerName}:${value}]`);
      });
    } else {
      // insert before the first timestamp line if no header is found, or after the last header if there are multiple headers
      let insertLine = 0;
      let extraNewline = '';
      for (let line = 0; line < this.document.lineCount; line++) {
        const text = this.document.lineAt(line).text;
        // check if header
        if (this.isHeaderLine(text)) {
          insertLine = line + 1;
        } else if (text.trim() === '') {
          insertLine = line;
          break;
        } else {
          insertLine = line;
          if (line == 0) {
            extraNewline = '\n';
          }
          break;
        }
      }
      await editor.edit(editBuilder => {
        editBuilder.insert(
          new vscode.Position(insertLine, 0),
          `[${headerName}:${value}]\n${extraNewline}`,
        );
      });
    }
  }
}

// find an active editor that has the given document opened
function findActiveEditor(document: vscode.TextDocument) {
  return vscode.window.visibleTextEditors.find(editor => editor.document === document);
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

  return `${String(minutes).padStart(2, '0')}:${seconds.padStart(5, '0')}`;
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

# Logging

Because the daemon protocol only has one sender stream and one receiver stream (and because proper multicasting was to difficult) logging messages need to be carefully interleaved with requests and responses. Usually this means that after the operation and all of its inputs (the request) has been read logging hijacks the sender stream (in the server case) and uses it to send typed logging messages while the request is being processed. When the response has been generated it will send `STDERR_LAST` or `STDERR_ERROR` to mark that what follows is the response data to the request.

While not in this state between request reading and response sending all messages are buffered until next time the logger can send data.

The logging messages supported are:
`STDERR_LAST`,  `STDERR_ERROR`,  `STDERR_NEXT`,  `STDERR_READ`, 
`STDERR_WRITE`,  `STDERR_START_ACTIVITY`,  `STDERR_STOP_ACTIVITY` and
`STDERR_RESULT`


### `STDERR_LAST`
Marks the last of the fake stderr and is sent when normal processing can resume.

### `STDERR_ERROR`
If protocol is 1.26 or higher sends exception otherwise sends error message text and exit status.
This also marks the end of this log "session" and so it has the same effect as `STDERR_LAST`

```
let error_type = self.from.read_string().await?;
assert_eq!(error_type, "Error");
let level: Verbosity = self.from.read_enum().await?;
let _name = self.from.read_string().await?; // Removed
let msg = self.from.read_string().await?;
let have_pos = self.from.read_usize().await?;
assert_eq!(have_pos, 0);
let nr_traces = self.from.read_usize().await?;
let mut traces = Vec::with_capacity(nr_traces);
for _ in 0..nr_traces {
  let have_pos = self.from.read_usize().await?;
  assert_eq!(have_pos, 0);
  let trace = self.from.read_string().await?;
  traces.push(trace);
}
```

### `STDERR_NEXT`
Normal string log message. Simply a single bytes buffer.

### `STDERR_READ`
Reader interface used by ImportsPaths and AddToStoreNar (between 1.21 and 1.23). It works by sending a desired buffer length and then on the receiver stream it reads bytes buffer of that length. If it receives 0 bytes it sees this as an unexpected EOF.

### `STDERR_WRITE`
Writer interface used by ExportPath. Simply writes a buffer.

### `STDERR_START_ACTIVITY`
Begins an activity. In other tracing frameworks this would be called a span.
Implemented in protocol 1.20 When backwards compatible with older versions of the protocol when this message would have been sent it is instead checks the level is enabled and sends the text as a normal log message with ```STDERR_NEXT```.

```
pub struct StartActivity {
  pub act: ActivityId,
  pub level: Verbosity,
  pub activity_type: ActivityType,
  pub text: String,
  pub fields: Vec<LoggerField>,
  pub parent: ActivityId,
}
```
act is atomic (nextId++ + (getPid() << 32))

```
pub enum ActivityType {
  Unknown = 0,
  CopyPath = 100,
  FileTransfer = 101,
  Realise = 102,
  CopyPaths = 103,
  Builds = 104,
  Build = 105,
  OptimiseStore = 106,
  VerifyPaths = 107,
  Substitute = 108,
  QueryPathInfo = 109,
  PostBuildHook = 110,
  BuildWaiting = 111,
  FetchTree = 112,
}
```

### `STDERR_STOP_ACTIVITY`
Stops the given activity. The activity id should not send any more results.
Just sends `ActivityId`.
Implemented in protocol 1.20. When backwards compatible with older versions of the protocol when this message would have been sent it is instead ignored.

### `STDERR_RESULT`
Sends results for a given activity.
Implemented in protocol 1.20. When backwards compatible with older versions of the protocol when this message would have been sent it is instead ignored.

```
pub struct ActivityResult {
  pub act: ActivityId,
  pub result_type: ResultType,
  pub fields: Vec<LoggerField>,
}
```

```
pub enum ResultType {
  FileLinked = 100,
  BuildLogLine = 101,
  UntrustedPath = 102,
  CorruptedPath = 103,
  SetPhase = 104,
  Progress = 105,
  SetExpected = 106,
  PostBuildLogLine = 107,
  FetchStatus = 108,
}
```

## Common Types

```
pub type ActivityId = u64;
```

```
pub enum LoggerField {
  Int(u64),
  String(String),
}
```
Serialized as first uint64 with LoggerFieldType and then the field value as either uint64 or string.

```
pub enum LoggerFieldType {
  Int = 0,
  String = 1,
}
```

```
pub enum Verbosity {
  Error = 0,
  Warn = 1,
  Notice = 2,
  Info = 3,
  Talkative = 4,
  Chatty = 5,
  Debug = 6,
  Vomit = 7,
}
```


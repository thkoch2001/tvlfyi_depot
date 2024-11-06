use std::{future::Future, io::Write, sync::Arc};
use tokio::{
    io::{AsyncWrite, AsyncWriteExt},
    sync::{
        mpsc::{self, UnboundedReceiver},
        oneshot, Mutex,
    },
    task::JoinHandle,
    task_local,
};
use tracing::Level;
use tracing_subscriber::{filter::filter_fn, Layer, Registry};

use crate::{nix_daemon::ser::NixWrite, worker_protocol::STDERR_NEXT};

pub fn client_layer() -> impl Layer<Registry> + Send + Sync {
    // TODO: should use a less fancy fmt? and send over spans as nix activity?
    tracing_subscriber::fmt::Layer::new()
        .with_writer(|| {
            CFG.try_with(|cfg| cfg.writer.clone())
                .unwrap_or(TracingInterceptor::Empty)
        })
        .with_filter(filter_fn(|meta| {
            CFG.try_with(|cfg| meta.level() <= &cfg.level)
                .unwrap_or(false)
        }))
}

/// Hooks tracing output into the provided writer.
///
/// Primary use case is to stream tracing logs to the nix-daemon client.
///
/// ```ignore
/// // somewhere in main()
///
/// tracing_subscriber::registry()
///   .with(client_layer())
///   // ...
///   .init();
///
/// // to activate
/// async fn process_client(writer: Arc<Mutex<NixWriter<SomeWriter>>>, level: tracing::Level) {
///   trace_into_writer(level, writer, async move || {
///     info!("Hello nix client");
///     // do stuff
///   }).await
/// }
/// ```
pub async fn trace_into_writer<T, R>(
    level: Level,
    writer: Arc<Mutex<T>>,
    fun: impl Future<Output = R>,
) -> R
where
    T: NixWrite + AsyncWrite + Send + Unpin + 'static,
{
    let (sender, receiver) = mpsc::unbounded_channel();
    let (shutdown_sender, shutdown_receiver) = oneshot::channel();

    let handle = tokio::spawn(async move {
        let mut worker = Worker {
            writer,
            receiver,
            shutdown: Some(shutdown_receiver),
        };
        worker.run().await
    });
    let me = TracingInterceptor::Active(sender.clone());
    let worker_guard = WorkerGuard {
        handle,
        sender,
        shutdown: shutdown_sender,
    };
    let result = CFG
        .scope(WriterCfg { writer: me, level }, async move {
            // Tell tracing that our interest may have changed due to level change.
            tracing_core::callsite::rebuild_interest_cache();
            fun.await
        })
        .await;
    worker_guard.drain().await;
    result
}

/// Task local config for the current tracing session.
struct WriterCfg {
    writer: TracingInterceptor,
    level: Level,
}

task_local! {
  static CFG: WriterCfg;
}

#[derive(Clone)]
#[allow(private_interfaces)]
enum TracingInterceptor {
    Empty,
    Active(mpsc::UnboundedSender<Msg>),
}

impl Write for TracingInterceptor {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            TracingInterceptor::Empty => Ok(buf.len()),
            TracingInterceptor::Active(sender) => {
                if sender.send(Msg::Line(buf.to_vec())).is_ok() {
                    Ok(buf.len())
                } else {
                    Err(std::io::Error::from(std::io::ErrorKind::Other))
                }
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.write(buf).map(|_| ())
    }
}

#[derive(Debug)]
pub(crate) enum Msg {
    Line(Vec<u8>),
    Shutdown,
}

pub struct WorkerGuard {
    handle: JoinHandle<()>,
    sender: mpsc::UnboundedSender<Msg>,
    shutdown: oneshot::Sender<()>,
}

impl WorkerGuard {
    pub async fn drain(self) {
        if self.sender.send(Msg::Shutdown).is_ok() {
            let _ = self.shutdown.send(());
            let _ = self.handle.await;
        };
    }
}

struct Worker<T> {
    writer: Arc<Mutex<T>>,
    receiver: UnboundedReceiver<Msg>,
    shutdown: Option<oneshot::Receiver<()>>,
}

impl<T: NixWrite + AsyncWrite + Send + Unpin> Worker<T> {
    async fn run(&mut self) {
        loop {
            match self.receiver.recv().await {
                Some(Msg::Line(line)) => {
                    let mut writer = self.writer.lock().await;
                    let _ = writer.write_number(STDERR_NEXT).await;
                    let _ = writer.write_slice(&line).await;
                }
                Some(Msg::Shutdown) => {
                    let _ = self.writer.lock().await.flush().await;
                    if let Some(rx) = self.shutdown.take() {
                        let _ = rx.await;
                    }
                    return;
                }
                None => {
                    return;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, time::Duration};

    use crate::nix_daemon::{
        ser::NixWriter,
        server::client_writer::{client_layer, trace_into_writer},
    };
    use tokio::{io::AsyncWriteExt, sync::Mutex};
    use tracing::{debug, error, Level};
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    fn vec_contains(vec: &[u8], value: &str) -> bool {
        vec.windows(value.len())
            .position(|window| window == value.as_bytes())
            .is_some()
    }

    #[tokio::test]
    async fn test_trace_into_writer() {
        tracing_subscriber::registry().with(client_layer()).init();

        error!("Not yet traced");

        let vec = vec![];
        let nix_writer = Arc::new(Mutex::new(NixWriter::new(vec)));

        trace_into_writer(Level::DEBUG, nix_writer.clone(), async move {
            error!("Traced");

            // Change some threads
            for _ in [0..3] {
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
            debug!("Still traced");
            //trace!("Level too low");
        })
        .await;
        debug!("No longer traced");
        let mut writer = nix_writer.lock().await;
        writer.flush().await.unwrap();

        let bytes = &writer.inner;

        assert!(!vec_contains(bytes, "Not yet traced"));
        assert!(vec_contains(bytes, "Traced"));
        assert!(vec_contains(bytes, "Still traced"));
        assert!(!vec_contains(bytes, "Level too low"));
        assert!(!vec_contains(bytes, "No longer traced"));
    }
}

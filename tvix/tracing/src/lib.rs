use indicatif::ProgressStyle;
use lazy_static::lazy_static;
use tokio::sync::{mpsc, oneshot};
use tracing::Level;
use tracing_indicatif::{filter::IndicatifFilter, IndicatifLayer};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer};

#[cfg(feature = "otlp")]
use opentelemetry::KeyValue;
#[cfg(feature = "otlp")]
use opentelemetry_sdk::{
    resource::{ResourceDetector, SdkProvidedResourceDetector},
    trace::BatchConfig,
    Resource,
};

lazy_static! {
    pub static ref PB_PROGRESS_STYLE: ProgressStyle = ProgressStyle::with_template(
        "{span_child_prefix}{bar:30} {wide_msg} [{elapsed_precise}]  {pos:>7}/{len:7}"
    )
    .expect("invalid progress template");
    pub static ref PB_SPINNER_STYLE: ProgressStyle = ProgressStyle::with_template(
        "{span_child_prefix}{spinner} {wide_msg} [{elapsed_precise}]  {pos:>7}/{len:7}"
    )
    .expect("invalid progress template");
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Init(#[from] tracing_subscriber::util::TryInitError),

    #[error(transparent)]
    MpscSend(#[from] mpsc::error::SendError<Option<oneshot::Sender<()>>>),

    #[error(transparent)]
    OneshotRecv(#[from] oneshot::error::RecvError),
}

#[derive(Clone)]
pub struct TracingHandle {
    #[cfg(feature = "otlp")]
    tx: mpsc::Sender<Option<oneshot::Sender<()>>>,
}

impl TracingHandle {
    pub async fn flush(&self, msg: Option<oneshot::Sender<()>>) -> Result<(), Error> {
        Ok(self.tx.send(msg).await?)
    }

    pub async fn shutdown(&self) -> Result<(), Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.flush(Some(tx)).await?;
        rx.await?;

        #[cfg(feature = "otlp")]
        {
            // Because of a bug within otlp we currently have to use spawn_blocking otherwise
            // calling `shutdown_tracer_provider` can block forever. See
            // https://github.com/open-telemetry/opentelemetry-rust/issues/1395#issuecomment-1953280335
            // TODO: this still throws an error, if tool exits regularly: "OpenTelemetry trace
            // error occurred. oneshot canceled", but not having this leads to errors if we cancel
            // with ctrl_c
            let _ = tokio::task::spawn_blocking(move || {
                opentelemetry::global::shutdown_tracer_provider();
            })
            .await;
        }

        Ok(())
    }
}

pub struct TracingBuilder {
    level: Level,

    #[cfg(feature = "otlp")]
    service_name: Option<&'static str>,
}

impl Default for TracingBuilder {
    fn default() -> Self {
        TracingBuilder {
            level: Level::INFO,

            #[cfg(feature = "otlp")]
            service_name: None,
        }
    }
}

impl TracingBuilder {
    pub fn level(mut self, level: Level) -> TracingBuilder {
        self.level = level;
        self
    }

    pub fn otlp(mut self, service_name: &'static str) -> TracingBuilder {
        self.service_name = Some(service_name);
        self
    }

    pub fn build(self) -> Result<TracingHandle, Error> {
        let indicatif_layer = IndicatifLayer::new().with_progress_style(PB_SPINNER_STYLE.clone());
        let (tx, mut rx) = mpsc::channel::<Option<oneshot::Sender<()>>>(16);

        // Set up the tracing subscriber.
        let subscriber = tracing_subscriber::registry()
            .with(
                tracing_subscriber::fmt::Layer::new()
                    .with_writer(indicatif_layer.get_stderr_writer())
                    .compact()
                    .with_filter(
                        EnvFilter::builder()
                            .with_default_directive(self.level.into())
                            .from_env()
                            .expect("invalid RUST_LOG"),
                    ),
            )
            .with(indicatif_layer.with_filter(
                // only show progress for spans with indicatif.pb_show field being set
                IndicatifFilter::new(false),
            ));

        #[cfg(feature = "otlp")]
        {
            if let Some(service_name) = self.service_name {
                let tracer = opentelemetry_otlp::new_pipeline()
                    .tracing()
                    .with_exporter(opentelemetry_otlp::new_exporter().tonic())
                    .with_batch_config(BatchConfig::default())
                    .with_trace_config(opentelemetry_sdk::trace::config().with_resource({
                        // use SdkProvidedResourceDetector.detect to detect resources,
                        // but replace the default service name with our default.
                        // https://github.com/open-telemetry/opentelemetry-rust/issues/1298
                        let resources =
                            SdkProvidedResourceDetector.detect(std::time::Duration::from_secs(0));
                        // SdkProvidedResourceDetector currently always sets
                        // `service.name`, but we don't like its default.
                        if resources.get("service.name".into()).unwrap() == "unknown_service".into()
                        {
                            resources.merge(&Resource::new([KeyValue::new(
                                "service.name",
                                service_name,
                            )]))
                        } else {
                            resources
                        }
                    }))
                    .install_batch(opentelemetry_sdk::runtime::Tokio)
                    .expect("Failed to install tokio runtime");

                let tracer_provider = tracer
                    .provider()
                    .expect("Failed to get the tracer provider");

                tokio::spawn(async move {
                    while let Some(m) = rx.recv().await {
                        let tracer_provider2 = tracer_provider.clone();

                        // Because of a bug within otlp we currently have to use spawn_blocking
                        // otherwise will calling `force_flush` block forever, especially if the
                        // tool was closed with ctrl_c. See
                        // https://github.com/open-telemetry/opentelemetry-rust/issues/1395#issuecomment-1953280335
                        let _ = tokio::task::spawn_blocking(move || {
                            tracer_provider2.force_flush();
                        })
                        .await;
                        if let Some(tx) = m {
                            let _ = tx.send(());
                        }
                    }
                });

                // Create a tracing layer with the configured tracer
                let layer = tracing_opentelemetry::layer().with_tracer(tracer);
                subscriber.with(Some(layer)).try_init()?;
                return Ok(TracingHandle { tx });
            }
        }

        // Noop receiver if we dont have otlp configured
        tokio::spawn(async move {
            while let Some(m) = rx.recv().await {
                if let Some(tx) = m {
                    let _ = tx.send(());
                }
            }
        });
        subscriber.try_init()?;
        Ok(TracingHandle { tx })
    }
}

//! Implements a `log::Log` logger that adheres to the structure
//! expected by Yandex Cloud Serverless logs.
//!
//! https://cloud.yandex.ru/docs/serverless-containers/concepts/logs

use log::{Level, Log};
use serde_json::json;

pub struct YandexCloudLogger;

pub const YANDEX_CLOUD_LOGGER: YandexCloudLogger = YandexCloudLogger;

fn level_map(level: &Level) -> &'static str {
    match level {
        Level::Error => "ERROR",
        Level::Warn => "WARN",
        Level::Info => "INFO",
        Level::Debug => "DEBUG",
        Level::Trace => "TRACE",
    }
}

impl Log for YandexCloudLogger {
    fn enabled(&self, _: &log::Metadata<'_>) -> bool {
        true
    }

    fn log(&self, record: &log::Record<'_>) {
        if !self.enabled(record.metadata()) {
            return;
        }

        eprintln!(
            "{}",
            json!({
                "level": level_map(&record.level()),
                "message": record.args().to_string(),
                "target": record.target(),
                "module": record.module_path(),
                "file": record.file(),
                "line": record.line(),
            })
        );
    }

    fn flush(&self) {}
}

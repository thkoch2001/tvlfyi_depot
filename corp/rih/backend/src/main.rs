use anyhow::{bail, Context, Result};
use log::{debug, error, info, warn, LevelFilter};
use rouille::{Request, Response};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::env;
use std::net::SocketAddr;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

mod yandex_log;

/// Represents the request sent by the frontend application.
#[derive(Debug, Deserialize)]
struct FrontendReq {
    captcha_token: String,
    record: Record,
}

/// Represents a single record as filled in by a user. This is the
/// primary data structure we want to populate and persist somewhere.
#[derive(Debug, Deserialize, Serialize)]
struct Record {
    // Record-specific metadata
    uuid: Uuid,

    // Personal information
    name: String,
    email: String,
    citizenship: String, // TODO
    personal_details: String,

    // Job information
    position: String,
    technologies: BTreeSet<String>,
    job_details: String,
    work_background: String,
}

impl Record {
    fn validate(&self) -> bool {
        true
    }
}

fn persist_record(ip: &SocketAddr, record: &Record) -> Result<()> {
    let bucket_name = "rih-backend-data";
    let credentials =
        s3::creds::Credentials::from_env().context("failed to initialise storage credentials")?;

    let yandex_region: s3::Region = s3::Region::Custom {
        region: "ru-central1".to_string(),
        endpoint: "storage.yandexcloud.net".to_string(),
    };

    let bucket = s3::Bucket::new(bucket_name, yandex_region, credentials)
        .context("failed to initialise storage client")?;

    let path_uuid = Uuid::new_v4();
    let epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .context("failed to get current time")?
        .as_secs();

    let path = format!("/records/{}-{}.json", epoch, path_uuid);

    info!("writing record to '{}'", path);

    let data = serde_json::json!({
        "ip": ip.to_string(),
        "record": record,
    });

    let response = bucket
        .put_object(path, data.to_string().as_bytes())
        .context("failed to persist storage object")?;

    debug!(
        "Object Storage response: ({}) {}",
        response.status_code(),
        response.as_str().unwrap_or("<unprintable>")
    );

    Ok(())
}

fn handle_submit(req: &Request) -> Result<Response> {
    let submitted: FrontendReq =
        rouille::input::json::json_input(req).context("failed to deserialise frontend request")?;

    if !submitted.record.validate() {
        bail!("invalid record: {:?}", submitted.record);
    }

    persist_record(req.remote_addr(), &submitted.record).context("failed to persist record")?;

    Ok(Response::text("success"))
}

fn main() -> Result<()> {
    log::set_logger(&yandex_log::YANDEX_CLOUD_LOGGER)
        .map(|()| log::set_max_level(LevelFilter::Info))
        .expect("log configuration must succeed");
    let port = env::var("PORT").unwrap_or_else(|_| /* rihb = */ "7442".to_string());
    let listen = format!("0.0.0.0:{port}");

    info!("launching rih-backend on: {}", listen);

    rouille::start_server(&listen, move |request| {
        if request.method() == "POST" && request.url() == "/submit" {
            info!("handling submit request from {}", request.remote_addr());
            match handle_submit(request) {
                Ok(response) => {
                    info!("submit handled successfully");
                    response
                }
                Err(err) => {
                    error!("failed to handle submit: {}", err);
                    Response::empty_400()
                }
            }
        } else {
            warn!(
                "no matching route for request: {} {}",
                request.method(),
                request.url()
            );

            Response::empty_404()
        }
    });
}

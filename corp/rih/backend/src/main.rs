use anyhow::{bail, Context, Result};
use rouille::{Request, Response};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::env;
use std::net::SocketAddr;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

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
    let credentials = s3::creds::Credentials::from_env()?;
    let yandex_region: s3::Region = s3::Region::Custom {
        region: "ru-central1".to_string(),
        endpoint: "storage.yandexcloud.net".to_string(),
    };

    let bucket = s3::Bucket::new(bucket_name, yandex_region, credentials)?;

    let path_uuid = Uuid::new_v4();
    let epoch = SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs();
    let path = format!("/records/{}-{}.json", epoch, path_uuid);

    let data = serde_json::json!({
        "ip": ip.to_string(),
        "record": record,
    });

    let _response = bucket.put_object(path, data.to_string().as_bytes());
    Ok(())
}

fn handle_submit(req: &Request) -> Result<Response> {
    let submitted: FrontendReq = rouille::input::json::json_input(req)?;

    if !submitted.record.validate() {
        bail!("invalid record: {:?}", submitted.record);
    }

    persist_record(req.remote_addr(), &submitted.record)?;

    Ok(Response::text("success"))
}

fn main() -> Result<()> {
    let port = env::var("PORT").unwrap_or_else(|_| /* rihb = */ "7442".to_string());
    let listen = format!("0.0.0.0:{port}");
    rouille::start_server(&listen, move |request| {
        if request.url() == "/submit" {
            match handle_submit(request) {
                Ok(response) => response,
                Err(_err) => Response::empty_400(), // TODO
            }
        } else {
            Response::empty_404()
        }
    });
}

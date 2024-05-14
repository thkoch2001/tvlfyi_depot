use axum::body::Body;
use axum::extract::Query;
use axum::http::StatusCode;
use axum::response::Response;
use bytes::Bytes;
use data_encoding::BASE64URL_NOPAD;
use serde::Deserialize;
use tokio_util::io::ReaderStream;
use tracing::{instrument, warn};

use crate::AppState;

#[derive(Debug, Deserialize)]
pub(crate) struct GetNARParams {
    #[serde(rename = "narsize")]
    nar_size: u64,
}

#[instrument(skip(blob_service, directory_service))]
pub async fn get(
    axum::extract::Path(root_node_enc): axum::extract::Path<String>,
    axum::extract::Query(GetNARParams { nar_size }): Query<GetNARParams>,
    axum::extract::State(AppState {
        blob_service,
        directory_service,
        ..
    }): axum::extract::State<AppState>,
) -> Result<Response, StatusCode> {
    use prost::Message;
    // decode the root node
    let root_node_enc = BASE64URL_NOPAD
        .decode(root_node_enc.as_bytes())
        .map_err(|e| {
            warn!(err=%e, "unable to decode root node b64");
            StatusCode::NOT_FOUND
        })?;
    // TODO: reasonable size limit, (digest len) validation
    let root_node: tvix_castore::proto::Node = Message::decode(Bytes::from(root_node_enc))
        .map_err(|e| {
            warn!(err=%e, "unable to decode root node proto");
            StatusCode::NOT_FOUND
        })?;
    let root_node = root_node.node.ok_or_else(|| {
        warn!("node must be some");
        StatusCode::NOT_FOUND
    })?;

    let (w, r) = tokio::io::duplex(1024 * 8);

    tokio::spawn(async move {
        if let Err(e) =
            tvix_store::nar::write_nar(w, &root_node, blob_service, directory_service).await
        {
            warn!(err=%e, "failed to write out NAR");
        }
    });

    Ok(Response::builder()
        .status(StatusCode::OK)
        .header("cache-control", "max-age=31536000, immutable")
        .header("content-length", nar_size)
        .body(Body::from_stream(ReaderStream::new(r)))
        .unwrap())
}

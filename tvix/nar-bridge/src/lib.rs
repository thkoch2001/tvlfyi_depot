use axum::body::Body;
use axum::http::StatusCode;
use axum::response::Response;
use bytes::Bytes;
use data_encoding::BASE64URL_NOPAD;
use nix_compat::nixbase32;
use std::sync::Arc;
use tokio_util::io::ReaderStream;
use tracing::{instrument, warn, Span};
use tvix_castore::blobservice::BlobService;
use tvix_castore::directoryservice::DirectoryService;
use tvix_castore::proto::node::Node;
use tvix_store::pathinfoservice::PathInfoService;

#[derive(Clone)]
pub struct AppState {
    blob_service: Arc<dyn BlobService>,
    directory_service: Arc<dyn DirectoryService>,
    path_info_service: Arc<dyn PathInfoService>,
}

impl AppState {
    pub fn new(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
        path_info_service: Arc<dyn PathInfoService>,
    ) -> Self {
        Self {
            blob_service,
            directory_service,
            path_info_service,
        }
    }
}

#[instrument(skip(path_info_service))]
pub async fn get_narinfo(
    axum::extract::Path(narinfo_str): axum::extract::Path<String>,
    axum::extract::State(AppState {
        path_info_service, ..
    }): axum::extract::State<AppState>,
) -> Result<String, StatusCode> {
    // pop the .narinfo suffix
    let hash_str = narinfo_str.strip_suffix(".narinfo").ok_or_else(|| {
        warn!("no .narinfo suffix");
        StatusCode::NOT_FOUND
    })?;
    // parse the digest
    let hash_str_fixed: [u8; 32] = hash_str.as_bytes().try_into().map_err(|e| {
        warn!(err=%e, "invalid digest len");
        StatusCode::NOT_FOUND
    })?;
    let digest: [u8; 20] = nixbase32::decode_fixed(hash_str_fixed).map_err(|e| {
        warn!(err=%e, "invalid digest");
        StatusCode::NOT_FOUND
    })?;

    Span::current().record("path_info.digest", hash_str);

    // fetch the PathInfo
    let path_info = path_info_service
        .get(digest)
        .await
        .map_err(|e| {
            warn!(err=%e, "failed to get PathInfo");
            StatusCode::INTERNAL_SERVER_ERROR
        })?
        .ok_or_else(|| {
            warn!("PathInfo not found");
            StatusCode::NOT_FOUND
        })?;

    let store_path = path_info.validate().map_err(|e| {
        warn!(err=%e, "invalid PathInfo");
        StatusCode::INTERNAL_SERVER_ERROR
    })?;

    let mut narinfo = path_info.to_narinfo(store_path).ok_or_else(|| {
        warn!(path_info=?path_info, "PathInfo contained no NAR data");
        StatusCode::INTERNAL_SERVER_ERROR
    })?;

    // encode the (unnamed) root node in the NAR url itself.
    let root_node = path_info
        .node
        .as_ref()
        .and_then(|n| n.node.as_ref())
        .expect("root node must not be none")
        .clone()
        .rename("".into());

    let mut buf = Vec::new();
    Node::encode(&root_node, &mut buf);

    let mut url = "nar/".to_string();
    data_encoding::BASE64URL_NOPAD.encode_append(&buf, &mut url);

    narinfo.url = &url;

    Ok(narinfo.to_string())
}

#[instrument(skip(blob_service, directory_service))]
pub async fn get_nar(
    axum::extract::Path(root_node_enc): axum::extract::Path<String>,
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
        // FUTUREWORK: send size in header?
        .body(Body::from_stream(ReaderStream::new(r)))
        .unwrap())
}

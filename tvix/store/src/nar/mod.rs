use crate::proto;
use data_encoding::BASE64;
use thiserror::Error;

mod non_caching_calculation_service;
mod renderer;

pub use non_caching_calculation_service::NonCachingNARCalculationService;

/// Errors that can encounter while rendering NARs.
#[derive(Debug, Error)]
pub enum RenderError {
    #[error("failure talking to a backing store client: {0}")]
    StoreError(crate::Error),

    #[error("unable to find directory {}, referred from {}", BASE64.encode(.0), .1)]
    DirectoryNotFound(Vec<u8>, String),

    #[error("unable to find blob {}, referred from {}", BASE64.encode(.0), .1)]
    BlobNotFound(Vec<u8>, String),

    #[error("failure using the NAR writer: {0}")]
    NARWriterError(std::io::Error),
}

/// The base trait for something calculating NARs, and returning their size and sha256.
pub trait NARCalculationService {
    fn calculate_nar(
        &self,
        root_node: proto::node::Node,
    ) -> Result<proto::CalculateNarResponse, RenderError>;
}

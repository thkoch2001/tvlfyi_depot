use thiserror::Error;
use tonic::Status;

/// Errors related to communication with the store.
#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid request: {0}")]
    InvalidRequest(String),

    #[error("internal error loading data: {0}")]
    StorageLoadError(String),

    #[error("internal error storing data: {0}")]
    StoragePutError(String),
}

impl From<Error> for Status {
    fn from(value: Error) -> Self {
        match value {
            Error::InvalidRequest(msg) => Status::invalid_argument(msg),
            Error::StorageLoadError(msg) => {
                Status::data_loss(format!("storage load error: {}", msg))
            }
            Error::StoragePutError(msg) => Status::data_loss(format!("storage put error: {}", msg)),
        }
    }
}

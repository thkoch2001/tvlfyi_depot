//! This module provides low-level generated gRPC clients for the
//! Yandex Cloud APIs.
//!
//! The clients are generated using the [tonic][] and [prost][]
//! crates and have default configuration.
//!
//! Documentation present in the protos is retained into the generated
//! Rust types, but for detailed API information you should visit the
//! official Yandex Cloud Documentation pages:
//!
//! * [in English](https://cloud.yandex.com/en-ru/docs/overview/api)
//! * [in Russian](https://cloud.yandex.ru/docs/overview/api)
//!
//! The proto sources are available on the [Yandex Cloud GitHub][protos].
//!
//! [tonic]: https://docs.rs/tonic/latest/tonic/
//! [prost]: https://docs.rs/prost/latest/prost/
//! [protos]: https://github.com/yandex-cloud/cloudapi
//!
//! The majority of user-facing structures can be found in the
//! [`yandex::cloud`] module.
//!
//! ## Usage
//!
//! Typically to use these APIs, you need to provide an authentication
//! credential and an endpoint to connect to. The full list of
//! Yandex's endpoints is [available online][endpoints] and you should
//! look up the service you plan to use and pick the correct endpoint
//! from the list.
//!
//! Authentication is done via an HTTP header using an IAM token,
//! which can be done in Tonic using [interceptors][]. The
//! [`AuthInterceptor`] provided by this crate can be used for that
//! purpose.
//!
//! [endpoints]: https://cloud.yandex.com/en/docs/api-design-guide/concepts/endpoints
//! [interceptors]: https://docs.rs/tonic/latest/tonic/service/trait.Interceptor.html

use tonic::metadata::{Ascii, MetadataValue};
use tonic::service::Interceptor;

/// Publicly re-export some types from tonic which users might need
/// for implementing traits, or for naming concrete client types.
pub mod tonic_exports {
    pub use tonic::service::interceptor::InterceptedService;
    pub use tonic::transport::Channel;
    pub use tonic::Status;
}

/// Helper trait for types or closures that can provide authentication
/// tokens for Yandex Cloud.
pub trait TokenProvider {
    /// Fetch a currently valid authentication token for Yandex Cloud.
    fn get_token<'a>(&'a mut self) -> Result<&'a str, tonic::Status>;
}

impl TokenProvider for String {
    fn get_token<'a>(&'a mut self) -> Result<&'a str, tonic::Status> {
        Ok(self.as_str())
    }
}

impl TokenProvider for &'static str {
    fn get_token(&mut self) -> Result<&'static str, tonic::Status> {
        Ok(*self)
    }
}

/// Interceptor for adding authentication headers to gRPC requests.
/// This is constructed with a callable that returns authentication
/// tokens.
///
/// This callable is responsible for ensuring that the returned tokens
/// are valid at the given time, i.e. it should take care of
/// refreshing and so on.
pub struct AuthInterceptor<T: TokenProvider> {
    token_provider: T,
}

impl<T: TokenProvider> AuthInterceptor<T> {
    pub fn new(token_provider: T) -> Self {
        Self { token_provider }
    }
}

impl<T: TokenProvider> Interceptor for AuthInterceptor<T> {
    fn call(
        &mut self,
        mut request: tonic::Request<()>,
    ) -> Result<tonic::Request<()>, tonic::Status> {
        let token: MetadataValue<Ascii> = format!("Bearer {}", self.token_provider.get_token()?)
            .try_into()
            .map_err(|_| {
                tonic::Status::invalid_argument("authorization token contained invalid characters")
            })?;

        request.metadata_mut().insert("authorization", token);

        Ok(request)
    }
}

// The rest of this file is generated by the build script at ../build.rs.
include!("includes.rs");

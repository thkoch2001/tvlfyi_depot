pub mod worker_protocol;

use std::io::Result;

use futures::future::try_join_all;
use tracing::warn;
use types::{QueryValidPaths, UnkeyedValidPathInfo};

use crate::store_path::StorePath;

pub mod handler;
pub mod types;

/// Represents all possible operations over the nix-daemon protocol.
pub trait NixDaemonIO: Sync {
    fn is_valid_path(
        &self,
        path: &StorePath<String>,
    ) -> impl std::future::Future<Output = Result<bool>> + Send {
        async move { Ok(self.query_path_info(path).await?.is_some()) }
    }

    fn query_path_info(
        &self,
        path: &StorePath<String>,
    ) -> impl std::future::Future<Output = Result<Option<UnkeyedValidPathInfo>>> + Send;

    fn query_path_from_hash_part(
        &self,
        hash: &[u8],
    ) -> impl std::future::Future<Output = Result<Option<UnkeyedValidPathInfo>>> + Send;

    fn query_valid_paths(
        &self,
        request: &QueryValidPaths,
    ) -> impl std::future::Future<Output = Result<Vec<UnkeyedValidPathInfo>>> + Send {
        async move {
            if request.substitute {
                warn!("tvix does not yet support substitution, ignoring the 'substitute' flag...");
            }
            // Using try_join_all here to avoid returning partial results to the client.
            // The only reason query_path_info can fail is due to transient IO errors,
            // so we return such errors to the client as opposed to only returning paths
            // that succeeded.
            let result =
                try_join_all(request.paths.iter().map(|path| self.query_path_info(path))).await?;
            let result: Vec<UnkeyedValidPathInfo> = result.into_iter().flatten().collect();
            Ok(result)
        }
    }

    fn query_valid_derivers(
        &self,
        path: &StorePath<String>,
    ) -> impl std::future::Future<Output = Result<Vec<StorePath<String>>>> + Send {
        async move {
            let result = self.query_path_info(path).await?;
            let result: Vec<_> = result.into_iter().filter_map(|info| info.deriver).collect();
            Ok(result)
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{nix_daemon::types::QueryValidPaths, store_path::StorePath};

    use super::{types::UnkeyedValidPathInfo, NixDaemonIO};

    // Very simple mock
    // Unable to use mockall as it does not support unboxed async traits.
    pub struct MockNixDaemonIO {
        query_path_info_result: Option<UnkeyedValidPathInfo>,
    }

    impl NixDaemonIO for MockNixDaemonIO {
        async fn query_path_info(
            &self,
            _path: &StorePath<String>,
        ) -> std::io::Result<Option<UnkeyedValidPathInfo>> {
            Ok(self.query_path_info_result.clone())
        }

        async fn query_path_from_hash_part(
            &self,
            _hash: &[u8],
        ) -> std::io::Result<Option<UnkeyedValidPathInfo>> {
            Ok(None)
        }
    }

    #[tokio::test]
    async fn test_is_valid_path_returns_true() {
        let path =
            StorePath::<String>::from_bytes("z6r3bn5l51679pwkvh9nalp6c317z34m-hello".as_bytes())
                .unwrap();
        let io = MockNixDaemonIO {
            query_path_info_result: Some(UnkeyedValidPathInfo::default()),
        };

        let result = io
            .is_valid_path(&path)
            .await
            .expect("expected to get a non-empty response");
        assert!(result, "expected to get true");
    }

    #[tokio::test]
    async fn test_is_valid_path_returns_false() {
        let path =
            StorePath::<String>::from_bytes("z6r3bn5l51679pwkvh9nalp6c317z34m-hello".as_bytes())
                .unwrap();
        let io = MockNixDaemonIO {
            query_path_info_result: None,
        };

        let result = io
            .is_valid_path(&path)
            .await
            .expect("expected to get a non-empty response");
        assert!(!result, "expected to get false");
    }

    #[tokio::test]
    async fn test_query_valid_paths_returns_empty_response() {
        let path =
            StorePath::<String>::from_bytes("z6r3bn5l51679pwkvh9nalp6c317z34m-hello".as_bytes())
                .unwrap();
        let io = MockNixDaemonIO {
            query_path_info_result: None,
        };

        let result = io
            .query_valid_paths(&QueryValidPaths {
                paths: vec![path],
                substitute: false,
            })
            .await
            .expect("expected to get a non-empty response");
        assert_eq!(result, vec![], "expected to get empty response");
    }

    #[tokio::test]
    async fn test_query_valid_paths_returns_non_empty_response() {
        let path =
            StorePath::<String>::from_bytes("z6r3bn5l51679pwkvh9nalp6c317z34m-hello".as_bytes())
                .unwrap();
        let io = MockNixDaemonIO {
            query_path_info_result: Some(UnkeyedValidPathInfo::default()),
        };

        let result = io
            .query_valid_paths(&QueryValidPaths {
                paths: vec![path],
                substitute: false,
            })
            .await
            .expect("expected to get a non-empty response");
        assert_eq!(
            result,
            vec![UnkeyedValidPathInfo::default()],
            "expected to get non empty response"
        );
    }

    #[tokio::test]
    async fn test_query_valid_derivers_returns_empty_response() {
        let path =
            StorePath::<String>::from_bytes("z6r3bn5l51679pwkvh9nalp6c317z34m-hello".as_bytes())
                .unwrap();
        let io = MockNixDaemonIO {
            query_path_info_result: None,
        };

        let result = io
            .query_valid_derivers(&path)
            .await
            .expect("expected to get a non-empty response");
        assert_eq!(result, vec![], "expected to get empty response");
    }

    #[tokio::test]
    async fn test_query_valid_derivers_returns_non_empty_response() {
        let path =
            StorePath::<String>::from_bytes("z6r3bn5l51679pwkvh9nalp6c317z34m-hello".as_bytes())
                .unwrap();
        let deriver = StorePath::<String>::from_bytes(
            "z6r3bn5l51679pwkvh9nalp6c317z34m-hello.drv".as_bytes(),
        )
        .unwrap();
        let io = MockNixDaemonIO {
            query_path_info_result: Some(UnkeyedValidPathInfo {
                deriver: Some(deriver.clone()),
                nar_hash: "".to_owned(),
                references: vec![],
                registration_time: 0,
                nar_size: 1,
                ultimate: true,
                signatures: vec![],
                ca: None,
            }),
        };

        let result = io
            .query_valid_derivers(&path)
            .await
            .expect("expected to get a non-empty response");
        assert_eq!(result, vec![deriver], "expected to get non empty response");
    }
}

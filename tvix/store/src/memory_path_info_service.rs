use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tonic::{Request, Response, Result, Status};

use crate::proto::get_path_info_request::ByWhat::ByOutputHash;
use crate::proto::path_info_service_server::PathInfoService;
use crate::proto::{GetPathInfoRequest, PathInfo, PutPathInfoRequest};

#[derive(Debug, Default)]
pub struct MemoryPathInfoService {
    path_infos: Arc<RwLock<HashMap<Vec<u8>, PathInfo>>>,
}

#[tonic::async_trait]
/// PathinfoService that stores PathInfo in memory
impl PathInfoService for MemoryPathInfoService {
    async fn get(&self, request: Request<GetPathInfoRequest>) -> Result<Response<PathInfo>> {
        let get_path_info_request = request.into_inner();
        if let Some(ByOutputHash(output_hash)) = get_path_info_request.by_what {
            let path_infos_r = self.path_infos.read().await;
            if let Some(path_info) = path_infos_r.get(&output_hash) {
                Ok(Response::new(path_info.clone()))
            } else {
                Err(Status::not_found("Not found."))
            }
        } else {
            Err(Status::invalid_argument(
                "The by_what field must be present and contain ByOutputHash",
            ))
        }
    }
    async fn put(&self, request: Request<PutPathInfoRequest>) -> Result<Response<PathInfo>> {
        let put_path_info_request = request.into_inner();
        if let Some(path_info) = put_path_info_request.path_info {
            // TODO: verify output hash
            let found = {
                let path_infos_r = self.path_infos.read().await;
                path_infos_r.contains_key(&put_path_info_request.output_hash)
            };
            // TODO: check if they match
            if !found {
                let mut path_infos_w = self.path_infos.write().await;
                path_infos_w.insert(put_path_info_request.output_hash, path_info.clone());
            }
            Ok(Response::new(path_info))
        } else {
            Err(Status::invalid_argument("The path_info field is missing."))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::memory_path_info_service::MemoryPathInfoService;
    use crate::proto::get_path_info_request::ByWhat::ByOutputHash;
    use crate::proto::path_info_service_server::PathInfoService;
    use crate::proto::{GetPathInfoRequest, PathInfo, PutPathInfoRequest};

    #[tokio::test]
    async fn test_put_get() {
        let service = MemoryPathInfoService::default();

        let path_info = PathInfo {
            references: vec![],
            narinfo: None,
            node: None,
        };
        // let hash = vec![0,32];
        let hash = "fake-hash".as_bytes().to_vec();

        let result = service
            .put(tonic::Request::new(PutPathInfoRequest {
                path_info: Some(path_info.clone()),
                output_hash: hash.clone(),
            }))
            .await;
        assert!(result.is_ok());

        let response = service
            .get(tonic::Request::new(GetPathInfoRequest {
                by_what: Some(ByOutputHash(hash)),
            }))
            .await
            .unwrap()
            .into_inner();
        assert_eq!(path_info, response);
    }
}

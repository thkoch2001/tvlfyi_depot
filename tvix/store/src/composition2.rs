use std::sync::Arc;
use std::path::Path;
use std::collections::HashMap;

use serde::Deserialize;
use futures::future::OptionFuture;
use tvix_castore::blobservice::BlobService;
use tvix_castore::directoryservice::DirectoryService;

use crate::pathinfoservice::PathInfoService;

type BlobStoreBuilder = Fn(name: String, from_config: BlobStoreBuilder, config: toml::Table) -> anyhow::Result<Arc<dyn BlobService>>;

#[derive(Default, Deserialize, Debug)]
pub struct BuilderRegistry {
    blob_store_builders: HashMap<String, BlobStoreBuilder>,
    directory_store_builders: HashMap<String, BlobStoreBuilder>,
    path_info_store_builders: HashMap<String, BlobStoreBuilder>,
}

#[derive(Default, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct StoreComposition {
    blob_stores: HashMap<String, BlobStoreConfig>,
//    directory_stores: HashMap<String, DirectoryStoreConfig>,
//    path_info_stores: HashMap<String, PathInfoStoreConfig>,
}

impl StoreComposition {
    pub async fn new() -> StoreComposition {
        let mut comp = StoreComposition::default();

        // Read system-wide config
        if let Some(system_config) = Self::from_file("/etc/tvix-store.toml").await {
            println!("{:?}", system_config);
            comp.merge(system_config);
        }

        // Read home config
        let user_config_path = dirs::config_dir().map(|dir| dir.join("tvix-store.toml"));
        if let Some(user_config) = OptionFuture::from(user_config_path.as_deref().map(Self::from_file)).await.flatten() {
            println!("{:?}", user_config);
            comp.merge(user_config);
        }

        comp
    }

    pub async fn from_file(path: &(impl AsRef<Path> + ?Sized)) -> Option<StoreComposition> {
        toml::from_str(&tokio::fs::read_to_string(&path).await.ok()?).ok()
    }

    pub fn merge(&mut self, new: StoreComposition) {
        self.blob_stores.extend(&mut new.blob_stores.into_iter());
        //self.directory_stores.extend(&mut new.directory_stores.into_iter());
        //self.path_info_stores.extend(&mut new.path_info_stores.into_iter());
    }

    pub fn instantiate_blob_service(&self, name: &str) -> Option<Arc<dyn BlobService>> {
        todo!()
    }
    pub fn instantiate_directory_service(&self, name: &str) -> Option<Arc<dyn DirectoryService>> {
        todo!()
    }
    pub fn instantiate_path_info_service(&self, name: &str) -> Option<Arc<dyn PathInfoService>> {
        todo!()
    }
}

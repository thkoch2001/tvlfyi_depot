use crate::blobservice::BlobService;
use crate::directoryservice::DirectoryService;
use erased_serde::deserialize;
use lazy_static::lazy_static;
use serde::de::DeserializeOwned;
use serde_tagged::de::FnSeed;
use std::collections::BTreeMap;
use std::sync::Mutex;

pub struct BoxFnSeed<V>(Box<dyn FnSeed<V, Output = Result<V, erased_serde::Error>> + Send + Sync>);

impl<'de, 'b, V> serde::de::DeserializeSeed<'de> for &'b BoxFnSeed<V> {
    type Value = V;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut de = <dyn erased_serde::Deserializer>::erase(deserializer);
        (self.0)(&mut de).map_err(serde::de::Error::custom)
    }
}

pub struct Registry<T: ?Sized>(BTreeMap<&'static str, BoxFnSeed<Box<T>>>);

impl<T: ?Sized> Registry<T> {
    #[allow(private_bounds)]
    pub fn register<C: DeserializeOwned, S: IntoDynBox<T>>(
        &mut self,
        name: &'static str,
        from_config: &'static (dyn Fn(C) -> Result<S, Box<dyn std::error::Error>>
                      + Send
                      + Sync
                      + 'static),
    ) {
        self.0.insert(
            name,
            BoxFnSeed(Box::new(move |de| {
                let cfg: C = deserialize(de)?;
                Ok(from_config(cfg).map_err(serde::de::Error::custom)?.into())
            })),
        );
    }
}

trait IntoDynBox<T: ?Sized> {
    fn into(self) -> Box<T>;
}

impl<T: BlobService + 'static> IntoDynBox<dyn BlobService> for T {
    fn into(self) -> Box<dyn BlobService> {
        Box::new(self)
    }
}

impl<T: DirectoryService + 'static> IntoDynBox<dyn DirectoryService> for T {
    fn into(self) -> Box<dyn DirectoryService> {
        Box::new(self)
    }
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct NoConfig {}

lazy_static! {
    pub static ref BLOB_REG: Mutex<Registry<dyn BlobService>> = {
        let mut reg = Registry(Default::default());
        reg.register(
            "objectstore",
            &super::blobservice::ObjectStoreBlobService::from_config,
        );
        reg.register("memory", &|_: NoConfig| {
            Ok(super::blobservice::MemoryBlobService::default())
        });
        Mutex::new(reg)
    };
    pub static ref DIRECTORY_REG: Mutex<Registry<dyn DirectoryService>> = {
        let mut reg = Registry(Default::default());
        reg.register(
            "objectstore",
            &super::directoryservice::ObjectStoreDirectoryService::from_config,
        );
        reg.register("memory", &|_: NoConfig| {
            Ok(super::directoryservice::MemoryDirectoryService::default())
        });
        Mutex::new(reg)
    };
}

impl<'de> serde::Deserialize<'de> for Box<dyn BlobService> {
    fn deserialize<D>(de: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde_tagged::de::internal::deserialize(de, "type", &BLOB_REG.lock().unwrap().0)
    }
}

impl<'de> serde::Deserialize<'de> for Box<dyn DirectoryService> {
    fn deserialize<D>(de: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde_tagged::de::internal::deserialize(de, "type", &DIRECTORY_REG.lock().unwrap().0)
    }
}

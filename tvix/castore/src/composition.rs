use crate::blobservice::BlobServiceBuilder;
use crate::directoryservice::DirectoryServiceBuilder;
use erased_serde::deserialize;
use lazy_static::lazy_static;
use serde::de::DeserializeOwned;
use serde_tagged::de::BoxFnSeed;
use std::collections::BTreeMap;

pub struct Registry<T: ?Sized>(BTreeMap<&'static str, BoxFnSeed<Box<T>>>);

impl<T: ?Sized> Registry<T> {
    #[allow(private_bounds)]
    pub fn register<C: DeserializeOwned + IntoDynBox<T>>(&mut self, type_name: &'static str) {
        self.0.insert(
            type_name,
            BoxFnSeed::new(|x| deserialize::<C>(x).map(IntoDynBox::into)),
        );
    }
}

impl<'de> serde::Deserialize<'de> for Box<dyn BlobServiceBuilder> {
    fn deserialize<D>(de: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde_tagged::de::internal::deserialize(de, "type", &ACTIVE_BLOB_REG.get().unwrap().0)
    }
}

impl<'de> serde::Deserialize<'de> for Box<dyn DirectoryServiceBuilder> {
    fn deserialize<D>(de: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde_tagged::de::internal::deserialize(de, "type", &ACTIVE_DIRECTORY_REG.get().unwrap().0)
    }
}

trait IntoDynBox<T: ?Sized> {
    fn into(self) -> Box<T>;
}

impl<T: BlobServiceBuilder + 'static> IntoDynBox<dyn BlobServiceBuilder> for T {
    fn into(self) -> Box<dyn BlobServiceBuilder> {
        Box::new(self)
    }
}

impl<T: DirectoryServiceBuilder + 'static> IntoDynBox<dyn DirectoryServiceBuilder> for T {
    fn into(self) -> Box<dyn DirectoryServiceBuilder> {
        Box::new(self)
    }
}

fn add_default_blobservices(reg: &mut Registry<dyn BlobServiceBuilder>) {
    reg.register::<super::blobservice::ObjectStoreBlobServiceConfig>("objectstore");
    reg.register::<super::blobservice::MemoryBlobServiceConfig>("memory");
    reg.register::<super::blobservice::CombinedBlobServiceConfig>("combined");
    reg.register::<super::blobservice::GRPCBlobServiceConfig>("grpc");
}

fn add_default_directoryservices(reg: &mut Registry<dyn DirectoryServiceBuilder>) {
    reg.register::<super::directoryservice::ObjectStoreDirectoryServiceConfig>("objectstore");
    reg.register::<super::directoryservice::MemoryDirectoryServiceConfig>("memory");
    reg.register::<super::directoryservice::CacheConfig>("cache");
}

use std::cell::Cell;

pub fn with_blob_registry<R>(
    reg: &'static Registry<dyn BlobServiceBuilder>,
    f: impl Fn() -> R,
) -> R {
    ACTIVE_BLOB_REG.set(Some(reg));
    let result = f();
    ACTIVE_BLOB_REG.set(None);
    result
}

thread_local! {
    static ACTIVE_BLOB_REG: Cell<Option<&'static Registry<dyn BlobServiceBuilder>>> = panic!("blob reg was accessed before initialization");
    static ACTIVE_DIRECTORY_REG: Cell<Option<&'static Registry<dyn DirectoryServiceBuilder>>> = panic!("directory reg was accessed before initialization");
}

lazy_static! {
    static ref BLOB_REG: Registry<dyn BlobServiceBuilder> = {
        let mut reg = Registry(Default::default());
        add_default_blobservices(&mut reg);
        reg
    };
    static ref DIRECTORY_REG: Registry<dyn DirectoryServiceBuilder> = {
        let mut reg = Registry(Default::default());
        add_default_directoryservices(&mut reg);
        reg
    };
}

use crate::blobservice::BlobServiceBuilder;
use crate::directoryservice::DirectoryServiceBuilder;
use erased_serde::deserialize;
use futures::future::BoxFuture;
use futures::FutureExt;
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

use crate::blobservice::BlobService;
use std::collections::HashMap;
use std::sync::Arc;

enum StoreInstantiationState {
    Config(Box<dyn BlobServiceBuilder>),
    InProgress(
        tokio::sync::watch::Receiver<Option<Result<Arc<dyn BlobService>, StoreCompositionError>>>,
    ),
    Done(Result<Arc<dyn BlobService>, StoreCompositionError>),
}

struct StoreComposition {
    stores: std::sync::Mutex<HashMap<String, StoreInstantiationState>>,
}

#[derive(thiserror::Error, Clone, Debug)]
enum StoreCompositionError {
    #[error("store not found")]
    NotFound,
    #[error("recursion not allowed")]
    Recursion,
    #[error("store construction panicked")]
    Poisoned,
    #[error("service instantiation failed: {0}")]
    Failed(String),
}

impl StoreComposition {
    fn build_internal<'a>(
        &'a self,
        stack: Vec<String>,
        entrypoint: String,
    ) -> BoxFuture<'a, Result<Arc<dyn BlobService + 'static>, StoreCompositionError>> {
        let mut stores = self.stores.lock().unwrap();
        let entry = match stores.get_mut(&entrypoint) {
            Some(v) => v,
            None => return Box::pin(futures::future::err(StoreCompositionError::NotFound)),
        };
        // for lifetime reasons, we put a placeholder value in the hashmap while we figure out what
        // the new value should be. the Mutex stays locked the entire time, so nobody will ever see
        // this temporary value.
        let prev_val = std::mem::replace(
            entry,
            StoreInstantiationState::Done(Err(StoreCompositionError::Poisoned)),
        );
        let (new_val, ret) = match prev_val {
            StoreInstantiationState::Done(service) => (
                StoreInstantiationState::Done(service.clone()),
                futures::future::ready(service).boxed(),
            ),
            // the construction of the store has not started yet.
            StoreInstantiationState::Config(config) => {
                let (tx, rx) = tokio::sync::watch::channel(None);
                (
                    StoreInstantiationState::InProgress(rx),
                    (async move {
                        let res = config
                            .build(&entrypoint, &|new_entrypoint| {
                                let mut new_stack = stack.clone();
                                new_stack.push(entrypoint.clone());
                                // disallow recursion
                                if stack.contains(&new_entrypoint) {
                                    return futures::future::err(
                                        StoreCompositionError::Recursion.into(),
                                    )
                                    .boxed();
                                }
                                Box::pin(async move {
                                    Ok(self.build_internal(new_stack, new_entrypoint).await?)
                                })
                            })
                            .await
                            .map_err(|e| StoreCompositionError::Failed(e.to_string()));
                        tx.send(Some(res.clone())).unwrap();
                        res
                    })
                    .boxed(),
                )
            }
            // there is already a task driving forward the construction of this store, wait for it
            // to notify us via the provided channel
            StoreInstantiationState::InProgress(mut recv) => {
                (StoreInstantiationState::InProgress(recv.clone()), {
                    (async move {
                        loop {
                            if let Some(v) =
                                recv.borrow_and_update().as_ref().map(|res| res.clone())
                            {
                                break v;
                            }
                            recv.changed().await.unwrap();
                        }
                    })
                    .boxed()
                })
            }
        };
        *entry = new_val;
        ret
    }
}

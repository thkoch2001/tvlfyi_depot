//! The composition module allows composing different kinds of services based on a set of service
//! configurations _at runtime_.
//!
//! Store configs are deserialized with serde. The registry provides a stateful mapping from the
//! `type` tag of an internally tagged enum on the serde side to a Config struct which is
//! deserialized and then returned as a `Box<dyn ServiceBuilder<Output = Arc<dyn BlobService>>>`
//! (the same for DirectoryService instead of BlobService etc).
//!
//! ### Example 1.: You want to implement a new BlobService
//!
//! You need a Config struct which implements `DeserializeOwned` and
//! `ServiceBuilder<Output = Arc<dyn BlobService>>`. Provide the user with a function to call with
//! their registry. You register your new type as:
//!
//! ```
//! pub fn add_my_service(reg: &mut Registry) {
//!     reg.register::<Box<dyn ServiceBuilder<Output = Arc<dyn BlobService>>>, MyBlobServiceConfig>("myblobservicetype");
//! }
//! ```
//!
//! Now, when a user deserializes a store config with the type tag "myblobservicetype" into a
//! `Box<dyn ServiceBuilder<Output = Arc<dyn BlobService>>>`, it will be done via `MyBlobServiceConfig`.
//!
//! ### Example 2.: You want to compose a store
//!
//! ```
//! use tvix_castore::composition::{with_registry, REG, Composition};
//!
//! let blob_services_configs_json = serde_json::json! {
//!   "blobstore1": {
//!     "type": "memory",
//!   },
//!   "blobstore2": {
//!     "type": "memory",
//!   },
//!   "default": {
//!     "type": "combined",
//!     "local": "blobstore1",
//!     "remote": "blobstore1"
//!   }
//! };
//!
//! let blob_services_configs = with_registry(&REG, || serde_json::from_value(&blob_services_configs_json))?;
//! let blob_service_composition = Composition::<dyn BlobService>::from_configs(blob_services_configs);
//! let blob_service = blob_service_composition.build("default").await?;
//! ```
//!
//! ### Example 3.: Create your own registry extending the default registry with third party types
//!
//! ```
//! let mut my_registry = tvix_castore::composition::Registry::default();
//! tvix_castore::composition::add_default_services(&mut my_registry);
//! add_my_service(&mut my_registry);
//! ```
//!
//! Continue with Example 2, with my_registry instead of REG

use erased_serde::deserialize;
use futures::FutureExt;
use futures::future::BoxFuture;
use lazy_static::lazy_static;
use serde::de::DeserializeOwned;
use serde_tagged::de::{BoxFnSeed, SeedFactory};
use serde_tagged::util::TagString;
use std::any::{Any, TypeId};
use std::cell::Cell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use tonic::async_trait;

// Registry implementation details:
// This is really ugly. Really we would want to store this as a generic static field:
//
// ```
// struct Registry<T>(BTreeMap<(&'static str), BoxSeedFn<T>);
// static REG<T>: Registry<T>;
// ```
//
// so that one version of the static is generated for each Type that the registry is accessed for.
// However, this is not possible, because generics are only a thing in functions, and even there
// they will not interact with static items:
// https://doc.rust-lang.org/reference/items/static-items.html#statics--generics
//
// So instead, we make this lookup at runtime by putting the TypeId into the key.
// But now we can no longer store the `BoxFnSeed<T>` because we are lacking the generic parameter
// T, so instead store it as `Box<dyn Any>` and downcast to `&BoxFnSeed<T>` when performing the
// lookup.
// I said it was ugly...
/// Resolves tag names to the corresponding Config type.
#[derive(Default)]
pub struct Registry(BTreeMap<(TypeId, &'static str), Box<dyn Any + Sync>>);

struct RegistryWithFakeType<'r, T>(&'r Registry, PhantomData<T>);

impl<'r, 'de: 'r, T: 'static> SeedFactory<'de, TagString<'de>> for RegistryWithFakeType<'r, T> {
    type Value = DeserializeWithRegistry<T>;
    type Seed = &'r BoxFnSeed<Self::Value>;

    // Required method
    fn seed<E>(self, tag: TagString<'de>) -> Result<Self::Seed, E>
    where
        E: serde::de::Error,
    {
        // using find() and not get() because of https://github.com/rust-lang/rust/issues/80389
        let seed: &Box<dyn Any + Sync> = self
            .0
             .0
            .iter()
            .find(|(k, _)| *k == &(TypeId::of::<T>(), tag.as_ref()))
            .ok_or_else(|| serde::de::Error::custom("Unknown tag"))?
            .1;

        Ok(<dyn Any>::downcast_ref(&**seed).unwrap())
    }
}

pub struct DeserializeWithRegistry<T>(T);

impl Registry {
    pub fn register<T: 'static, C: DeserializeOwned + Into<T>>(&mut self, type_name: &'static str) {
        let seed = BoxFnSeed::new(|x| {
            deserialize::<C>(x)
                .map(Into::into)
                .map(DeserializeWithRegistry)
        });
        self.0
            .insert((TypeId::of::<T>(), type_name), Box::new(seed));
    }
}

impl<'de, T: 'static> serde::Deserialize<'de> for DeserializeWithRegistry<T> {
    fn deserialize<D>(de: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde_tagged::de::internal::deserialize(
            de,
            "type",
            RegistryWithFakeType(ACTIVE_REG.get().unwrap(), PhantomData::<T>),
        )
    }
}

thread_local! {
    static ACTIVE_REG: Cell<Option<&'static Registry>> = panic!("reg was accessed before initialization");
}

/// Run the provided closure with a registry context.
/// Any serde deserialize calls within the closure will use the registry to resolve tag names to
/// the corresponding Config type.
pub fn with_registry<R>(reg: &'static Registry, f: impl Fn() -> R) -> R {
    ACTIVE_REG.set(Some(reg));
    let result = f();
    ACTIVE_REG.set(None);
    result
}

lazy_static! {
    pub static ref REG: Registry = {
        let mut reg = Registry(Default::default());
        add_default_services(&mut reg);
        reg
    };
}

pub fn add_default_services(reg: &mut Registry) {
    crate::blobservice::add_default_services(reg);
    crate::directoryservice::add_default_services(reg);
}

#[async_trait]
pub trait ServiceBuilder: Send + Sync {
    type Output;
    async fn build<'a>(
        &'a self,
        instance_name: &str,
        resolve: &(dyn Fn(
            String,
        ) -> BoxFuture<
            'a,
            Result<Self::Output, Box<dyn std::error::Error + Send + Sync + 'static>>,
        > + Sync),
    ) -> Result<Self::Output, Box<dyn std::error::Error + Send + Sync + 'static>>;
}

impl<T, S: ServiceBuilder<Output = T> + 'static> From<S> for Box<dyn ServiceBuilder<Output = T>> {
    fn from(t: S) -> Self {
        Box::new(t)
    }
}

enum InstantiationState<T: ?Sized> {
    Config(Box<dyn ServiceBuilder<Output = Arc<T>>>),
    InProgress(tokio::sync::watch::Receiver<Option<Result<Arc<T>, CompositionError>>>),
    Done(Result<Arc<T>, CompositionError>),
}

pub struct Composition<T: ?Sized> {
    stores: std::sync::Mutex<HashMap<String, InstantiationState<T>>>,
}

#[derive(thiserror::Error, Clone, Debug)]
pub enum CompositionError {
    #[error("store not found")]
    NotFound,
    #[error("recursion not allowed")]
    Recursion,
    #[error("store construction panicked")]
    Poisoned,
    #[error("service instantiation failed: {0}")]
    Failed(Arc<dyn std::error::Error + Send + Sync>),
}

impl<T: Send + Sync + ?Sized + 'static> Composition<T> {
    pub fn from_configs(
        configs: HashMap<String, DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = Arc<T>>>>>,
    ) -> Self {
        Composition {
            stores: std::sync::Mutex::new(
                configs
                    .into_iter()
                    .map(|(k, v)| (k, InstantiationState::Config(v.0)))
                    .collect(),
            ),
        }
    }

    pub async fn build(&self, entrypoint: &str) -> Result<Arc<T>, CompositionError> {
        self.build_internal(vec![], entrypoint.to_string()).await
    }

    fn build_internal(
        &self,
        stack: Vec<String>,
        entrypoint: String,
    ) -> BoxFuture<'_, Result<Arc<T>, CompositionError>> {
        let mut stores = self.stores.lock().unwrap();
        let entry = match stores.get_mut(&entrypoint) {
            Some(v) => v,
            None => return Box::pin(futures::future::err(CompositionError::NotFound)),
        };
        // for lifetime reasons, we put a placeholder value in the hashmap while we figure out what
        // the new value should be. the Mutex stays locked the entire time, so nobody will ever see
        // this temporary value.
        let prev_val = std::mem::replace(
            entry,
            InstantiationState::Done(Err(CompositionError::Poisoned)),
        );
        let (new_val, ret) = match prev_val {
            InstantiationState::Done(service) => (
                InstantiationState::Done(service.clone()),
                futures::future::ready(service).boxed(),
            ),
            // the construction of the store has not started yet.
            InstantiationState::Config(config) => {
                let (tx, rx) = tokio::sync::watch::channel(None);
                (
                    InstantiationState::InProgress(rx),
                    (async move {
                        let res = config
                            .build(&entrypoint, &|new_entrypoint| {
                                let mut new_stack = stack.clone();
                                new_stack.push(entrypoint.clone());
                                // disallow recursion
                                if stack.contains(&new_entrypoint) {
                                    return futures::future::err(
                                        CompositionError::Recursion.into(),
                                    )
                                    .boxed();
                                }
                                Box::pin(async move {
                                    Ok(self.build_internal(new_stack, new_entrypoint).await?)
                                })
                            })
                            .await
                            .map_err(|e| CompositionError::Failed(e.into()));
                        tx.send(Some(res.clone())).unwrap();
                        res
                    })
                    .boxed(),
                )
            }
            // there is already a task driving forward the construction of this store, wait for it
            // to notify us via the provided channel
            InstantiationState::InProgress(mut recv) => {
                (InstantiationState::InProgress(recv.clone()), {
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

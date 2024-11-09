//! The composition module allows composing different kinds of services based on a set of service
//! configurations _at runtime_.
//!
//! Store configs are deserialized with serde. The registry provides a stateful mapping from the
//! `type` tag of an internally tagged enum on the serde side to a Config struct which is
//! deserialized and then returned as a `Box<dyn ServiceBuilder<Output = dyn BlobService>>`
//! (the same for DirectoryService instead of BlobService etc).
//!
//! ### Example 1.: Implementing a new BlobService
//!
//! You need a Config struct which implements `DeserializeOwned` and
//! `ServiceBuilder<Output = dyn BlobService>`.
//! Provide the user with a function to call with
//! their registry. You register your new type as:
//!
//! ```
//! use std::sync::Arc;
//!
//! use tvix_castore::composition::*;
//! use tvix_castore::blobservice::BlobService;
//!
//! #[derive(serde::Deserialize)]
//! struct MyBlobServiceConfig {
//! }
//!
//! #[tonic::async_trait]
//! impl ServiceBuilder for MyBlobServiceConfig {
//!     type Output = dyn BlobService;
//!     async fn build(&self, _: &str, _: &CompositionContext) -> Result<Arc<Self::Output>, Box<dyn std::error::Error + Send + Sync + 'static>> {
//!         todo!()
//!     }
//! }
//!
//! impl TryFrom<url::Url> for MyBlobServiceConfig {
//!     type Error = Box<dyn std::error::Error + Send + Sync>;
//!     fn try_from(url: url::Url) -> Result<Self, Self::Error> {
//!         todo!()
//!     }
//! }
//!
//! pub fn add_my_service(reg: &mut Registry) {
//!     reg.register::<Box<dyn ServiceBuilder<Output = dyn BlobService>>, MyBlobServiceConfig>("myblobservicetype");
//! }
//! ```
//!
//! Now, when a user deserializes a store config with the type tag "myblobservicetype" into a
//! `Box<dyn ServiceBuilder<Output = Arc<dyn BlobService>>>`, it will be done via `MyBlobServiceConfig`.
//!
//! ### Example 2.: Composing stores to get one store
//!
//! ```
//! use std::sync::Arc;
//! use tvix_castore::composition::*;
//! use tvix_castore::blobservice::BlobService;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap().block_on(async move {
//! let blob_services_configs_json = serde_json::json!({
//!   "blobstore1": {
//!     "type": "memory"
//!   },
//!   "blobstore2": {
//!     "type": "memory"
//!   },
//!   "root": {
//!     "type": "combined",
//!     "local": "blobstore1",
//!     "remote": "blobstore2"
//!   }
//! });
//!
//! let blob_services_configs = with_registry(&REG, || serde_json::from_value(blob_services_configs_json))?;
//! let mut blob_service_composition = Composition::new(&REG);
//! blob_service_composition.extend_with_configs::<dyn BlobService>(blob_services_configs);
//! let blob_service: Arc<dyn BlobService> = blob_service_composition.build("root").await?;
//! # Ok(())
//! # })
//! # }
//! ```
//!
//! ### Example 3.: Creating another registry extending the default registry with third-party types
//!
//! ```
//! # pub fn add_my_service(reg: &mut tvix_castore::composition::Registry) {}
//! let mut my_registry = tvix_castore::composition::Registry::default();
//! tvix_castore::composition::add_default_services(&mut my_registry);
//! add_my_service(&mut my_registry);
//! ```
//!
//! Continue with Example 2, with my_registry instead of REG
//!
//! EXPERIMENTAL: If the xp-composition-url-refs feature is enabled,
//! entrypoints can also be URL strings, which are created as
//! anonymous stores. Instantiations of the same URL will
//! result in a new, distinct anonymous store each time, so creating
//! two `memory://` stores with this method will not share the same view.
//! This behavior might change in the future.

use erased_serde::deserialize;
use futures::future::BoxFuture;
use futures::FutureExt;
use serde::de::DeserializeOwned;
use serde_tagged::de::{BoxFnSeed, SeedFactory};
use serde_tagged::util::TagString;
use std::any::{Any, TypeId};
use std::cell::Cell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::{Arc, LazyLock};
use tonic::async_trait;

/// Resolves tag names to the corresponding Config type.
// Registry implementation details:
// This is really ugly. Really we would want to store this as a generic static field:
//
// ```
// struct Registry<T>(BTreeMap<(&'static str), RegistryEntry<T>);
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
#[derive(Default)]
pub struct Registry(BTreeMap<(TypeId, &'static str), Box<dyn Any + Sync>>);
pub type FromUrlSeed<T> =
    Box<dyn Fn(url::Url) -> Result<T, Box<dyn std::error::Error + Send + Sync>> + Sync>;
pub struct RegistryEntry<T> {
    serde_deserialize_seed: BoxFnSeed<DeserializeWithRegistry<T>>,
    from_url_seed: FromUrlSeed<DeserializeWithRegistry<T>>,
}

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
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown type: {}", tag)))?
            .1;

        let entry: &RegistryEntry<T> = <dyn Any>::downcast_ref(&**seed).unwrap();

        Ok(&entry.serde_deserialize_seed)
    }
}

/// Wrapper type which implements Deserialize using the registry
///
/// Wrap your type in this in order to deserialize it using a registry, e.g.
/// `RegistryWithFakeType<Box<dyn MyTrait>>`, then the types registered for `Box<dyn MyTrait>`
/// will be used.
pub struct DeserializeWithRegistry<T>(pub T);

impl Registry {
    /// Registers a mapping from type tag to a concrete type into the registry.
    ///
    /// The type parameters are very important:
    /// After calling `register::<Box<dyn FooTrait>, FooStruct>("footype")`, when a user
    /// deserializes into an input with the type tag "myblobservicetype" into a
    /// `Box<dyn FooTrait>`, it will first call the Deserialize imple of `FooStruct` and
    /// then convert it into a `Box<dyn FooTrait>` using From::from.
    pub fn register<
        T: 'static,
        C: DeserializeOwned
            + TryFrom<url::Url, Error = Box<dyn std::error::Error + Send + Sync>>
            + Into<T>,
    >(
        &mut self,
        type_name: &'static str,
    ) {
        self.0.insert(
            (TypeId::of::<T>(), type_name),
            Box::new(RegistryEntry {
                serde_deserialize_seed: BoxFnSeed::new(|x| {
                    deserialize::<C>(x)
                        .map(Into::into)
                        .map(DeserializeWithRegistry)
                }),
                from_url_seed: Box::new(|url| {
                    C::try_from(url)
                        .map(Into::into)
                        .map(DeserializeWithRegistry)
                }),
            }),
        );
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

#[derive(Debug, thiserror::Error)]
enum TryFromUrlError {
    #[error("Unknown type: {0}")]
    UnknownTag(String),
}

impl<T: 'static> TryFrom<url::Url> for DeserializeWithRegistry<T> {
    type Error = Box<dyn std::error::Error + Send + Sync>;
    fn try_from(url: url::Url) -> Result<Self, Self::Error> {
        let tag = url.scheme().split('+').next().unwrap();
        // same as in the SeedFactory impl: using find() and not get() because of https://github.com/rust-lang/rust/issues/80389
        let seed = ACTIVE_REG
            .get()
            .unwrap()
            .0
            .iter()
            .find(|(k, _)| *k == &(TypeId::of::<T>(), tag))
            .ok_or_else(|| Box::new(TryFromUrlError::UnknownTag(tag.into())))?
            .1;
        let entry: &RegistryEntry<T> = <dyn Any>::downcast_ref(&**seed).unwrap();
        (entry.from_url_seed)(url)
    }
}

thread_local! {
    /// The active Registry is global state, because there is no convenient and universal way to pass state
    /// into the functions usually used for deserialization, e.g. `serde_json::from_str`, `toml::from_str`,
    /// `serde_qs::from_str`.
    static ACTIVE_REG: Cell<Option<&'static Registry>> = panic!("reg was accessed before initialization");
}

/// Run the provided closure with a registry context.
/// Any serde deserialize calls within the closure will use the registry to resolve tag names to
/// the corresponding Config type.
pub fn with_registry<R>(reg: &'static Registry, f: impl FnOnce() -> R) -> R {
    ACTIVE_REG.set(Some(reg));
    let result = f();
    ACTIVE_REG.set(None);
    result
}

/// The provided registry of tvix_castore, with all builtin BlobStore/DirectoryStore implementations
pub static REG: LazyLock<&'static Registry> = LazyLock::new(|| {
    let mut reg = Default::default();
    add_default_services(&mut reg);
    // explicitly leak to get an &'static, so that we gain `&Registry: Send` from `Registry: Sync`
    Box::leak(Box::new(reg))
});

// ---------- End of generic registry code --------- //

/// Register the builtin services of tvix_castore with the given registry.
/// This is useful for creating your own registry with the builtin types _and_
/// extra third party types.
pub fn add_default_services(reg: &mut Registry) {
    crate::blobservice::register_blob_services(reg);
    crate::directoryservice::register_directory_services(reg);
}

pub struct CompositionContext<'a> {
    // The stack used to detect recursive instantiations and prevent deadlocks
    // The TypeId of the trait object is included to distinguish e.g. the
    // BlobService "root" and the DirectoryService "root".
    stack: Vec<(TypeId, String)>,
    registry: &'static Registry,
    composition: Option<&'a Composition>,
}

impl<'a> CompositionContext<'a> {
    /// Get a composition context for one-off store creation.
    pub fn blank(registry: &'static Registry) -> Self {
        Self {
            registry,
            stack: Default::default(),
            composition: None,
        }
    }

    pub async fn resolve<T: ?Sized + Send + Sync + 'static>(
        &self,
        entrypoint: String,
    ) -> Result<Arc<T>, Box<dyn std::error::Error + Send + Sync + 'static>> {
        // disallow recursion
        if self
            .stack
            .contains(&(TypeId::of::<T>(), entrypoint.clone()))
        {
            return Err(CompositionError::Recursion(
                self.stack.iter().map(|(_, n)| n.clone()).collect(),
            )
            .into());
        }

        Ok(self.build_internal(entrypoint).await?)
    }

    #[cfg(feature = "xp-composition-url-refs")]
    async fn build_anonymous<T: ?Sized + Send + Sync + 'static>(
        &self,
        entrypoint: String,
    ) -> Result<Arc<T>, Box<dyn std::error::Error + Send + Sync>> {
        let url = url::Url::parse(&entrypoint)?;
        let config: DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = T>>> =
            with_registry(self.registry, || url.try_into())?;
        config.0.build("anonymous", self).await
    }

    fn build_internal<T: ?Sized + Send + Sync + 'static>(
        &self,
        entrypoint: String,
    ) -> BoxFuture<'_, Result<Arc<T>, CompositionError>> {
        #[cfg(feature = "xp-composition-url-refs")]
        if entrypoint.contains("://") {
            // There is a chance this is a url. we are building an anonymous store
            return Box::pin(async move {
                self.build_anonymous(entrypoint.clone())
                    .await
                    .map_err(|e| CompositionError::Failed(entrypoint, Arc::from(e)))
            });
        }

        let mut stores = match self.composition {
            Some(comp) => comp.stores.lock().unwrap(),
            None => return Box::pin(futures::future::err(CompositionError::NotFound(entrypoint))),
        };
        let entry = match stores.get_mut(&(TypeId::of::<T>(), entrypoint.clone())) {
            Some(v) => v,
            None => return Box::pin(futures::future::err(CompositionError::NotFound(entrypoint))),
        };
        // for lifetime reasons, we put a placeholder value in the hashmap while we figure out what
        // the new value should be. the Mutex stays locked the entire time, so nobody will ever see
        // this temporary value.
        let prev_val = std::mem::replace(
            entry,
            Box::new(InstantiationState::<T>::Done(Err(
                CompositionError::Poisoned(entrypoint.clone()),
            ))),
        );
        let (new_val, ret) = match *prev_val.downcast::<InstantiationState<T>>().unwrap() {
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
                        let mut new_context = CompositionContext {
                            composition: self.composition,
                            registry: self.registry,
                            stack: self.stack.clone(),
                        };
                        new_context
                            .stack
                            .push((TypeId::of::<T>(), entrypoint.clone()));
                        let res =
                            config.build(&entrypoint, &new_context).await.map_err(|e| {
                                match e.downcast() {
                                    Ok(e) => *e,
                                    Err(e) => CompositionError::Failed(entrypoint, e.into()),
                                }
                            });
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
        *entry = Box::new(new_val);
        ret
    }
}

#[async_trait]
/// This is the trait usually implemented on a per-store-type Config struct and
/// used to instantiate it.
pub trait ServiceBuilder: Send + Sync {
    type Output: ?Sized;
    async fn build(
        &self,
        instance_name: &str,
        context: &CompositionContext,
    ) -> Result<Arc<Self::Output>, Box<dyn std::error::Error + Send + Sync + 'static>>;
}

impl<T: ?Sized, S: ServiceBuilder<Output = T> + 'static> From<S>
    for Box<dyn ServiceBuilder<Output = T>>
{
    fn from(t: S) -> Self {
        Box::new(t)
    }
}

enum InstantiationState<T: ?Sized> {
    Config(Box<dyn ServiceBuilder<Output = T>>),
    InProgress(tokio::sync::watch::Receiver<Option<Result<Arc<T>, CompositionError>>>),
    Done(Result<Arc<T>, CompositionError>),
}

pub struct Composition {
    registry: &'static Registry,
    stores: std::sync::Mutex<HashMap<(TypeId, String), Box<dyn Any + Send + Sync>>>,
}

#[derive(thiserror::Error, Clone, Debug)]
pub enum CompositionError {
    #[error("store not found: {0}")]
    NotFound(String),
    #[error("recursion not allowed {0:?}")]
    Recursion(Vec<String>),
    #[error("store construction panicked {0}")]
    Poisoned(String),
    #[error("instantiation of service {0} failed: {1}")]
    Failed(String, Arc<dyn std::error::Error + Send + Sync>),
}

impl<T: ?Sized + Send + Sync + 'static>
    Extend<(
        String,
        DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = T>>>,
    )> for Composition
{
    fn extend<I>(&mut self, configs: I)
    where
        I: IntoIterator<
            Item = (
                String,
                DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = T>>>,
            ),
        >,
    {
        self.stores
            .lock()
            .unwrap()
            .extend(configs.into_iter().map(|(k, v)| {
                (
                    (TypeId::of::<T>(), k),
                    Box::new(InstantiationState::Config(v.0)) as Box<dyn Any + Send + Sync>,
                )
            }))
    }
}

impl Composition {
    /// The given registry will be used for creation of anonymous stores during composition
    pub fn new(registry: &'static Registry) -> Self {
        Self {
            registry,
            stores: Default::default(),
        }
    }

    pub fn extend_with_configs<T: ?Sized + Send + Sync + 'static>(
        &mut self,
        // Keep the concrete `HashMap` type here since it allows for type
        // inference of what type is previously being deserialized.
        configs: HashMap<String, DeserializeWithRegistry<Box<dyn ServiceBuilder<Output = T>>>>,
    ) {
        self.extend(configs);
    }

    /// Looks up the entrypoint name in the composition and returns an instantiated service.
    pub async fn build<T: ?Sized + Send + Sync + 'static>(
        &self,
        entrypoint: &str,
    ) -> Result<Arc<T>, CompositionError> {
        self.context().build_internal(entrypoint.to_string()).await
    }

    pub fn context(&self) -> CompositionContext {
        CompositionContext {
            registry: self.registry,
            stack: vec![],
            composition: Some(self),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::blobservice::BlobService;
    use std::sync::Arc;

    /// Test that we return a reference to the same instance of MemoryBlobService (via ptr_eq)
    /// when instantiating the same entrypoint twice. By instantiating concurrently, we also
    /// test the channels notifying the second consumer when the store has been instantiated.
    #[tokio::test]
    async fn concurrent() {
        let blob_services_configs_json = serde_json::json!({
            "root": {
                "type": "memory",
            }
        });

        let blob_services_configs =
            with_registry(&REG, || serde_json::from_value(blob_services_configs_json)).unwrap();
        let mut blob_service_composition = Composition::new(&REG);
        blob_service_composition.extend_with_configs::<dyn BlobService>(blob_services_configs);
        let (blob_service1, blob_service2) = tokio::join!(
            blob_service_composition.build::<dyn BlobService>("root"),
            blob_service_composition.build::<dyn BlobService>("root")
        );
        assert!(Arc::ptr_eq(
            &blob_service1.unwrap(),
            &blob_service2.unwrap()
        ));
    }

    /// Test that we throw the correct error when an instantiation would recurse (deadlock)
    #[tokio::test]
    async fn reject_recursion() {
        let blob_services_configs_json = serde_json::json!({
            "root": {
                "type": "combined",
                "local": "other",
                "remote": "other"
            },
            "other": {
                "type": "combined",
                "local": "root",
                "remote": "root"
            }
        });

        let blob_services_configs =
            with_registry(&REG, || serde_json::from_value(blob_services_configs_json)).unwrap();
        let mut blob_service_composition = Composition::new(&REG);
        blob_service_composition.extend_with_configs::<dyn BlobService>(blob_services_configs);
        match blob_service_composition
            .build::<dyn BlobService>("root")
            .await
        {
            Err(CompositionError::Recursion(stack)) => {
                assert_eq!(stack, vec!["root".to_string(), "other".to_string()])
            }
            other => panic!("should have returned an error, returned: {:?}", other.err()),
        }
    }
}

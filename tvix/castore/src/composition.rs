use erased_serde::deserialize;
use futures::future::BoxFuture;
use futures::FutureExt;
use lazy_static::lazy_static;
use serde::de::DeserializeOwned;
use serde_tagged::de::{BoxFnSeed, SeedFactory};
use serde_tagged::util::TagString;
use std::any::{Any, TypeId};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use tonic::async_trait;

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

        Ok(<dyn Any>::downcast_ref(seed).unwrap())
    }
}

pub struct DeserializeWithRegistry<T>(T);

impl Registry {
    #[allow(private_bounds)]
    pub fn register<T: 'static, C: DeserializeOwned + Into<T>>(&mut self, type_name: &'static str) {
        self.0.insert(
            (TypeId::of::<T>(), type_name),
            Box::new(BoxFnSeed::new(|x| deserialize::<C>(x).map(Into::into))),
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

use std::cell::Cell;

pub fn with_registry<R>(reg: &'static Registry, f: impl Fn() -> R) -> R {
    ACTIVE_REG.set(Some(reg));
    let result = f();
    ACTIVE_REG.set(None);
    result
}

thread_local! {
    static ACTIVE_REG: Cell<Option<&'static Registry>> = panic!("reg was accessed before initialization");
}

lazy_static! {
    static ref REG: Registry = {
        let mut reg = Registry(Default::default());
        crate::blobservice::add_default_services(&mut reg);
        crate::directoryservice::add_default_services(&mut reg);
        reg
    };
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

enum InstantiationState<T> {
    Config(Box<dyn ServiceBuilder<Output = Arc<T>>>),
    InProgress(tokio::sync::watch::Receiver<Option<Result<Arc<T>, CompositionError>>>),
    Done(Result<Arc<T>, CompositionError>),
}

struct Composition<T> {
    stores: std::sync::Mutex<HashMap<String, InstantiationState<T>>>,
}

#[derive(thiserror::Error, Clone, Debug)]
enum CompositionError {
    #[error("store not found")]
    NotFound,
    #[error("recursion not allowed")]
    Recursion,
    #[error("store construction panicked")]
    Poisoned,
    #[error("service instantiation failed: {0}")]
    Failed(String),
}

impl<T: Send + Sync + 'static> Composition<T> {
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
                            .map_err(|e| CompositionError::Failed(e.to_string()));
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

use std::{
    collections::HashMap,
    io::{Error, ErrorKind},
};

use enum_primitive_derive::Primitive;
use num_traits::{FromPrimitive, ToPrimitive};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use crate::wire::primitive;

use super::bytes::read_bytes;

pub static STDERR_LAST: u64 = 0x616c7473;

/// Worker Operation
///
/// These operations are encoded as unsigned 64 bits before being sent
/// to the wire. See the [read_op] and
/// [write_op] operations to serialize/deserialize the
/// operation on the wire.
///
/// Note: for now, we're using the Nix 2.20 operation description. The
/// operations marked as obsolete are obsolete for Nix 2.20, not
/// necessarily for Nix 2.3. We'll revisit this later on.
#[derive(Debug, PartialEq, Primitive)]
pub enum Operation {
    IsValidPath = 1,
    HasSubstitutes = 3,
    QueryPathHash = 4,   // obsolete
    QueryReferences = 5, // obsolete
    QueryReferrers = 6,
    AddToStore = 7,
    AddTextToStore = 8, // obsolete since 1.25, Nix 3.0. Use WorkerProto::Op::AddToStore
    BuildPaths = 9,
    EnsurePath = 10,
    AddTempRoot = 11,
    AddIndirectRoot = 12,
    SyncWithGC = 13,
    FindRoots = 14,
    ExportPath = 16,   // obsolete
    QueryDeriver = 18, // obsolete
    SetOptions = 19,
    CollectGarbage = 20,
    QuerySubstitutablePathInfo = 21,
    QueryDerivationOutputs = 22, // obsolete
    QueryAllValidPaths = 23,
    QueryFailedPaths = 24,
    ClearFailedPaths = 25,
    QueryPathInfo = 26,
    ImportPaths = 27,                // obsolete
    QueryDerivationOutputNames = 28, // obsolete
    QueryPathFromHashPart = 29,
    QuerySubstitutablePathInfos = 30,
    QueryValidPaths = 31,
    QuerySubstitutablePaths = 32,
    QueryValidDerivers = 33,
    OptimiseStore = 34,
    VerifyStore = 35,
    BuildDerivation = 36,
    AddSignatures = 37,
    NarFromPath = 38,
    AddToStoreNar = 39,
    QueryMissing = 40,
    QueryDerivationOutputMap = 41,
    RegisterDrvOutput = 42,
    QueryRealisation = 43,
    AddMultipleToStore = 44,
    AddBuildLog = 45,
    BuildPathsWithResults = 46,
    AddPermRoot = 47,
}

/// Log verbosity. In the Nix wire protocol, the client requests a
/// verbosity level to the daemon, which in turns does not produce any
/// log below this verbosity.
#[derive(Debug, PartialEq, Primitive)]
pub enum Verbosity {
    LvlError = 0,
    LvlWarn = 1,
    LvlNotice = 2,
    LvlInfo = 3,
    LvlTalkative = 4,
    LvlChatty = 5,
    LvlDebug = 6,
    LvlVomit = 7,
}

/// Settings requested by the client. These settings are applied to a
/// connection to between the daemon and a client.
#[derive(Debug)]
pub struct ClientSettings {
    pub keep_failed: bool,
    pub keep_going: bool,
    pub try_fallback: bool,
    pub verbosity: Verbosity,
    pub max_build_jobs: u64,
    pub max_silent_time: u64,
    pub verbose_build: bool,
    pub build_cores: u64,
    pub use_substitutes: bool,
    /// Key/Value dictionary in charge of overriding the settings set
    /// by the Nix config file.
    ///
    /// Some settings can be safely overidden,
    /// some other require the user running the Nix client to be part
    /// of the trusted users group.
    pub overrides: HashMap<Vec<u8>, Vec<u8>>,
}

/// Reads the client settings from the wire.
///
/// Note: this function **only** reads the settings. It does not
/// manage the log state with the daemon. You'll have to do that on
/// your own. A minimal log implementation will consist in sending
/// back [STDERR_LAST] to the client after reading the client
/// settings.
pub async fn read_client_settings<R: AsyncReadExt + Unpin>(
    r: &mut R,
    client_version: u64,
) -> std::io::Result<ClientSettings> {
    let keep_failed = primitive::read_bool(r).await?;
    let keep_going = primitive::read_bool(r).await?;
    let try_fallback = primitive::read_bool(r).await?;
    let verbosity_uint = primitive::read_u64(r).await?;
    let verbosity = Verbosity::from_u64(verbosity_uint).ok_or_else(|| {
        Error::new(
            ErrorKind::Other,
            format!("Can't convert integer {} to verbosity", verbosity_uint),
        )
    })?;
    let max_build_jobs = primitive::read_u64(r).await?;
    let max_silent_time = primitive::read_u64(r).await?;
    _ = primitive::read_u64(r).await?;
    let verbose_build = primitive::read_bool(r).await?;
    _ = primitive::read_u64(r).await?;
    _ = primitive::read_u64(r).await?;
    let build_cores = primitive::read_u64(r).await?;
    let use_substitutes = primitive::read_bool(r).await?;
    let mut overrides = HashMap::new();
    if client_version >= 12 {
        let overrides_nb = primitive::read_u64(r).await?;
        for _ in 0..overrides_nb {
            let name = read_bytes(r, 0..1024).await?;
            let value = read_bytes(r, 0..1024).await?;
            overrides.insert(name, value);
        }
    }
    Ok(ClientSettings {
        keep_failed,
        keep_going,
        try_fallback,
        verbosity,
        max_build_jobs,
        max_silent_time,
        verbose_build,
        build_cores,
        use_substitutes,
        overrides,
    })
}

/// Read a worker [Operation] from the wire.
pub async fn read_op<R: AsyncReadExt + Unpin>(r: &mut R) -> std::io::Result<Operation> {
    let op_number = primitive::read_u64(r).await?;
    Operation::from_u64(op_number).ok_or(Error::new(
        ErrorKind::Other,
        format!("Invalid OP number {}", op_number),
    ))
}

/// Write a worker [Operation] to the wire.
pub async fn write_op<W: AsyncWriteExt + Unpin>(w: &mut W, op: &Operation) -> std::io::Result<()> {
    let op = Operation::to_u64(op).ok_or(Error::new(
        ErrorKind::Other,
        format!("Can't convert the OP {:?} to u64", op),
    ))?;
    w.write_u64(op).await
}

#[derive(Debug, PartialEq)]
pub enum Trust {
    Trusted,
    NotTrusted,
}

/// Write the worker [Trust] level to the wire.
///
/// Cpp Nix has a legacy third option: u8 0. This option is meant to
/// be used as a backward compatible measure. Since we're not
/// targetting protocol versions pre-dating the trust notion, we
/// decided not to implement it here.
pub async fn write_worker_trust_level<W>(conn: &mut W, t: Trust) -> std::io::Result<()>
where
    W: AsyncReadExt + AsyncWriteExt + Unpin + std::fmt::Debug,
{
    match t {
        Trust::Trusted => primitive::write_u64(conn, 1).await,
        Trust::NotTrusted => primitive::write_u64(conn, 2).await,
    }
}

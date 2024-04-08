use std::{
    collections::HashMap,
    io::{Error, ErrorKind},
};

use enum_primitive_derive::Primitive;
use num_traits::{FromPrimitive, ToPrimitive};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use crate::wire::{bytes, primitive};

use super::bytes::read_string;

static WORKER_MAGIC_1: u64 = 0x6e697863; // "nixc"
static WORKER_MAGIC_2: u64 = 0x6478696f; // "dxio"
pub static STDERR_LAST: u64 = 0x616c7473; // "alts"
/// Protocol version (1.37)
static PROTOCOL_VERSION: [u8; 8] = [37, 1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];

/// Max length of a Nix setting name/value. In bytes.
///
/// This value has been arbitrarily choosen after looking the nix.conf
/// manpage. Don't hesitate to increase it if it's too limiting.
pub static MAX_SETTING_SIZE: u64 = 1024;

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
#[derive(Debug, PartialEq)]
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
    pub overrides: HashMap<String, String>,
}

/// Reads the client settings from the wire.
///
/// Note: this function **only** reads the settings. It does not
/// manage the log state with the daemon. You'll have to do that on
/// your own. A minimal log implementation will consist in sending
/// back [STDERR_LAST] to the client after reading the client
/// settings.
///
/// FUTUREWORK: write serialization.
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
            ErrorKind::InvalidData,
            format!("Can't convert integer {} to verbosity", verbosity_uint),
        )
    })?;
    let max_build_jobs = primitive::read_u64(r).await?;
    let max_silent_time = primitive::read_u64(r).await?;
    _ = primitive::read_u64(r).await?; // obsolete useBuildHook
    let verbose_build = primitive::read_bool(r).await?;
    _ = primitive::read_u64(r).await?; // obsolete logType
    _ = primitive::read_u64(r).await?; // obsolete printBuildTrace
    let build_cores = primitive::read_u64(r).await?;
    let use_substitutes = primitive::read_bool(r).await?;
    let mut overrides = HashMap::new();
    if client_version >= 12 {
        let num_overrides = primitive::read_u64(r).await?;
        for _ in 0..num_overrides {
            let name = read_string(r, 0..MAX_SETTING_SIZE).await?;
            let value = read_string(r, 0..MAX_SETTING_SIZE).await?;
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

/// Structure used to hold the client socket connection and some
/// metadata about the connection.
#[derive(Debug)]
pub struct ClientConnection<R: AsyncReadExt + AsyncWriteExt + Unpin> {
    pub conn: R,
    pub version_minor: u64,
    pub client_settings: Option<ClientSettings>,
}

/// Performs the initial handshake. During the handshake, the client
/// will first send a magic u64, to which the daemon needs to respond
/// with another magic u64.
///
/// We then retrieve the client version, and discard a bunch of now
/// obsolete data.
///
/// # Arguments
///
/// * conn: connection with the Nix client.
/// * nix_version: semantic version of the Nix daemon. "2.18.2" for
///   instance.
/// * trusted: trust level of the Nix client.
pub async fn perform_init_handshake<'a, R: 'a>(
    mut conn: &'a mut R,
    nix_version: &str,
    trusted: Trust,
) -> std::io::Result<ClientConnection<&'a mut R>>
where
    &'a mut R: AsyncReadExt + AsyncWriteExt + Unpin,
{
    let worker_magic_1 = primitive::read_u64(&mut conn).await?;
    if worker_magic_1 != WORKER_MAGIC_1 {
        Err(std::io::Error::new(
            ErrorKind::InvalidData,
            format!("Incorrect worker magic number received: {}", worker_magic_1),
        ))
    } else {
        primitive::write_u64(&mut conn, WORKER_MAGIC_2).await?;
        conn.write_all(&PROTOCOL_VERSION).await?;
        conn.flush().await?;
        let client_version = primitive::read_u64(&mut conn).await?;
        if client_version < 0x10a {
            return Err(Error::new(
                ErrorKind::Unsupported,
                format!("The nix client version {} is too old", client_version),
            ));
        }
        let protocol_minor = client_version & 0x00ff;
        let _protocol_major = client_version & 0xff00;
        if protocol_minor >= 14 {
            // Obsolete CPU affinity.
            let read_affinity = primitive::read_u64(&mut conn).await?;
            if read_affinity != 0 {
                skip_8_bytes(&mut conn).await?;
            };
        }
        if protocol_minor >= 11 {
            // Obsolete reserveSpace
            skip_8_bytes(&mut conn).await?;
        }
        if protocol_minor >= 33 {
            // Nix version. We're plain lying, we're not Nix, but eh…
            // Setting it to the 2.3 lineage. Not 100% sure this is a
            // good idea.
            bytes::write_bytes(&mut conn, nix_version).await?;
            conn.flush().await?;
        }
        if protocol_minor >= 35 {
            write_worker_trust_level(&mut conn, trusted).await?;
        }
        Ok(ClientConnection {
            conn,
            version_minor: protocol_minor,
            client_settings: None,
        })
    }
}

async fn skip_8_bytes<R>(conn: &mut R) -> std::io::Result<()>
where
    R: AsyncReadExt + Unpin,
{
    let mut _discard_buffer = [0; 8];
    conn.read_exact(&mut _discard_buffer).await?;
    Ok(())
}

/// Read a worker [Operation] from the wire.
pub async fn read_op<R: AsyncReadExt + Unpin>(r: &mut R) -> std::io::Result<Operation> {
    let op_number = primitive::read_u64(r).await?;
    Operation::from_u64(op_number).ok_or(Error::new(
        ErrorKind::InvalidData,
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
    W: AsyncReadExt + AsyncWriteExt + Unpin,
{
    match t {
        Trust::Trusted => primitive::write_u64(conn, 1).await,
        Trust::NotTrusted => primitive::write_u64(conn, 2).await,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;
    use tokio_test::io::Builder;

    #[tokio::test]
    async fn test_init_hanshake() {
        let mut test_conn = tokio_test::io::Builder::new()
            .read(&WORKER_MAGIC_1.to_le_bytes())
            .write(&WORKER_MAGIC_2.to_le_bytes())
            .write(&PROTOCOL_VERSION)
            // Let's say the client is in sync with the daemon
            // protocol-wise
            .read(&PROTOCOL_VERSION)
            // cpu affinity
            .read(&vec![0; 8])
            // reservespace
            .read(&vec![0; 8])
            // version (size)
            .write(&vec![0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
            // version (data == 2.18.2 + padding)
            .write(&vec![50, 46, 49, 56, 46, 50, 0, 0])
            // Trusted (1 == client trusted
            .write(&vec![1, 0, 0, 0, 0, 0, 0, 0])
            .build();
        perform_init_handshake(&mut test_conn, "2.18.2", Trust::Trusted)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn test_read_client_settings_without_overrides() {
        // Client settings bits captured from a Nix 2.3.17 run w/ sockdump (protocol version 21).
        let wire_bits = hex!(
            "00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             02 00 00 00 00 00 00 00 \
             10 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             01 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             01 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00"
        );
        let mut mock = Builder::new().read(&wire_bits).build();
        let settings = read_client_settings(&mut mock, 21)
            .await
            .expect("should parse");
        let expected = ClientSettings {
            keep_failed: false,
            keep_going: false,
            try_fallback: false,
            verbosity: Verbosity::LvlNotice,
            max_build_jobs: 16,
            max_silent_time: 0,
            verbose_build: false,
            build_cores: 0,
            use_substitutes: true,
            overrides: HashMap::new(),
        };
        assert_eq!(settings, expected);
    }

    #[tokio::test]
    async fn test_read_client_settings_with_overrides() {
        // Client settings bits captured from a Nix 2.3.17 run w/ sockdump (protocol version 21).
        let wire_bits = hex!(
            "00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             02 00 00 00 00 00 00 00 \
             10 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             01 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             00 00 00 00 00 00 00 00 \
             01 00 00 00 00 00 00 00 \
             02 00 00 00 00 00 00 00 \
             0c 00 00 00 00 00 00 00 \
             61 6c 6c 6f 77 65 64 2d \
             75 72 69 73 00 00 00 00 \
             1e 00 00 00 00 00 00 00 \
             68 74 74 70 73 3a 2f 2f \
             62 6f 72 64 65 61 75 78 \
             2e 67 75 69 78 2e 67 6e \
             75 2e 6f 72 67 2f 00 00 \
             0d 00 00 00 00 00 00 00 \
             61 6c 6c 6f 77 65 64 2d \
             75 73 65 72 73 00 00 00 \
             0b 00 00 00 00 00 00 00 \
             6a 65 61 6e 20 70 69 65 \
             72 72 65 00 00 00 00 00"
        );
        let mut mock = Builder::new().read(&wire_bits).build();
        let settings = read_client_settings(&mut mock, 21)
            .await
            .expect("should parse");
        let overrides = HashMap::from([
            (
                String::from("allowed-uris"),
                String::from("https://bordeaux.guix.gnu.org/"),
            ),
            (String::from("allowed-users"), String::from("jean pierre")),
        ]);
        let expected = ClientSettings {
            keep_failed: false,
            keep_going: false,
            try_fallback: false,
            verbosity: Verbosity::LvlNotice,
            max_build_jobs: 16,
            max_silent_time: 0,
            verbose_build: false,
            build_cores: 0,
            use_substitutes: true,
            overrides,
        };
        assert_eq!(settings, expected);
    }
}

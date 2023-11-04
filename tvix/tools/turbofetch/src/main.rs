//! turbofetch retrieves narinfo files from cache.nixos.org as fast as possible.
//! It is designed to run many instances in parallel, each writing to a common S3 bucket.
//! The list of files to retrieve is read from a file consisting of nixbase32 format Nix store path hashes, without separators.
//! Each instance receives a range in this file (specified as indices in the listing file), and writes them to the target bucket after zstd compression.
//! Empirically, the resulting compression ratio is about 4.7x, on chunks of 20k narinfo files sorted by creation time.
//! No additional metadata is included, and the files within each group are concatenated in arbitrary order.
//! The narinfo format already starts each file off with a `StorePath` line, so this poses no practical issues.
//!
//! TODO(edef): any retries/error handling whatsoever

use bytes::Bytes;
use futures::{stream::FuturesUnordered, Stream, TryStreamExt};
use rusoto_core::ByteStream;
use rusoto_s3::{GetObjectRequest, PutObjectRequest, S3Client, S3};
use serde::Deserialize;
use std::{io::Write, mem, ops::Range, ptr};
use tokio::{
    io::{self, AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};

/// `keys` must be a slice of Nix path hashes in nixbase32 format
/// file contents is concatenated and returned in ~arbitrary order
/// any network error at all fails the entire batch, and there is no rate limiting
fn fetch(keys: &[[u8; 32]]) -> impl Stream<Item = io::Result<Bytes>> {
    // S3 supports only HTTP/1.1, but we can ease the pain somewhat by using HTTP pipelining.
    // It terminates the TCP connection after receiving 100 requests, so we chunk the keys up accordingly, and make one TCP connection for each chunk.
    keys.chunks(100)
        .map(|chunk| {
            const PREFIX: &[u8] = b"GET /nix-cache/";
            const SUFFIX: &[u8] = b".narinfo HTTP/1.1\nHost: s3.amazonaws.com\n\n";
            const LENGTH: usize = PREFIX.len() + 32 + SUFFIX.len();

            let mut request = Vec::with_capacity(LENGTH * 100);
            for key in chunk {
                request.extend_from_slice(PREFIX);
                request.extend_from_slice(key);
                request.extend_from_slice(SUFFIX);
            }

            (request, chunk.len())
        })
        .map(|(request, n)| async move {
            let (mut read, mut write) = TcpStream::connect("s3.amazonaws.com:80")
                .await?
                .into_split();

            let _handle = tokio::spawn(async move {
                let request = request;
                write.write_all(&request).await
            });

            let mut buffer = turbofetch::Buffer::new(512 * 1024);
            let mut bodies = vec![];

            for _ in 0..n {
                let body = turbofetch::parse_response(&mut read, &mut buffer).await?;
                bodies.extend_from_slice(body);
            }

            Ok::<_, io::Error>(Bytes::from(bodies))
        })
        .collect::<FuturesUnordered<_>>()
}

async fn get_range(
    s3: &'static S3Client,
    bucket: &str,
    range: Range<u64>,
) -> io::Result<Box<[[u8; 32]]>> {
    let resp = s3
        .get_object(GetObjectRequest {
            bucket: bucket.to_owned(),
            key: "narinfo_by_date.txt".to_owned(),
            range: Some(format!("bytes={}-{}", range.start * 32, range.end * 32 - 1)),
            ..GetObjectRequest::default()
        })
        .await
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    let mut body = vec![];
    resp.body
        .ok_or(io::ErrorKind::InvalidData)?
        .into_async_read()
        .read_to_end(&mut body)
        .await?;

    let body = exact_chunks(body.into_boxed_slice()).ok_or(io::ErrorKind::InvalidData)?;

    Ok(body)
}

fn exact_chunks(mut buf: Box<[u8]>) -> Option<Box<[[u8; 32]]>> {
    unsafe {
        let ptr = buf.as_mut_ptr();
        let len = buf.len();

        if len % 32 != 0 {
            return None;
        }

        let ptr = ptr as *mut [u8; 32];
        let len = len / 32;
        mem::forget(buf);

        Some(Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, len)))
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), lambda_runtime::Error> {
    let s3 = S3Client::new(rusoto_core::Region::UsEast1);
    let s3 = &*Box::leak(Box::new(s3));

    tracing_subscriber::fmt()
        .json()
        .with_max_level(tracing::Level::INFO)
        // this needs to be set to remove duplicated information in the log.
        .with_current_span(false)
        // this needs to be set to false, otherwise ANSI color codes will
        // show up in a confusing manner in CloudWatch logs.
        .with_ansi(false)
        // disabling time is handy because CloudWatch will add the ingestion time.
        .without_time()
        // remove the name of the function from every log entry
        .with_target(false)
        .init();

    lambda_runtime::run_with_streaming_response(lambda_runtime::service_fn(|event| func(s3, event)))
        .await
}

#[derive(Debug, Deserialize)]
struct Params {
    bucket: String,
    start: u64,
    end: u64,
}

#[tracing::instrument(skip(s3, event), fields(req_id = %event.context.request_id))]
async fn func(
    s3: &'static S3Client,
    event: lambda_runtime::LambdaEvent<
        aws_lambda_events::lambda_function_urls::LambdaFunctionUrlRequest,
    >,
) -> Result<hyper::Response<String>, lambda_runtime::Error> {
    let mut params = event.payload.body.ok_or("no body")?;

    if event.payload.is_base64_encoded {
        params = String::from_utf8(data_encoding::BASE64.decode(params.as_bytes())?)?;
    }

    let params: Params = serde_json::from_str(&params)?;

    if params.start >= params.end {
        return Err("nope".into());
    }

    let keys = get_range(s3, &params.bucket, params.start..params.end).await?;

    let zchunks = fetch(&keys)
        .try_fold(
            Box::new(zstd::Encoder::new(vec![], zstd::DEFAULT_COMPRESSION_LEVEL).unwrap()),
            |mut w, buf| {
                w.write_all(&buf).unwrap();
                async { Ok(w) }
            },
        )
        .await?;

    let zchunks = to_byte_stream(zchunks.finish().unwrap());

    tracing::info!("we got to put_object");

    s3.put_object(PutObjectRequest {
        bucket: params.bucket,
        key: format!("narinfo.zst/{:016x}-{:016x}", params.start, params.end),
        body: Some(zchunks),
        ..Default::default()
    })
    .await
    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    tracing::info!("… and it worked!");

    Ok(hyper::Response::builder()
        .header("Content-Type", "application/json")
        .body("OK".to_owned())
        .unwrap())
}

fn to_byte_stream(buffer: Vec<u8>) -> ByteStream {
    let size_hint = buffer.len();
    ByteStream::new_with_size(
        futures::stream::once(async { Ok(buffer.into()) }),
        size_hint,
    )
}

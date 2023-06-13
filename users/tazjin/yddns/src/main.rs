use anyhow::{anyhow, bail, Context, Result};
use crimp::Request;
use std::env;
use std::net::Ipv4Addr;
use tokio::runtime;
use tonic::codegen::InterceptedService;
use tonic::transport::channel::Channel;
use tonic::transport::Endpoint;
use yandex_cloud::yandex::cloud::dns::v1 as dns;
use yandex_cloud::yandex::cloud::dns::v1::dns_zone_service_client::DnsZoneServiceClient;
use yandex_cloud::{AuthInterceptor, TokenProvider};

type DnsClient<T> = DnsZoneServiceClient<InterceptedService<Channel, AuthInterceptor<T>>>;

/// Fetch the current IP from the given URL. It should be the URL of a
/// site that responds only with the IP in plain text, and nothing else.
fn get_current_ip(source: &str) -> Result<Ipv4Addr> {
    let response = Request::get(source)
        .send()
        .context("failed to fetch current IP")?
        .error_for_status(|resp| anyhow!("error response ({})", resp.status))
        .context("received error response for IP")?
        .as_string()?
        .body;

    Ok(response.trim().parse().with_context(|| {
        format!(
            "failed to parse IP address from response body: {}",
            response
        )
    })?)
}

/// Fetch the current address of the target record.
async fn fetch_current_record_addr<T: TokenProvider>(
    client: &mut DnsClient<T>,
    zone_id: &str,
    record_name: &str,
) -> Result<Ipv4Addr> {
    let req = dns::GetDnsZoneRecordSetRequest {
        dns_zone_id: zone_id.into(),
        name: record_name.into(),
        r#type: "A".into(),
    };

    let response = client
        .get_record_set(req)
        .await
        .context("failed to fetch current record set")?
        .into_inner();

    if response.data.len() != 1 {
        bail!(
            "expected exactly one record for 'A {}', but found {}",
            record_name,
            response.data.len()
        );
    }

    Ok(response.data[0]
        .parse()
        .context("failed to parse returned record")?)
}

/// Update the record with the new address, if required.
async fn update_record<T: TokenProvider>(
    client: &mut DnsClient<T>,
    zone_id: &str,
    record_name: &str,
    new_address: Ipv4Addr,
) -> Result<()> {
    let request = dns::UpsertRecordSetsRequest {
        dns_zone_id: zone_id.into(),
        replacements: vec![dns::RecordSet {
            name: record_name.into(),
            r#type: "A".into(),
            ttl: 3600, // 1 hour
            data: vec![new_address.to_string()],
        }],
        ..Default::default()
    };

    client
        .upsert_record_sets(request)
        .await
        .context("failed to update record")?;

    Ok(())
}

/// Compare the record with the expected value, and issue an update if
/// necessary.
async fn compare_update_record<T: TokenProvider>(
    client: &mut DnsClient<T>,
    zone_id: &str,
    record_name: &str,
    new_ip: Ipv4Addr,
) -> Result<()> {
    let old_ip = fetch_current_record_addr(client, zone_id, record_name).await?;

    if old_ip == new_ip {
        println!("IP address unchanged ({})", old_ip);
        return Ok(());
    }

    println!(
        "IP address changed: current record points to {}, but address is {}",
        old_ip, new_ip
    );

    update_record(client, zone_id, record_name, new_ip).await?;
    println!("successfully updated '{}' to 'A {}'", record_name, new_ip);

    Ok(())
}

// TODO(tazjin): missing TokenProvider impl for `String` in //ops/yandex-cloud
struct Token(String);
impl TokenProvider for Token {
    fn get_token(&mut self) -> &str {
        self.0.as_str()
    }
}

fn main() -> Result<()> {
    let target_zone_id =
        env::var("TARGET_ZONE").unwrap_or_else(|_| "dnsd0tif5mokfu0mg8i5".to_string());
    let target_record = env::var("TARGET_RECORD").unwrap_or_else(|_| "khtrsk".to_string());

    let token = Token(format!("Bearer {}", env::var("YANDEX_CLOUD_TOKEN")?));

    let current_ip = get_current_ip("http://ifconfig.me")?;
    println!("current IP address is '{}'", current_ip);

    let rt = runtime::Builder::new_current_thread()
        .enable_time()
        .enable_io()
        .build()?;

    rt.block_on(async move {
        let channel = Endpoint::from_static("https://dns.api.cloud.yandex.net")
            .connect()
            .await?;

        let mut client =
            DnsZoneServiceClient::with_interceptor(channel, AuthInterceptor::new(token));

        compare_update_record(&mut client, &target_zone_id, &target_record, current_ip).await
    })?;

    Ok(())
}

//! integration with yandex cloud translate api, for automatically
//! translating telegram posts.
//!
//! most of this module is concerned with handling the authentication
//! tokens for yandex cloud, as jwt signing needs to be handled
//! manually (none of the rust jwt libraries that i tried actually
//! work).

use anyhow::{anyhow, Context, Result};
use base64::prelude::BASE64_URL_SAFE_NO_PAD as B64;
use base64::Engine;
use lazy_static::lazy_static;
use ring::signature as sig;
use serde::Deserialize;
use serde_json::{json, Value};
use std::sync::Mutex;
use std::time::{Duration, SystemTime};

/// token exchange url (exchanging a signed jwt for an iam token
/// understood by the translation service)
const TOKEN_URL: &str = "https://iam.api.cloud.yandex.net/iam/v1/tokens";

/// translation endpoint
const TRANSLATE_URL: &str = "https://translate.api.cloud.yandex.net/translate/v2/translate";

/// describes the private key as downloaded from yandex, pem-encoded.
#[derive(Deserialize)]
struct AuthorizedKey {
    id: String,
    service_account_id: String,
    private_key: String,
}

/// cached iam token for yandex cloud
struct Token {
    token: String,
    expiry: SystemTime,
}

impl Token {
    fn is_expired(&self) -> bool {
        self.expiry < SystemTime::now()
    }
}

lazy_static! {
    static ref KEY_FILE: String =
        std::env::var("YANDEX_KEY_FILE").expect("`YANDEX_KEY_FILE` variable should be set");
    static ref CACHED_TOKEN: Mutex<Token> = {
        let token = refresh_token().expect("fetching initial translation token must not fail");
        Mutex::new(token)
    };
}

/// wrap all the authentication logic below into a single function.
fn refresh_token() -> Result<Token> {
    let file = std::fs::File::open(KEY_FILE.as_str())?;
    let key: AuthorizedKey = serde_json::from_reader(file)?;
    let jwt = sign_yandex_jwt(&key)?;
    let token = fetch_iam_token(&jwt)?;

    Ok(Token {
        token,
        expiry: SystemTime::now() + Duration::from_secs(3600),
    })
}

/// wrapper around the cached token that refreshes if required.
fn current_token() -> Result<String> {
    let mut token = CACHED_TOKEN
        .lock()
        .expect("thread operating on token should never fail");

    if token.is_expired() {
        println!("refreshing translation token");
        *token = refresh_token().context("refreshing translation token")?;
    }

    Ok(token.token.clone())
}

/// use openssl to read the pem-encoded key, as ring itself is not
/// capable of this.
fn read_pem_key(key: &AuthorizedKey) -> Result<sig::RsaKeyPair> {
    let rsa = openssl::rsa::Rsa::private_key_from_pem(key.private_key.as_bytes())
        .context("parsing RSA key")?;

    let der = rsa
        .private_key_to_der()
        .context("encoding key as DER for ring")?;

    sig::RsaKeyPair::from_der(&der).map_err(|err| anyhow!("decoding DER key in ring: {}", err))
}

/// manually construct and sign the jwt required to perform the
/// iam-token key exchange with yandex.
fn sign_yandex_jwt(key: &AuthorizedKey) -> Result<String> {
    let iat = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)?
        .as_secs();

    let header = json!({
        "typ": "JWT",
        "alg": "PS256",
        "kid": key.id,
    })
    .to_string();

    let payload = json!({
        "iss": key.service_account_id,
        "aud": TOKEN_URL,
        "iat": iat,
        "exp": iat + 60,
    })
    .to_string();

    let unsigned = format!("{}.{}", B64.encode(header), B64.encode(payload));
    let key_pair = read_pem_key(key)?;

    let rng = ring::rand::SystemRandom::new();
    let mut signature = vec![0; key_pair.public_modulus_len()];
    key_pair
        .sign(
            &sig::RSA_PSS_SHA256,
            &rng,
            unsigned.as_bytes(),
            &mut signature,
        )
        .map_err(|err| anyhow!("while signing JWT: {}", err))?;

    Ok(format!("{}.{}", unsigned, B64.encode(&signature)))
}

/// exchange the jwt for an iam token
fn fetch_iam_token(token: &str) -> Result<String> {
    #[derive(Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct TokenResponse {
        iam_token: String,
    }

    let response = crimp::Request::post(TOKEN_URL)
        .json(&json!({
            "jwt": token,
        }))?
        .send()?
        .error_for_status(|resp| {
            anyhow::anyhow!("{} ({})", String::from_utf8_lossy(&resp.body), resp.status)
        })?
        .as_json::<TokenResponse>()
        .context("deserialising IAM token")?;

    Ok(response.body.iam_token)
}

pub fn fetch_translation(message: &str) -> Result<String> {
    let request_body = json!({
        "folderId": "b1gq41rsbggeum4qafnh",
        "texts": [ message ],
        "targetLanguageCode": "en",
    });

    let response = crimp::Request::post(TRANSLATE_URL)
        .bearer_auth(&current_token()?)
        .context("adding 'Bearer' token")?
        .json(&request_body)
        .context("preparing JSON body")?
        .send()
        .context("failed to fetch translation from yandex")?
        .error_for_status(|resp| {
            anyhow!(
                "translation request failed: {} ({})",
                String::from_utf8_lossy(&resp.body),
                resp.status
            )
        })?
        .as_json::<Value>()?
        .body;

    let translation = response
        .get("translations")
        .ok_or_else(|| anyhow!("missing 'translations' key"))?
        .get(0)
        .ok_or_else(|| anyhow!("translations list is empty"))?
        .get("text")
        .ok_or_else(|| anyhow!("translation missing 'text' key"))?
        .as_str()
        .ok_or_else(|| anyhow!("'text' was not a string"))?;

    Ok(translation.to_string())
}

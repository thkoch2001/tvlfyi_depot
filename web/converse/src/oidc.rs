// Copyright (C) 2018-2021 Vincent Ambo <tazjin@tvl.su>
//
// This file is part of Converse.
//
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see
// <https://www.gnu.org/licenses/>.

//! This module provides authentication via OIDC compliant
//! authentication sources.
//!
//! Currently Converse only supports a single OIDC provider. Note that
//! this has so far only been tested with Office365.

use actix::prelude::*;
use crate::errors::*;
use crimp::Request;
use url::Url;
use url_serde;
use curl::easy::Form;

/// This structure represents the contents of an OIDC discovery
/// document.
#[derive(Deserialize, Debug, Clone)]
pub struct OidcConfig {
    #[serde(with = "url_serde")]
    authorization_endpoint: Url,
    token_endpoint: String,
    userinfo_endpoint: String,

    scopes_supported: Vec<String>,
    issuer: String,
}

#[derive(Clone, Debug)]
pub struct OidcExecutor {
    pub client_id: String,
    pub client_secret: String,
    pub redirect_uri: String,
    pub oidc_config: OidcConfig,
}

/// This struct represents the form response returned by an OIDC
/// provider with the `code`.
#[derive(Debug, Deserialize)]
pub struct CodeResponse {
    pub code: String,
}

/// This struct represents the data extracted from the ID token and
/// stored in the user's session.
#[derive(Debug, Serialize, Deserialize)]
pub struct Author {
    pub name: String,
    pub email: String,
}

impl Actor for OidcExecutor {
    type Context = Context<Self>;
}

/// Message used to request the login URL:
pub struct GetLoginUrl; // TODO: Add a nonce parameter stored in session.
message!(GetLoginUrl, String);

impl Handler<GetLoginUrl> for OidcExecutor {
    type Result = String;

    fn handle(&mut self, _: GetLoginUrl, _: &mut Self::Context) -> Self::Result {
        let mut url: Url = self.oidc_config.authorization_endpoint.clone();
        {
            let mut params = url.query_pairs_mut();
            params.append_pair("client_id", &self.client_id);
            params.append_pair("response_type", "code");
            params.append_pair("scope", "openid");
            params.append_pair("redirect_uri", &self.redirect_uri);
            params.append_pair("response_mode", "form_post");
        }
        return url.into_string();
    }
}

/// Message used to request the token from the returned code and
/// retrieve userinfo from the appropriate endpoint.
pub struct RetrieveToken(pub CodeResponse);
message!(RetrieveToken, Result<Author>);

#[derive(Debug, Deserialize)]
struct TokenResponse {
    access_token: String,
}

// TODO: This is currently hardcoded to Office365 fields.
#[derive(Debug, Deserialize)]
struct Userinfo {
    name: String,
    unique_name: String, // email in office365
}

impl Handler<RetrieveToken> for OidcExecutor {
    type Result = Result<Author>;

    fn handle(&mut self, msg: RetrieveToken, _: &mut Self::Context) -> Self::Result {
        debug!("Received OAuth2 code, requesting access_token");

        let mut form = Form::new();
        form.part("client_id").contents(&self.client_id.as_bytes())
            .add().expect("critical error: invalid form data");

        form.part("client_secret").contents(&self.client_secret.as_bytes())
            .add().expect("critical error: invalid form data");

        form.part("grant_type").contents("authorization_code".as_bytes())
            .add().expect("critical error: invalid form data");

        form.part("code").contents(&msg.0.code.as_bytes())
            .add().expect("critical error: invalid form data");

        form.part("redirect_uri").contents(&self.redirect_uri.as_bytes())
            .add().expect("critical error: invalid form data");

        let response = Request::post(&self.oidc_config.token_endpoint)
            .user_agent(concat!("converse-", env!("CARGO_PKG_VERSION")))?
            .form(form)
            .send()?;

        debug!("Received token response: {:?}", response);
        let token: TokenResponse = response.as_json()?.body;

        let bearer = format!("Bearer {}", token.access_token);
        let user: Userinfo = Request::get(&self.oidc_config.userinfo_endpoint)
            .user_agent(concat!("converse-", env!("CARGO_PKG_VERSION")))?
            .header("Authorization", &bearer)?
            .send()?
            .as_json()?.body;

        Ok(Author {
            name: user.name,
            email: user.unique_name,
        })
    }
}

/// Convenience function to attempt loading an OIDC discovery document
/// from a specified URL:
pub fn load_oidc(url: &str) -> Result<OidcConfig> {
    let config: OidcConfig = Request::get(url).send()?.as_json()?.body;
    Ok(config)
}

//! Atward implements TVL's redirection service, living at
//! atward.tvl.fyi
//!
//! This service is designed to be added as a search engine to web
//! browsers and attempts to send users to useful locations based on
//! their search query (falling back to another search engine).
use regex::Regex;
use rouille::{Request, Response};

#[cfg(test)]
mod tests;

/// A query handler supported by atward. It consists of a pattern on
/// which to match and trigger the query, and a function to execute
/// that returns the target URL.
struct Handler {
    /// Regular expression on which to match the query string.
    pattern: Regex,

    /// Function to construct the target URL. If the pattern matches,
    /// this is invoked with the captured matches and the entire URI.
    ///
    /// Returning `None` causes atward to fall through to the next
    /// query (and eventually to the default search engine).
    target: for<'s> fn(&Query, regex::Captures<'s>) -> Option<String>,
}

/// An Atward query supplied by a user.
#[derive(Debug, PartialEq)]
struct Query {
    /// Query string itself.
    query: String,
}

impl Query {
    fn from_request(req: &Request) -> Option<Query> {
        match req.get_param("q") {
            Some(query) => Some(Query { query }),
            None => return None,
        }
    }
}

#[cfg(test)]
impl From<&str> for Query {
    fn from(query: &str) -> Query {
        Query {
            query: query.to_string(),
        }
    }
}

/// Create a URL to a file (and, optionally, specific line) in cgit.
fn cgit_url(path: &str) -> String {
    if path.ends_with(".md") {
        format!("https://code.tvl.fyi/about/{}", path)
    } else {
        format!("https://code.tvl.fyi/tree/{}", path)
    }
}

/// Definition of all supported query handlers in atward.
fn handlers() -> Vec<Handler> {
    vec![
        // Bug IDs (e.g. b/123)
        Handler {
            pattern: Regex::new("^b/(?P<bug>\\d+)$").unwrap(),
            target: |_, captures| Some(format!("https://b.tvl.fyi/{}", &captures["bug"])),
        },
        // Changelists (e.g. cl/42)
        Handler {
            pattern: Regex::new("^cl/(?P<cl>\\d+)$").unwrap(),
            target: |_, captures| Some(format!("https://cl.tvl.fyi/{}", &captures["cl"])),
        },
        // Non-parameterised short hostnames should redirect to $host.tvl.fyi
        Handler {
            pattern: Regex::new("^(?P<host>b|cl|cs|code|at|todo)$").unwrap(),
            target: |_, captures| Some(format!("https://{}.tvl.fyi/", &captures["host"])),
        },
        // Depot revisions (e.g. r/3002)
        Handler {
            pattern: Regex::new("^r/(?P<rev>\\d+)$").unwrap(),
            target: |_, captures| {
                Some(format!(
                    "https://code.tvl.fyi/commit/?id=refs/r/{}",
                    &captures["rev"]
                ))
            },
        },
        // Depot paths (e.g. //web/atward or //ops/nixos/whitby/default.nix)
        // TODO(tazjin): Add support for specifying lines in a query parameter
        Handler {
            pattern: Regex::new("^//(?P<path>[a-zA-Z].*)?$").unwrap(),
            target: |_, captures| {
                // Pass an empty string if the path is missing, to
                // redirect to the depot root.
                let path = captures.name("path").map(|m| m.as_str()).unwrap_or("");
                Some(cgit_url(path))
            },
        },
    ]
}

/// Attempt to match against all known query types, and return the
/// destination URL if one is found.
fn dispatch(handlers: &[Handler], query: &Query) -> Option<String> {
    for handler in handlers {
        if let Some(captures) = handler.pattern.captures(&query.query) {
            if let Some(destination) = (handler.target)(query, captures) {
                return Some(destination);
            }
        }
    }

    None
}

/// Return the opensearch.xml file which is required for adding atward
/// as a search engine in Firefox.
fn opensearch() -> Response {
    Response::text(include_str!("opensearch.xml"))
        .with_unique_header("Content-Type", "application/opensearchdescription+xml")
}

/// Render the atward index page which gives users some information
/// about how to use the service.
fn index() -> Response {
    Response::html(include_str!(env!("ATWARD_INDEX_HTML")))
}

/// Render the fallback page which informs users that their query is
/// unsupported.
fn fallback() -> Response {
    Response::text("error for emphasis that i am angery and the query whimchst i angery atward")
        .with_status_code(404)
}

fn main() {
    let queries = handlers();
    let address = std::env::var("ATWARD_LISTEN_ADDRESS")
        .expect("ATWARD_LISTEN_ADDRESS environment variable must be set");

    rouille::start_server(&address, move |request| {
        rouille::log(&request, std::io::stderr(), || {
            if request.url() == "/opensearch.xml" {
                return opensearch();
            }

            let query = match Query::from_request(&request) {
                Some(q) => q,
                None => return index(),
            };

            match dispatch(&queries, &query) {
                None => fallback(),
                Some(destination) => Response::redirect_303(destination),
            }
        })
    });
}

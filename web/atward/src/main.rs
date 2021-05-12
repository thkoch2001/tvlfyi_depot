//! Atward implements TVL's redirection service, living at
//! atward.tvl.fyi
//!
//! This service is designed to be added as a search engine to web
//! browsers and attempts to send users to useful locations based on
//! their search query (falling back to another search engine).
use regex::Regex;
use rouille::input::cookies;
use rouille::{Request, Response};

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

    /// Should Sourcegraph be used instead of cgit?
    cs: bool,
}

/// Helper function for setting a parameter based on a query
/// parameter.
fn query_setting(req: &Request, config: &mut bool, param: &str) {
    match req.get_param(param) {
        Some(s) if s == "true" => *config = true,
        Some(s) if s == "false" => *config = false,
        _ => {}
    }
}

impl Query {
    fn from_request(req: &Request) -> Option<Query> {
        // First extract the actual search query ...
        let mut query = match req.get_param("q") {
            Some(query) => Query { query, cs: false },
            None => return None,
        };

        // ... then apply settings to it. Settings in query parameters
        // take precedence over cookies.
        for cookie in cookies(req) {
            match cookie {
                ("cs", "true") => {
                    query.cs = true;
                }
                _ => {}
            }
        }

        query_setting(req, &mut query.cs, "cs");

        Some(query)
    }
}

#[cfg(test)]
impl From<&str> for Query {
    fn from(query: &str) -> Query {
        Query {
            query: query.to_string(),
            cs: false,
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

/// Create a URL to a path in Sourcegraph.
fn sourcegraph_path_url(path: &str) -> String {
    format!("https://cs.tvl.fyi/depot/-/tree/{}", path)
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
        // Depot paths (e.g. //web/atward or //ops/nixos/whitby/default.nix)
        // TODO(tazjin): Add support for specifying lines in a query parameter
        Handler {
            pattern: Regex::new("^//(?P<path>[a-zA-Z].*)$").unwrap(),
            target: |query, captures| {
                if query.cs {
                    Some(sourcegraph_path_url(&captures["path"]))
                } else {
                    Some(cgit_url(&captures["path"]))
                }
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
    Response::html(include_str!("index.html"))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bug_query() {
        assert_eq!(
            dispatch(&handlers(), &"b/42".into()),
            Some("https://b.tvl.fyi/42".to_string())
        );

        assert_eq!(
            dispatch(&handlers(), &"something only mentioning b/42".into()),
            None,
        );
        assert_eq!(dispatch(&handlers(), &"b/invalid".into()), None,);
    }

    #[test]
    fn cl_query() {
        assert_eq!(
            dispatch(&handlers(), &"cl/42".into()),
            Some("https://cl.tvl.fyi/42".to_string())
        );

        assert_eq!(
            dispatch(&handlers(), &"something only mentioning cl/42".into()),
            None,
        );
        assert_eq!(dispatch(&handlers(), &"cl/invalid".into()), None,);
    }

    #[test]
    fn depot_path_cgit_query() {
        assert_eq!(
            dispatch(&handlers(), &"//web/atward/default.nix".into()),
            Some("https://code.tvl.fyi/tree/web/atward/default.nix".to_string()),
        );

        assert_eq!(
            dispatch(&handlers(), &"//nix/readTree/README.md".into()),
            Some("https://code.tvl.fyi/about/nix/readTree/README.md".to_string()),
        );

        assert_eq!(dispatch(&handlers(), &"/not/a/depot/path".into()), None);
    }

    #[test]
    fn depot_path_sourcegraph_query() {
        assert_eq!(
            dispatch(
                &handlers(),
                &Query {
                    query: "//web/atward/default.nix".to_string(),
                    cs: true,
                }
            ),
            Some("https://cs.tvl.fyi/depot/-/tree/web/atward/default.nix".to_string()),
        );

        assert_eq!(
            dispatch(
                &handlers(),
                &Query {
                    query: "/not/a/depot/path".to_string(),
                    cs: true,
                }
            ),
            None
        );
    }

    #[test]
    fn request_to_query() {
        assert_eq!(
            Query::from_request(&Request::fake_http("GET", "/?q=b%2F42", vec![], vec![]))
                .expect("request should parse to a query"),
            Query {
                query: "b/42".to_string(),
                cs: false,
            },
        );

        assert_eq!(
            Query::from_request(&Request::fake_http("GET", "/", vec![], vec![])),
            None
        );
    }

    #[test]
    fn settings_from_cookie() {
        assert_eq!(
            Query::from_request(&Request::fake_http(
                "GET",
                "/?q=b%2F42",
                vec![("Cookie".to_string(), "cs=true;".to_string())],
                vec![]
            ))
            .expect("request should parse to a query"),
            Query {
                query: "b/42".to_string(),
                cs: true,
            },
        );
    }

    #[test]
    fn settings_from_query_parameter() {
        assert_eq!(
            Query::from_request(&Request::fake_http(
                "GET",
                "/?q=b%2F42&cs=true",
                vec![],
                vec![]
            ))
            .expect("request should parse to a query"),
            Query {
                query: "b/42".to_string(),
                cs: true,
            },
        );

        // Query parameter should override cookie
        assert_eq!(
            Query::from_request(&Request::fake_http(
                "GET",
                "/?q=b%2F42&cs=false",
                vec![("Cookie".to_string(), "cs=true;".to_string())],
                vec![]
            ))
            .expect("request should parse to a query"),
            Query {
                query: "b/42".to_string(),
                cs: false,
            },
        );
    }
}

//! Atward implements TVL's redirection service, living at
//! atward.tvl.fyi
//!
//! This service is designed to be added as a search engine to web
//! browsers and attempts to send users to useful locations based on
//! their search query (falling back to another search engine).
use regex::Regex;
use rouille::Response;

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
struct Query {
    /// Query string itself.
    query: String,
}

impl Query {
    fn from_request(req: &rouille::Request) -> Option<Query> {
        let query = match req.get_param("q") {
            Some(q) => q,
            None => return None,
        };

        Some(Query { query })
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
        // Depot paths (e.g. //web/atward or //ops/nixos/whitby/default.nix)
        // TODO(tazjin): Add support for specifying lines in a query parameter
        Handler {
            pattern: Regex::new("^//(?P<path>[a-zA-Z].*)$").unwrap(),
            target: |_, captures| Some(cgit_url(&captures["path"])),
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
            let query = match Query::from_request(&request) {
                Some(q) => q,
                None => return fallback(),
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
    fn depot_path_query() {
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
}

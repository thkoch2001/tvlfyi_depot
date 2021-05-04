//! Atward implements TVL's redirection service, living at
//! atward.tvl.fyi
//!
//! This service is designed to be added as a search engine to web
//! browsers and attempts to send users to useful locations based on
//! their search query (falling back to another search engine).
use regex::Regex;
use rouille::Response;

/// A query type supported by atward. It consists of a pattern on
/// which to match and trigger the query, and a function to execute
/// that returns the target URL.
struct Query {
    /// Regular expression on which to match the query string.
    pattern: Regex,

    /// Function to construct the target URL. If the pattern matches,
    /// this is invoked with the captured matches and the entire URI.
    ///
    /// Returning `None` causes atward to fall through to the next
    /// query (and eventually to the default search engine).
    target: for<'s> fn(&'s str, regex::Captures<'s>) -> Option<String>,
}

/// Create a URL to a file (and, optionally, specific line) in cgit.
fn cgit_url(path: &str) -> String {
    if path.ends_with(".md") {
        format!("https://code.tvl.fyi/about/{}", path)
    } else {
        format!("https://code.tvl.fyi/tree/{}", path)
    }
}

/// Definition of all supported queries in atward.
fn queries() -> Vec<Query> {
    vec![
        // Bug IDs (e.g. b/123)
        Query {
            pattern: Regex::new("^b/(?P<bug>\\d+)$").unwrap(),
            target: |_, captures| Some(format!("https://b.tvl.fyi/{}", &captures["bug"])),
        },
        // Changelists (e.g. cl/42)
        Query {
            pattern: Regex::new("^cl/(?P<cl>\\d+)$").unwrap(),
            target: |_, captures| Some(format!("https://cl.tvl.fyi/{}", &captures["cl"])),
        },
        // Depot paths (e.g. //web/atward or //ops/nixos/whitby/default.nix)
        // TODO(tazjin): Add support for specifying lines in a query parameter
        Query {
            pattern: Regex::new("^//(?P<path>[a-zA-Z].*)$").unwrap(),
            target: |_, captures| Some(cgit_url(&captures["path"])),
        },
    ]
}

/// Attempt to match against all known query types, and return the
/// destination URL if one is found.
fn dispatch(queries: &[Query], uri: &str) -> Option<String> {
    for query in queries {
        if let Some(captures) = query.pattern.captures(uri) {
            if let Some(destination) = (query.target)(uri, captures) {
                return Some(destination);
            }
        }
    }

    None
}

fn fallback() -> Response {
    Response::text("no match for atward whimchst query").with_status_code(404)
}

fn main() {
    let queries = queries();
    let address = std::env::var("ATWARD_LISTEN_ADDRESS")
        .expect("ATWARD_LISTEN_ADDRESS environment variable must be set");

    rouille::start_server(&address, move |request| {
        rouille::log(&request, std::io::stderr(), || {
            match dispatch(&queries, &request.url()) {
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
            dispatch(&queries(), "b/42"),
            Some("https://b.tvl.fyi/42".to_string())
        );

        assert_eq!(dispatch(&queries(), "something only mentioning b/42"), None,);
        assert_eq!(dispatch(&queries(), "b/invalid"), None,);
    }

    #[test]
    fn cl_query() {
        assert_eq!(
            dispatch(&queries(), "cl/42"),
            Some("https://cl.tvl.fyi/42".to_string())
        );

        assert_eq!(
            dispatch(&queries(), "something only mentioning cl/42"),
            None,
        );
        assert_eq!(dispatch(&queries(), "cl/invalid"), None,);
    }

    #[test]
    fn depot_path_query() {
        assert_eq!(
            dispatch(&queries(), "//web/atward/default.nix"),
            Some("https://code.tvl.fyi/tree/web/atward/default.nix".to_string()),
        );

        assert_eq!(
            dispatch(&queries(), "//nix/readTree/README.md"),
            Some("https://code.tvl.fyi/about/nix/readTree/README.md".to_string()),
        );

        assert_eq!(dispatch(&queries(), "/not/a/depot/path"), None);
    }
}

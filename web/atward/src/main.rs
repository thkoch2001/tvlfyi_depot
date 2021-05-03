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

/// Definition of all supported queries in atward.
fn queries() -> Vec<Query> {
    vec![
        // Bug IDs (e.g. b/123)
        Query {
            pattern: Regex::new("^b/(?P<bug>\\d+)$").unwrap(),
            target: |_, captures| Some(format!("https://b.tvl.fyi/{}", &captures["bug"])),
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
    let port = std::env::var("ATWARD_PORT").unwrap_or("28973".to_string());
    let address = format!("0.0.0.0:{}", port);

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
}

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
fn depot_root_cgit_query() {
    assert_eq!(
        dispatch(
            &handlers(),
            &Query {
                query: "//".to_string(),
                cs: false,
            }
        ),
        Some("https://code.tvl.fyi/tree/".to_string()),
    );
}

#[test]
fn plain_host_queries() {
    assert_eq!(
        dispatch(&handlers(), &"cs".into()),
        Some("https://cs.tvl.fyi/".to_string()),
    );

    assert_eq!(
        dispatch(&handlers(), &"cl".into()),
        Some("https://cl.tvl.fyi/".to_string()),
    );

    assert_eq!(
        dispatch(&handlers(), &"b".into()),
        Some("https://b.tvl.fyi/".to_string()),
    );

    assert_eq!(
        dispatch(&handlers(), &"todo".into()),
        Some("https://todo.tvl.fyi/".to_string()),
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

#[test]
fn depot_revision_query() {
    assert_eq!(
        dispatch(&handlers(), &"r/3002".into()),
        Some("https://code.tvl.fyi/commit/?id=refs/r/3002".to_string())
    );

    assert_eq!(
        dispatch(&handlers(), &"something only mentioning r/3002".into()),
        None,
    );

    assert_eq!(dispatch(&handlers(), &"r/invalid".into()), None,);
}

use std::{path::PathBuf, str::FromStr};
use url::Url;

pub enum FetchTreeArgs {
    File {
        last_modified: Option<u64>,
        nar_hash: Option<String>,
        rev: Option<String>,
        rev_count: Option<u64>,
        url: Url,
    },
    Git {
        all_refs: bool,
        export_ignore: bool,
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
        r#ref: Option<String>,
        rev: Option<String>,
        shallow: bool,
        submodules: bool,
        url: Url,
        verify_commit: bool,
    },
    GitHub {
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
        r#ref: Option<String>,
        rev: Option<String>,
    },
    GitLab {
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
        r#ref: Option<String>,
        rev: Option<String>,
    },
    Indirect {
        id: String,
        r#ref: Option<String>,
        rev: Option<String>,
    },
    Mercurial {
        r#ref: Option<String>,
        rev: Option<String>,
    },
    Path {
        last_modified: Option<u64>,
        nar_hash: Option<String>,
        path: PathBuf,
        rev: Option<String>,
        rev_count: Option<u64>,
    },
    SourceHut {
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
        r#ref: Option<String>,
        rev: Option<String>,
    },
    Tarball {
        last_modified: Option<u64>,
        nar_hash: Option<String>,
        rev: Option<String>,
        rev_count: Option<u64>,
        url: Url,
    },
}

enum FetchType {
    File,
    Git,
    GitHub,
    GitLab,
    Indirect,
    Path,
    SourceHut,
    Tarball,
}

impl FromStr for FetchType {
    type Err = FetchTreeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "path" => FetchType::Path,
            "file" => FetchType::File,
            "tarball" => FetchType::Tarball,
            "git" => FetchType::Git,
            "github" => FetchType::GitHub,
            "gitlab" => FetchType::GitLab,
            "sourcehut" => FetchType::SourceHut,
            "indirect" => FetchType::Indirect,
            // TODO: No Mercurial support: https://github.com/search?q=path%3A%2F%5Eflake.lock%24%2F+%28%2F%22hg%22%2F+OR+%2F%22mercurial%22%2F%29&type=code
            _ => return Err(FetchTreeError::UnsupportedType(s.to_string())),
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FetchTreeError {
    #[error("failed to parse URL: {0}")]
    UrlParseError(#[from] url::ParseError),
    #[error("unsupported input type: {0}")]
    UnsupportedType(String),
}

pub fn parse_url(url: &str) -> Result<FetchTreeArgs, FetchTreeError> {
    let mut url = Url::parse(url)?;
    let mut newprotocol = None;

    // fetchers use fetch_type+protocol or if fetch_type is missing, it's guessed.
    let fetch_type = if let Some((r#type, protocol)) = url.scheme().split_once("+") {
        newprotocol = Some(protocol.to_string());
        r#type.parse()?
    } else {
        match url.scheme() {
            // Direct schemes
            "path" => FetchType::Path,
            "github" => FetchType::GitHub,
            "sourcehut" => FetchType::SourceHut,
            // Git prefixed schemes
            "git" => FetchType::Git,
            // Check for tarball file extensions
            _ if [
                ".zip", ".tar", ".tgz", ".tar.gz", ".tar.xz", ".tar.bz2", ".tar.zst",
            ]
            .iter()
            .any(|ext| url.path().ends_with(ext)) =>
            {
                FetchType::Tarball
            }
            // File/tarball schemes
            _ => FetchType::File,
        }
    };

    if let Some(protocol) = newprotocol {
        // url.set_scheme is too restrictive: https://github.com/servo/rust-url/pull/768
        let mut url_str = url.to_string();
        url_str.replace_range(..url.scheme().len(), &protocol);
        url = Url::parse(&url_str)?;
    }

    // Extract common query parameters
    let mut query_pairs = std::collections::HashMap::new();
    for (k, v) in url.query_pairs() {
        query_pairs.entry(k).or_insert(v);
    }
    // Extract common URL parameters
    Ok(match fetch_type {
        FetchType::File => {
            let nar_hash = query_pairs.get("narHash").map(|s| s.to_string());
            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let rev_count = query_pairs.get("revCount").and_then(|s| s.parse().ok());
            let last_modified = query_pairs.get("lastModified").and_then(|s| s.parse().ok());

            FetchTreeArgs::File {
                url,
                nar_hash,
                rev,
                rev_count,
                last_modified,
            }
        }
        FetchType::Tarball => {
            let nar_hash = query_pairs.get("narHash").map(|s| s.to_string());
            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let rev_count = query_pairs.get("revCount").and_then(|s| s.parse().ok());
            let last_modified = query_pairs.get("lastModified").and_then(|s| s.parse().ok());

            FetchTreeArgs::Tarball {
                url,
                nar_hash,
                rev,
                rev_count,
                last_modified,
            }
        }
        FetchType::Indirect => {
            let r#ref = query_pairs.get("ref").map(|s| s.to_string());
            let rev = query_pairs.get("rev").map(|s| s.to_string());

            FetchTreeArgs::Indirect {
                id: url.path().to_string(),
                rev,
                r#ref,
            }
        }
        FetchType::Git => {
            let r#ref = query_pairs.get("ref").map(|s| s.to_string());
            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let keytype = query_pairs.get("keytype").map(|s| s.to_string());
            let public_key = query_pairs.get("publicKey").map(|s| s.to_string());
            let public_keys = query_pairs
                .get("publicKeys")
                .map(|s| s.split(',').map(|s| s.to_string()).collect::<Vec<_>>());
            let submodules = query_pairs
                .get("submodules")
                .map(|v| v == "1" || v.to_lowercase() == "true")
                .unwrap_or(false);
            let shallow = query_pairs
                .get("shallow")
                .map(|v| v == "1" || v.to_lowercase() == "true")
                .unwrap_or(false);
            let export_ignore = query_pairs
                .get("exportIgnore")
                .map(|v| v == "1" || v.to_lowercase() == "true")
                .unwrap_or(false);
            let all_refs = query_pairs
                .get("allRefs")
                .map(|v| v == "1" || v.to_lowercase() == "true")
                .unwrap_or(false);
            let verify_commit = query_pairs
                .get("verifyCommit")
                .map(|v| v == "1" || v.to_lowercase() == "true")
                .unwrap_or(false);

            FetchTreeArgs::Git {
                url,
                r#ref,
                rev,
                keytype,
                public_key,
                public_keys,
                shallow,
                submodules,
                export_ignore,
                all_refs,
                verify_commit,
            }
        }
        FetchType::Path => {
            let nar_hash = query_pairs.get("narHash").map(|s| s.to_string());
            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let rev_count = query_pairs.get("revCount").and_then(|s| s.parse().ok());
            let last_modified = query_pairs.get("lastModified").and_then(|s| s.parse().ok());

            FetchTreeArgs::Path {
                path: PathBuf::from(url.path()),
                rev,
                nar_hash,
                rev_count,
                last_modified,
            }
        }
        FetchType::GitHub => {
            let path_segments: Vec<&str> =
                url.path().trim_start_matches('/').splitn(3, '/').collect();
            if path_segments.len() < 2 {
                return Err(FetchTreeError::UnsupportedType(
                    "GitHub URLs must contain owner and repo".to_string(),
                ));
            }

            // Check for branch/tag conflicts
            let path_ref = if path_segments.len() > 2 {
                Some(path_segments[2].to_string())
            } else {
                None
            };

            if path_ref.is_some() && query_pairs.contains_key("ref") {
                return Err(FetchTreeError::UnsupportedType(
                    "URL contains multiple branch/tag names".to_string(),
                ));
            }

            let r#ref = if let Some(path_ref) = path_ref {
                Some(path_ref)
            } else {
                query_pairs.get("ref").map(|s| s.to_string())
            };

            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let keytype = query_pairs.get("keytype").map(|s| s.to_string());
            let public_key = query_pairs.get("publicKey").map(|s| s.to_string());
            let public_keys = query_pairs
                .get("publicKeys")
                .map(|s| s.split(',').map(|s| s.to_string()).collect::<Vec<_>>());

            FetchTreeArgs::GitHub {
                r#ref,
                rev,
                keytype,
                public_key,
                public_keys,
            }
        }
        FetchType::GitLab => {
            let path_segments: Vec<&str> =
                url.path().trim_start_matches('/').splitn(3, '/').collect();
            if path_segments.len() < 2 {
                return Err(FetchTreeError::UnsupportedType(
                    "GitLab URLs must contain owner and repo".to_string(),
                ));
            }

            // Check for branch/tag conflicts
            let path_ref = if path_segments.len() > 2 {
                Some(path_segments[2].to_string())
            } else {
                None
            };

            if path_ref.is_some() && query_pairs.contains_key("ref") {
                return Err(FetchTreeError::UnsupportedType(
                    "URL contains multiple branch/tag names".to_string(),
                ));
            }

            let r#ref = if let Some(path_ref) = path_ref {
                Some(path_ref)
            } else {
                query_pairs.get("ref").map(|s| s.to_string())
            };

            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let keytype = query_pairs.get("keytype").map(|s| s.to_string());
            let public_key = query_pairs.get("publicKey").map(|s| s.to_string());
            let public_keys = query_pairs
                .get("publicKeys")
                .map(|s| s.split(',').map(|s| s.to_string()).collect::<Vec<_>>());

            FetchTreeArgs::GitLab {
                r#ref,
                rev,
                keytype,
                public_key,
                public_keys,
            }
        }
        FetchType::SourceHut => {
            let r#ref = query_pairs.get("ref").map(|s| s.to_string());
            let rev = query_pairs.get("rev").map(|s| s.to_string());
            let keytype = query_pairs.get("keytype").map(|s| s.to_string());
            let public_key = query_pairs.get("publicKey").map(|s| s.to_string());
            let public_keys = query_pairs
                .get("publicKeys")
                .map(|s| s.split(',').map(|s| s.to_string()).collect::<Vec<_>>());

            FetchTreeArgs::SourceHut {
                r#ref,
                rev,
                keytype,
                public_key,
                public_keys,
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_git_urls() {
        let input = "git+https://github.com/lichess-org/fishnet?submodules=1";
        match parse_url(input) {
            Ok(FetchTreeArgs::Git {
                submodules,
                shallow,
                export_ignore,
                all_refs,
                verify_commit,
                ..
            }) => {
                assert!(submodules);
                assert!(!shallow);
                assert!(!export_ignore);
                assert!(!all_refs);
                assert!(!verify_commit);
            }
            _ => panic!("Expected Git input type"),
        }

        let input = "git+file:///home/user/project?ref=fa1e2d23a22";
        match parse_url(input) {
            Ok(FetchTreeArgs::Git { r#ref, rev, .. }) => {
                assert_eq!(r#ref, Some("fa1e2d23a22".to_string()));
                assert_eq!(rev, None);
            }
            _ => panic!("Expected Git input type"),
        }

        let input = "git+git://github.com/someuser/my-repo?rev=v1.2.3";
        match parse_url(input) {
            Ok(FetchTreeArgs::Git { rev, .. }) => {
                assert_eq!(rev, Some("v1.2.3".to_string()));
            }
            _ => panic!("Expected Git input type"),
        }
    }

    #[test]
    fn test_github_urls() {
        let input = "github:snowfallorg/lib?ref=v2.1.1";
        match parse_url(input) {
            Ok(FetchTreeArgs::GitHub { r#ref, rev, .. }) => {
                assert_eq!(r#ref, Some("v2.1.1".to_string()));
                assert_eq!(rev, None);
            }
            _ => panic!("Expected GitHub input type"),
        }

        let input = "github:aarowill/base16-alacritty";
        match parse_url(input) {
            Ok(FetchTreeArgs::GitHub { r#ref, rev, .. }) => {
                assert_eq!(r#ref, None);
                assert_eq!(rev, None);
            }
            _ => panic!("Expected GitHub input type"),
        }

        let input = "github:a/b/c?ref=yyy";
        match parse_url(input) {
            Ok(_) => panic!("Expected error for multiple identifiers"),
            Err(FetchTreeError::UnsupportedType(_)) => (),
            _ => panic!("Expected UnsupportedType error"),
        }

        let input = "github:a";
        match parse_url(input) {
            Ok(_) => panic!("Expected error for missing repo"),
            Err(FetchTreeError::UnsupportedType(_)) => (),
            _ => panic!("Expected UnsupportedType error"),
        }

        let input = "github:a/b/master/extra";
        match parse_url(input) {
            Ok(FetchTreeArgs::GitHub { r#ref, rev, .. }) => {
                assert_eq!(r#ref, Some("master/extra".to_string()));
                assert_eq!(rev, None);
            }
            _ => panic!("Expected GitHub input type"),
        }

        let input = "github:a/b";
        match parse_url(input) {
            Ok(FetchTreeArgs::GitHub { r#ref, .. }) => {
                assert_eq!(r#ref, None);
            }
            _ => panic!("Expected GitHub input type"),
        }
    }

    #[test]
    fn test_file_urls() {
        let input = "https://www.shutterstock.com/image-photo/young-potato-isolated-on-white-260nw-630239534.jpg";
        match parse_url(input) {
            Ok(FetchTreeArgs::File {
                url,
                nar_hash,
                rev,
                rev_count,
                last_modified,
            }) => {
                assert_eq!(url.to_string(), input);
                assert_eq!(nar_hash, None);
                assert_eq!(rev, None);
                assert_eq!(rev_count, None);
                assert_eq!(last_modified, None);
            }
            _ => panic!("Expected File input type"),
        }
    }

    #[test]
    fn test_path_urls() {
        let input = "path:./go";
        match parse_url(input) {
            Ok(FetchTreeArgs::Path {
                path,
                rev,
                nar_hash,
                rev_count,
                last_modified,
            }) => {
                assert_eq!(path.to_str().unwrap(), "./go");
                assert_eq!(rev, None);
                assert_eq!(nar_hash, None);
                assert_eq!(rev_count, None);
                assert_eq!(last_modified, None);
            }
            _ => panic!("Expected Path input type"),
        }

        let input = "~/Downloads/a.zip";
        match parse_url(input) {
            Ok(_) => panic!("Expected error for invalid URL format"),
            Err(FetchTreeError::UrlParseError(_)) => (),
            _ => panic!("Expected UrlParseError error"),
        }
    }
}

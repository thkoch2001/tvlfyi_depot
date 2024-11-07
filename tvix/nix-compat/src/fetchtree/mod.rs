use std::{path::PathBuf, str::FromStr};
use url::Url;

// TODO: ref, rev don't make sense for some of these cases, should be moved into separate return value
pub enum FetchTreeArgs {
    Path {
        path: PathBuf,
        rev: Option<String>,
        nar_hash: Option<String>,
        rev_count: Option<u64>,
        last_modified: Option<u64>,
    },
    File {
        url: Url,
        nar_hash: Option<String>,
        rev: Option<String>,
        rev_count: Option<u64>,
        last_modified: Option<u64>,
    },
    Tarball {
        url: Url,
        nar_hash: Option<String>,
        rev: Option<String>,
        rev_count: Option<u64>,
        last_modified: Option<u64>,
    },
    Git {
        url: Url,
        r#ref: Option<String>,
        rev: Option<String>,
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
        shallow: bool,
        submodules: bool,
        export_ignore: bool,
        all_refs: bool,
        verify_commit: bool,
    },
    GitHub {
        r#ref: Option<String>,
        rev: Option<String>,
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
    },
    GitLab {
        r#ref: Option<String>,
        rev: Option<String>,
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
    },
    SourceHut {
        r#ref: Option<String>,
        rev: Option<String>,
        keytype: Option<String>,
        public_key: Option<String>,
        public_keys: Option<Vec<String>>,
    },
    Mercurial {
        r#ref: Option<String>,
        rev: Option<String>,
    },
    Indirect {
        id: String,
        rev: Option<String>,
        r#ref: Option<String>,
    },
}

enum FetchType {
    Path,
    File,
    Tarball,
    Git,
    GitHub,
    GitLab,
    SourceHut,
    Indirect,
}

impl FromStr for FetchType {
    type Err = FetchTreeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "path" => Ok(FetchType::Path),
            "file" => Ok(FetchType::File),
            "tarball" => Ok(FetchType::Tarball),
            "git" => Ok(FetchType::Git),
            "github" => Ok(FetchType::GitHub),
            "gitlab" => Ok(FetchType::GitLab),
            "sourcehut" => Ok(FetchType::SourceHut),
            "indirect" => Ok(FetchType::Indirect),
            // TODO: No Mercurial support: https://github.com/search?q=path%3A%2F%5Eflake.lock%24%2F+%28%2F%22hg%22%2F+OR+%2F%22mercurial%22%2F%29&type=code
            _ => Err(FetchTreeError::UnsupportedType(s.to_string())),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FetchTreeError {
    #[error("failed to parse URL: {0}")]
    UrlParseError(url::ParseError),
    #[error("unsupported input type: {0}")]
    UnsupportedType(String),
}

pub fn parse_url(url: &str) -> Result<FetchTreeArgs, FetchTreeError> {
    let mut url = Url::parse(url).map_err(FetchTreeError::UrlParseError)?;
    let mut newprotocol = None;

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
            s if s.starts_with("git") => FetchType::Git,
            // Check for tarball file extensions
            _ if url.path().ends_with(".zip")
                || url.path().ends_with(".tar")
                || url.path().ends_with(".tgz")
                || url.path().ends_with(".tar.gz")
                || url.path().ends_with(".tar.xz")
                || url.path().ends_with(".tar.bz2")
                || url.path().ends_with(".tar.zst") =>
            {
                FetchType::Tarball
            }
            // File/tarball schemes
            _ => FetchType::File,
        }
    };

    if let Some(protocol) = newprotocol {
        let mut url_str = url.to_string();
        url_str.replace_range(..url.scheme().len(), &protocol);
        url = Url::parse(&url_str).map_err(FetchTreeError::UrlParseError)?;
    }

    // Extract common query parameters
    let mut query_pairs = std::collections::HashMap::new();
    for (k, v) in url.query_pairs() {
        query_pairs.entry(k).or_insert(v);
    }
    let ref_param = query_pairs.get("ref").map(|s| s.to_string());
    let rev_param = query_pairs.get("rev").map(|s| s.to_string());
    let submodules_param = query_pairs
        .get("submodules")
        .map(|v| v == "1" || v.to_lowercase() == "true")
        .unwrap_or(false);

    match fetch_type {
        FetchType::File => Ok(FetchTreeArgs::File {
            url,
            nar_hash: None,
            rev: rev_param,
            rev_count: None,
            last_modified: None,
        }),
        FetchType::Tarball => Ok(FetchTreeArgs::Tarball {
            url,
            nar_hash: None,
            rev: rev_param,
            rev_count: None,
            last_modified: None,
        }),
        FetchType::Indirect => Ok(FetchTreeArgs::Indirect {
            id: url.path().to_string(),
            rev: rev_param,
            r#ref: ref_param,
        }),
        FetchType::Git => Ok(FetchTreeArgs::Git {
            url,
            r#ref: ref_param,
            rev: rev_param,
            keytype: None,
            public_key: None,
            public_keys: None,
            shallow: true,
            submodules: submodules_param,
            export_ignore: false,
            all_refs: false,
            verify_commit: false,
        }),
        FetchType::Path => Ok(FetchTreeArgs::Path {
            path: PathBuf::from(url.path()),
            rev: rev_param,
            nar_hash: None,
            rev_count: None,
            last_modified: None,
        }),
        FetchType::GitHub => Ok(FetchTreeArgs::GitHub {
            r#ref: ref_param,
            rev: rev_param,
            keytype: None,
            public_key: None,
            public_keys: None,
        }),
        FetchType::GitLab => Ok(FetchTreeArgs::GitLab {
            r#ref: ref_param,
            rev: rev_param,
            keytype: None,
            public_key: None,
            public_keys: None,
        }),
        FetchType::SourceHut => Ok(FetchTreeArgs::SourceHut {
            r#ref: ref_param,
            rev: rev_param,
            keytype: None,
            public_key: None,
            public_keys: None,
        }),
    }
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
                assert!(shallow);
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
    }
}

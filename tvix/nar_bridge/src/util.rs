use std::path::Path;

pub fn basename_if_exists(path: Option<&Path>) -> String {
    path.map_or(String::new(), |v| {
        v.file_name()
            .map_or(String::new(), |v| v.to_string_lossy().to_string())
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::util::basename_if_exists;

    #[test]
    fn test_basename_if_exists() {
        let p: Option<PathBuf> = Some(PathBuf::from("/foo/bar/baz"));
        assert_eq!(basename_if_exists(p.as_deref()), "baz");

        let p: Option<PathBuf> = Some(PathBuf::from("/"));
        assert_eq!(basename_if_exists(p.as_deref()), "");

        let p: Option<PathBuf> = None;
        assert_eq!(basename_if_exists(p.as_deref()), "");

        let p: Option<PathBuf> = Some(PathBuf::from(""));
        assert_eq!(basename_if_exists(p.as_deref()), "");
    }
}

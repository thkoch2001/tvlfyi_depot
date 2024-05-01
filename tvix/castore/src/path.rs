//! Contains data structures to deal with Paths in the tvix-castore model.

use std::str::FromStr;

use bstr::ByteSlice;

/// Represents a Path in the castore model.
/// These are always relative, and platform-independent, which distinguishes
/// them from the ones provided in the standard library.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Path<'a> {
    // As node names in the castore model cannot contain slashes,
    // we use them as component separators here.
    inner: &'a [u8],
}

#[allow(dead_code)]
impl Path<'_> {
    pub fn parent(&self) -> Option<Path<'_>> {
        let (parent, _file_name) = self.inner.rsplit_once_str(b"/")?;
        Some(Self { inner: parent })
    }

    pub fn join(&self, name: &[u8]) -> Result<PathBuf, std::io::Error> {
        if name.contains(&b'/') || name.is_empty() {
            return Err(std::io::ErrorKind::InvalidData.into());
        }

        let mut v = self.inner.to_vec();
        if !v.is_empty() {
            v.extend_from_slice(b"/");
        }
        v.extend_from_slice(name);

        Ok(PathBuf { inner: v })
    }

    /// Produces an iterator over the components of the path, which are
    /// individual byte slices.
    /// In case the path is empty, an empty iterator is returned.
    pub fn components(&self) -> impl Iterator<Item = &[u8]> {
        let mut iter = self.inner.split_str(&b"/");

        // We don't want to return an empty element, consume it if it's the only one.
        if self.inner.is_empty() {
            let _ = iter.next();
        }

        iter
    }

    /// Returns the final component of the Path, if there is one.
    pub fn file_name(&self) -> Option<&[u8]> {
        self.components().last()
    }

    pub fn as_slice(&self) -> &[u8] {
        self.inner
    }

    /// Returns an owned [PathBuf], copying the data.
    pub fn to_owned(&self) -> PathBuf {
        PathBuf {
            inner: self.inner.to_vec(),
        }
    }
}

/// Represents a owned PathBuf in the castore model.
/// These are always relative, and platform-independent, which distinguishes
/// them from the ones provided in the standard library.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct PathBuf {
    inner: Vec<u8>,
}

#[allow(dead_code)]
impl PathBuf {
    pub fn as_ref(&self) -> Path<'_> {
        Path { inner: &self.inner }
    }

    pub fn parent(&self) -> Option<Path<'_>> {
        let (parent, _file_name) = self.inner.rsplit_once_str(b"/")?;
        Some(Path { inner: parent })
    }

    pub fn join(&self, name: &[u8]) -> Result<Self, std::io::Error> {
        self.as_ref().join(name)
    }

    /// Produces an iterator over the components of the path, which are
    /// individual byte slices.
    pub fn components(&self) -> impl Iterator<Item = &[u8]> {
        // TODO(edef): get rid of the duplication
        let mut iter = self.inner.split_str(&b"/");

        // We don't want to return an empty element, consume it if it's the only one.
        if self.inner.is_empty() {
            let _ = iter.next();
        }

        iter
    }

    /// Returns the final component of the Path, if there is one.
    pub fn file_name(&self) -> Option<&[u8]> {
        self.components().last()
    }

    pub fn as_slice(&self) -> &[u8] {
        // TODO(edef): get rid of the duplication
        self.inner.as_slice()
    }
}

impl FromStr for PathBuf {
    type Err = std::io::Error;

    fn from_str(s: &str) -> Result<PathBuf, Self::Err> {
        // Ensure there's no empty components (aka, double forward slashes),
        // and all components individually validate.
        let p = Path {
            inner: s.as_bytes(),
        };

        for component in p.components() {
            if component.is_empty() {
                return Err(std::io::ErrorKind::InvalidData.into());
            }
        }

        Ok(PathBuf {
            inner: s.to_string().into(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::PathBuf;
    use bstr::ByteSlice;
    use rstest::rstest;

    // TODO: add some manual tests including invalid UTF-8 (hard to express
    // with rstest)

    #[rstest]
    #[case::empty("", 0)]
    #[case("a", 1)]
    #[case("a/b", 2)]
    #[case("a/b/c", 3)]
    // add two slightly more cursed variants.
    // Technically nothing prevents us from representing this with castore,
    // but maybe we want to disallow constructing paths like this as it's a
    // bad idea.
    #[case::cursed("C:\\a/b", 2)]
    #[case::cursed("\\tvix-store", 1)]
    pub fn from_str(#[case] s: &str, #[case] num_components: usize) {
        let p: PathBuf = s.parse().expect("must parse");

        assert_eq!(s.as_bytes(), p.as_slice(), "inner bytes mismatch");
        assert_eq!(
            num_components,
            p.components().count(),
            "number of components mismatch"
        );
    }

    #[rstest]
    #[case::absolute("/a/b")]
    #[case::two_forward_slashes_start("//a/b")]
    #[case::two_forward_slashes_middle("a/b//c/d")]
    #[case::trailing_slash("a/b/")]
    pub fn from_str_fail(#[case] s: &str) {
        s.parse::<PathBuf>().expect_err("must fail");
    }

    #[rstest]
    #[case("foo/bar", "foo")]
    #[case("foo2/bar2", "foo2")]
    #[case("foo/bar/baz", "foo/bar")]
    pub fn parent(#[case] p: PathBuf, #[case] exp_parent: PathBuf) {
        assert_eq!(Some(exp_parent.as_ref()), p.parent());

        // same for Path
        let p = p.as_ref();
        assert_eq!(Some(exp_parent.as_ref()), p.parent());
    }

    #[rstest]
    #[case::empty("")]
    #[case::single("foo")]
    pub fn no_parent(#[case] p: PathBuf) {
        assert!(p.parent().is_none());

        // same for Path
        assert!(p.as_ref().parent().is_none());
    }

    #[rstest]
    #[case("a", "b", "a/b")]
    #[case("a", "b", "a/b")]
    pub fn join(#[case] p: PathBuf, #[case] name: &str, #[case] exp_p: PathBuf) {
        assert_eq!(exp_p, p.join(name.as_bytes()).expect("join failed"));
        // same for Path
        assert_eq!(
            exp_p,
            p.as_ref().join(name.as_bytes()).expect("join failed")
        );
    }

    #[rstest]
    #[case("a", "/")]
    #[case("a", "")]
    #[case("a", "b/c")]
    #[case("", "/")]
    #[case("", "")]
    #[case("", "b/c")]
    pub fn join_fail(#[case] p: PathBuf, #[case] name: &str) {
        p.join(name.as_bytes())
            .expect_err("join succeeded unexpectedly");

        // same for Path
        p.as_ref()
            .join(name.as_bytes())
            .expect_err("join succeeded unexpectedly");
    }

    #[rstest]
    #[case::empty("", vec![])]
    #[case("a", vec!["a"])]
    #[case("a/b", vec!["a", "b"])]
    #[case("a/b/c", vec!["a","b", "c"])]
    pub fn components(#[case] p: PathBuf, #[case] exp_components: Vec<&str>) {
        assert_eq!(
            exp_components,
            p.components()
                .map(|x| x.to_str().unwrap())
                .collect::<Vec<_>>()
        );

        // same for Path
        let p = p.as_ref();
        assert_eq!(
            exp_components,
            p.components()
                .map(|x| x.to_str().unwrap())
                .collect::<Vec<_>>()
        );
    }
}

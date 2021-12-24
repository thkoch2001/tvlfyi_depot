// TODO: make this no_std if possible

use camino::Utf8PathBuf;

mod hbm;
pub use hbm::HalfBytesMask;

mod spec;
pub use spec::*;

/// limit maximal length of store basename
const BASENAME_MAXLEN: usize = 255;

/// this is a trait which implements the interface of possible inputs
/// (usually byte slices)
pub trait ScannerInput: AsRef<[u8]> + Sized {
    /// Splits the input into two at the given index.
    /// Afterwards self contains elements [at, len), and the returned input part contains elements [0, at).
    fn split_to(&mut self, at: usize) -> Self;
    fn finish(&mut self);
}

impl ScannerInput for &[u8] {
    fn split_to(&mut self, at: usize) -> Self {
        let (a, b) = self.split_at(at);
        *self = b;
        a
    }

    fn finish(&mut self) {
        *self = &[];
    }
}

impl ScannerInput for &mut [u8] {
    fn split_to(&mut self, at: usize) -> Self {
        // Lifetime dance taken from `impl Write for &mut [u8]`.
        // Taken from crate `std`.
        let (a, b) = core::mem::replace(self, &mut []).split_at_mut(at);
        *self = b;
        a
    }

    fn finish(&mut self) {
        *self = &mut [];
    }
}

/// this is the primary structure of this crate
///
/// it represents a scanner which scans binary slices for store references,
/// and implements an iterator interfaces which returns these as byte slices.
pub struct StoreRefScanner<'x, Input: 'x> {
    input: Input,
    spec: &'x StoreSpec,
}

/// Taken from crate `yz-string-utils`.
fn get_offset_of<T>(whole_buffer: &T, part: &T) -> usize
where
    T: AsRef<[u8]> + ?Sized,
{
    // NOTE: originally I wanted to use offset_from() here once it's stable,
    // but according to https://github.com/rust-lang/rust/issues/41079#issuecomment-657163887
    // this would be UB in cases where the code below isn't.
    part.as_ref().as_ptr() as usize - whole_buffer.as_ref().as_ptr() as usize
}

impl<'x, Input> StoreRefScanner<'x, Input>
where
    Input: ScannerInput + 'x,
{
    pub fn new(input: Input, spec: &'x StoreSpec) -> Self {
        for i in [&spec.valid_hashbytes, &spec.valid_restbytes] {
            for j in [b'\0', b' ', b'\t', b'\n', b'/', b'\\'] {
                assert!(!i.contains(j));
            }
        }
        Self { input, spec }
    }
}

impl<'x, Input: 'x> Iterator for StoreRefScanner<'x, Input>
where
    Input: ScannerInput + 'x,
{
    type Item = Input;

    fn next(&mut self) -> Option<Input> {
        let empty_path = camino::Utf8Path::new("");
        let hbl: usize = self.spec.hashbytes_len.into();
        while !self.input.as_ref().is_empty() {
            if self.spec.path_to_store != empty_path {
                let p2sas = self.spec.path_to_store.as_str();
                if self.input.as_ref().starts_with(p2sas.as_bytes()) {
                    self.input.split_to(p2sas.len());
                } else {
                    self.input.split_to(1);
                    continue;
                }
            }
            let hsep = matches!(self.input.as_ref().iter().next(), Some(b'/') | Some(b'\\'));
            self.input.split_to(1);
            if hsep && self.spec.check_rest(self.input.as_ref()) {
                // we have found a valid hash
                // rest contains the store basename and all following components
                // now let's search for the end
                // and then cut off possible following components after the basename
                let rlen = self
                    .input
                    .as_ref()
                    .iter()
                    .enumerate()
                    .take(BASENAME_MAXLEN)
                    .skip(hbl)
                    .find(|&(_, &i)| !self.spec.valid_restbytes.contains(i))
                    .map(|(eosp, _)| eosp)
                    .unwrap_or(core::cmp::min(BASENAME_MAXLEN, self.input.as_ref().len()));
                return Some(self.input.split_to(rlen));
            }
        }
        self.input.finish();
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_nix2() {
        let drv: &[u8] = br#"
            Derive([("out","","r:sha256","")],[("/nix/store/2ax7bvjdfkzim69q957i0jlg0nvmapg0-util-linux-2.37.2.drv",["dev"]),("/nix/store/6b55ssmh8pzqsc4q4kw1yl3kqvr4fvqj-bash-5.1-p12.drv",["out"]),("/nix/store/fp2vx24kczlzv84avds28wyzsmrn8kyv-source.drv",["out"]),("/nix/store/s6c2lm5hpsvdwnxq9y1g3ngncghjzc3k-stdenv-linux.drv",["out"]),("/nix/store/xlnzpf4mzghi8vl0krabrgcbnqk5qjf3-pkg-config-wrapper-0.29.2.drv",["out"])],["/nix/store/03sl46khd8gmjpsad7223m32ma965vy9-fix-static.patch","/nix/store/2q3z7587yhlz0i2xvfvvap42zk5carlv-bcache-udev-modern.patch","/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"],"x86_64-linux","/0g15yibzzi3rmw29gqlbms05x9dbghbvh61v1qggydvmzh3bginw/bin/bash",["-e","/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"],[("buildInputs","/0sdk1r4l43yw4g6lmqdhd92vhdfhlwz3m76jxzvzsqsv63czw2km"),("builder","/0g15yibzzi3rmw29gqlbms05x9dbghbvh61v1qggydvmzh3bginw/bin/bash"),("configureFlags",""),("depsBuildBuild",""),("depsBuildBuildPropagated",""),("depsBuildTarget",""),("depsBuildTargetPropagated",""),("depsHostHost",""),("depsHostHostPropagated",""),("depsTargetTarget",""),("depsTargetTargetPropagated",""),("doCheck",""),("doInstallCheck",""),("makeFlags","PREFIX=/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9 UDEVLIBDIR=/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9/lib/udev/"),("name","bcache-tools-1.0.7"),("nativeBuildInputs","/1kw0rwgdyq9q69wmmsa5d2kap6p52b0yldbzi4w17bhcq5g5cp2f"),("out","/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9"),("outputHashAlgo","sha256"),("outputHashMode","recursive"),("outputs","out"),("patches","/nix/store/2q3z7587yhlz0i2xvfvvap42zk5carlv-bcache-udev-modern.patch /nix/store/03sl46khd8gmjpsad7223m32ma965vy9-fix-static.patch"),("pname","bcache-tools"),("preBuild","sed -e \"s|/bin/sh|/0g15yibzzi3rmw29gqlbms05x9dbghbvh61v1qggydvmzh3bginw/bin/sh|\" -i *.rules\n"),("preInstall","mkdir -p \"$out/sbin\" \"$out/lib/udev/rules.d\" \"$out/share/man/man8\"\n"),("prePatch","sed -e \"/INSTALL.*initramfs\\/hook/d\" \\\n    -e \"/INSTALL.*initcpio\\/install/d\" \\\n    -e \"/INSTALL.*dracut\\/module-setup.sh/d\" \\\n    -e \"s/pkg-config/$PKG_CONFIG/\" \\\n    -i Makefile\n"),("propagatedBuildInputs",""),("propagatedNativeBuildInputs",""),("src","/nix/store/6izcafvfcbz19chi7hl20834g0fa043n-source"),("stdenv","/01ncyv8bxibj0imgfvmxgqy648n697bachil6aw6i46g1jk0bbds"),("strictDeps",""),("system","x86_64-linux"),("version","1.0.7")])
        "#;
        // we convert everything into strings because it is way easier to compare elements in error messages
        let refs: Vec<&str> = StoreRefScanner::new(drv, &*SPEC_DFL_NIX2)
            .map(|i| core::str::from_utf8(i).unwrap())
            .collect();
        let refs_expect: Vec<&[u8]> = vec![
            b"2ax7bvjdfkzim69q957i0jlg0nvmapg0-util-linux-2.37.2.drv",
            b"6b55ssmh8pzqsc4q4kw1yl3kqvr4fvqj-bash-5.1-p12.drv",
            b"fp2vx24kczlzv84avds28wyzsmrn8kyv-source.drv",
            b"s6c2lm5hpsvdwnxq9y1g3ngncghjzc3k-stdenv-linux.drv",
            b"xlnzpf4mzghi8vl0krabrgcbnqk5qjf3-pkg-config-wrapper-0.29.2.drv",
            b"03sl46khd8gmjpsad7223m32ma965vy9-fix-static.patch",
            b"2q3z7587yhlz0i2xvfvvap42zk5carlv-bcache-udev-modern.patch",
            b"9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh",
            b"9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh",
            b"2q3z7587yhlz0i2xvfvvap42zk5carlv-bcache-udev-modern.patch",
            b"03sl46khd8gmjpsad7223m32ma965vy9-fix-static.patch",
            b"6izcafvfcbz19chi7hl20834g0fa043n-source",
        ];
        let refs_expect: Vec<&str> = refs_expect
            .into_iter()
            .map(|i| core::str::from_utf8(i).unwrap())
            .collect();
        assert_eq!(refs, refs_expect);
    }

    #[test]
    fn simple_yzix1() {
        // I haven't yet produced any yzix derivation which included /yzixs absolute paths...
        let fake: &[u8] = br#"
            /yzixs/4Zx1PBoft1YyAuKdhjAY1seZFHloxQ+8voHQRkRMuys:         ASCII text
            /yzixs/dNE3yogD4JHKHzNa2t3jQMZddT8wjqlMDB0naDIFo0A:         ASCII text
            /yzixs/FMluSVOHLc4bxX7F4lBCXafNljBnDn+rAM5HzG7k8LI:         unified diff output, ASCII text
            /yzixs/g2G3GRL87hGEdw9cq2BZWqDQP_HeHSPRLbJ9P9KH+HI:         unified diff output, ASCII text
            /yzixs/H08Av1ZAONwFdzVLpFQm0Sc0dvyk0sbnk82waoBig7I:         ASCII text
            /yzixs/IndARQp+gaGDLS3K+PeyXdaRqAcCyS3EIbRXkkYjC94:         unified diff output, ASCII text
            /yzixs/IrLPnbkEolTAuWRxkXpuvVs6Imb1iB6wUJcI+fxWwkU:         POSIX shell script, ASCII text executable
            /yzixs/JsS_H3n3TSh2R6fiIzgOPZdjSmRkV71vGxstJJKPmr4:         unified diff output, ASCII text
            /yzixs/LZ6pQh1x8DRxZ2IYzetBRS4LuE__IXFjpOfQPxHVwpw:         unified diff output, ASCII text
            /yzixs/mEi2RPep9daRs0JUvwt1JsDfgYSph5sH_+_ihwn8IGQ:         ASCII text
            /yzixs/nd4DyljinP3auDMHL_LrpsRJkWQpSHQK2jqtyyzWcBA:         POSIX shell script, ASCII text executable
            /yzixs/nzpaknF0_ONSHtd0i_e1E3pkLF1QPeJQhAB7x9Ogo_M:         unified diff output, ASCII text
            /yzixs/UZ3uzVUUMC1gKGLw6tg_aLFwoFrJedXB3xbhEgQOaiY:         unified diff output, ASCII text
            /yzixs/VKyXxKTXsDGxYJ24YgbvCc1bZkA5twp3TC+Gbi4Kwd8:         unified diff output, ASCII text
            /yzixs/VPJMl8O1xkc1LsJznpoQrCrQO0Iy+ODCPsgoUBLiRZc:         unified diff output, ASCII text
            /yzixs/W6r1ow001ASHRj+gtRfyj9Fb_gCO_pBztX8WhYXVdIc:         unified diff output, ASCII text
            /yzixs/xvwEcXIob_rQynUEtQiQbwaDXEobTVKEGaBMir9oH9k:         unified diff output, ASCII text
            /yzixs/ZPvQbRJrtyeSITvW3FUZvw99hhNOO3CFqGgmWgScxcg:         ASCII text
        "#;
        let refs: Vec<&str> = StoreRefScanner::new(fake, &*SPEC_DFL_YZIX1)
            .map(|i| core::str::from_utf8(i).unwrap())
            .collect();
        let refs_expect: Vec<&[u8]> = vec![
            b"4Zx1PBoft1YyAuKdhjAY1seZFHloxQ+8voHQRkRMuys",
            b"dNE3yogD4JHKHzNa2t3jQMZddT8wjqlMDB0naDIFo0A",
            b"FMluSVOHLc4bxX7F4lBCXafNljBnDn+rAM5HzG7k8LI",
            b"g2G3GRL87hGEdw9cq2BZWqDQP_HeHSPRLbJ9P9KH+HI",
            b"H08Av1ZAONwFdzVLpFQm0Sc0dvyk0sbnk82waoBig7I",
            b"IndARQp+gaGDLS3K+PeyXdaRqAcCyS3EIbRXkkYjC94",
            b"IrLPnbkEolTAuWRxkXpuvVs6Imb1iB6wUJcI+fxWwkU",
            b"JsS_H3n3TSh2R6fiIzgOPZdjSmRkV71vGxstJJKPmr4",
            b"LZ6pQh1x8DRxZ2IYzetBRS4LuE__IXFjpOfQPxHVwpw",
            b"mEi2RPep9daRs0JUvwt1JsDfgYSph5sH_+_ihwn8IGQ",
            b"nd4DyljinP3auDMHL_LrpsRJkWQpSHQK2jqtyyzWcBA",
            b"nzpaknF0_ONSHtd0i_e1E3pkLF1QPeJQhAB7x9Ogo_M",
            b"UZ3uzVUUMC1gKGLw6tg_aLFwoFrJedXB3xbhEgQOaiY",
            b"VKyXxKTXsDGxYJ24YgbvCc1bZkA5twp3TC+Gbi4Kwd8",
            b"VPJMl8O1xkc1LsJznpoQrCrQO0Iy+ODCPsgoUBLiRZc",
            b"W6r1ow001ASHRj+gtRfyj9Fb_gCO_pBztX8WhYXVdIc",
            b"xvwEcXIob_rQynUEtQiQbwaDXEobTVKEGaBMir9oH9k",
            b"ZPvQbRJrtyeSITvW3FUZvw99hhNOO3CFqGgmWgScxcg",
        ];
        let refs_expect: Vec<&str> = refs_expect
            .into_iter()
            .map(|i| core::str::from_utf8(i).unwrap())
            .collect();
        assert_eq!(refs, refs_expect);
    }

    proptest::proptest! {
        #[test]
        fn nocrash_nix2(s: Vec<u8>) {
            let _ = StoreRefScanner::new(&s[..], &*SPEC_DFL_NIX2).count();
        }

        #[test]
        fn nocrash_yzix1(s: Vec<u8>) {
            let _ = StoreRefScanner::new(&s[..], &*SPEC_DFL_YZIX1).count();
        }
    }
}

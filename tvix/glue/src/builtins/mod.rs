//! Contains builtins that deal with the store or builder.

use std::rc::Rc;

use crate::tvix_store_io::TvixStoreIO;

mod derivation;
mod derivation_error;

mod import;

pub use derivation_error::Error as DerivationError;

/// Adds derivation-related builtins to the passed [tvix_eval::Evaluation].
///
/// These are `derivation` and `derivationStrict`.
///
/// As they need to interact with `known_paths`, we also need to pass in
/// `known_paths`.
pub fn add_derivation_builtins<IO>(eval: &mut tvix_eval::Evaluation<IO>, io: Rc<TvixStoreIO>) {
    eval.builtins
        .extend(derivation::derivation_builtins::builtins(io));

    // Add the actual `builtins.derivation` from compiled Nix code
    eval.src_builtins
        .push(("derivation", include_str!("derivation.nix")));
}

/// Adds import-related builtins to the passed [tvix_eval::Evaluation].
///
/// These are `filterSource` and `path`
///
/// As they need to interact with the store implementation, we pass [`TvixStoreIO`].
pub fn add_import_builtins<IO>(eval: &mut tvix_eval::Evaluation<IO>, io: Rc<TvixStoreIO>) {
    eval.builtins.extend(import::import_builtins(io));
    // FUTUREWORK: should we implement `builtins.filterSource` via `builtins.path` in pure Nix?
    // indeed: `builtins.filterSource = filter: path: builtins.path { inherit path filter; };`…
}

#[cfg(test)]
mod tests {
    use std::{rc::Rc, sync::Arc};

    use crate::tvix_store_io::TvixStoreIO;

    use super::add_derivation_builtins;
    use nix_compat::store_path::hash_placeholder;
    use test_case::test_case;
    use tvix_build::buildservice::DummyBuildService;
    use tvix_eval::{EvalIO, EvaluationResult};
    use tvix_store::utils::construct_services;

    /// evaluates a given nix expression and returns the result.
    /// Takes care of setting up the evaluator so it knows about the
    // `derivation` builtin.
    fn eval(str: &str) -> EvaluationResult {
        // We assemble a complete store in memory.
        let runtime = tokio::runtime::Runtime::new().expect("Failed to build a Tokio runtime");
        let (blob_service, directory_service, path_info_service) = runtime
            .block_on(async { construct_services("memory://", "memory://", "memory://").await })
            .expect("Failed to construct store services in memory");

        let io = Rc::new(TvixStoreIO::new(
            blob_service,
            directory_service,
            path_info_service.into(),
            Arc::<DummyBuildService>::default(),
            runtime.handle().clone(),
        ));

        let mut eval = tvix_eval::Evaluation::new(io.clone() as Rc<dyn EvalIO>, false);

        add_derivation_builtins(&mut eval, io.clone());
        add_import_builtins(&mut eval, io);

        // run the evaluation itself.
        eval.evaluate(str, None)
    }

    #[test]
    fn derivation() {
        let result = eval(
            r#"(derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux";}).outPath"#,
        );

        assert!(result.errors.is_empty(), "expect evaluation to succeed");
        let value = result.value.expect("must be some");

        match value {
            tvix_eval::Value::String(s) => {
                assert_eq!(
                    "/nix/store/xpcvxsx5sw4rbq666blz6sxqlmsqphmr-foo",
                    s.as_str()
                );
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    /// a derivation with an empty name is an error.
    #[test]
    fn derivation_empty_name_fail() {
        let result = eval(
            r#"(derivation { name = ""; builder = "/bin/sh"; system = "x86_64-linux";}).outPath"#,
        );

        assert!(!result.errors.is_empty(), "expect evaluation to fail");
    }

    /// construct some calls to builtins.derivation and compare produced output
    /// paths.
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/17wgs52s7kcamcyin4ja58njkf91ipq8-foo"; "r:sha256")]
    #[test_case(r#"(builtins.derivation { name = "foo2"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/gi0p8vd635vpk1nq029cz3aa3jkhar5k-foo2"; "r:sha256 other name")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha1"; outputHash = "sha1-VUCRC+16gU5lcrLYHlPSUyx0Y/Q="; }).outPath"#, "/nix/store/p5sammmhpa84ama7ymkbgwwzrilva24x-foo"; "r:sha1")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "md5"; outputHash = "md5-07BzhNET7exJ6qYjitX/AA=="; }).outPath"#, "/nix/store/gmmxgpy1jrzs86r5y05wy6wiy2m15xgi-foo"; "r:md5")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha512"; outputHash = "sha512-DPkYCnZKuoY6Z7bXLwkYvBMcZ3JkLLLc5aNPCnAvlHDdwr8SXBIZixmVwjPDS0r9NGxUojNMNQqUilG26LTmtg=="; }).outPath"#, "/nix/store/lfi2bfyyap88y45mfdwi4j99gkaxaj19-foo"; "r:sha512")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "4374173a8cbe88de152b609f96f46e958bcf65762017474eec5a05ec2bd61530"; }).outPath"#, "/nix/store/17wgs52s7kcamcyin4ja58njkf91ipq8-foo"; "r:sha256 base16")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "0c0msqmyq1asxi74f5r0frjwz2wmdvs9d7v05caxx25yihx1fx23"; }).outPath"#, "/nix/store/17wgs52s7kcamcyin4ja58njkf91ipq8-foo"; "r:sha256 nixbase32")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/17wgs52s7kcamcyin4ja58njkf91ipq8-foo"; "r:sha256 base64")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "sha256-fgIr3TyFGDAXP5+qoAaiMKDg/a1MlT6Fv/S/DaA24S8="; }).outPath"#, "/nix/store/xm1l9dx4zgycv9qdhcqqvji1z88z534b-foo"; "r:sha256 base64 nopad")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "flat"; outputHashAlgo = "sha256"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/q4pkwkxdib797fhk22p0k3g1q32jmxvf-foo"; "sha256")]
    #[test_case(r#"(builtins.derivation { name = "foo2"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "flat"; outputHashAlgo = "sha256"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/znw17xlmx9r6gw8izjkqxkl6s28sza4l-foo2"; "sha256 other name")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "flat"; outputHashAlgo = "sha1"; outputHash = "sha1-VUCRC+16gU5lcrLYHlPSUyx0Y/Q="; }).outPath"#, "/nix/store/zgpnjjmga53d8srp8chh3m9fn7nnbdv6-foo"; "sha1")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "flat"; outputHashAlgo = "md5"; outputHash = "md5-07BzhNET7exJ6qYjitX/AA=="; }).outPath"#, "/nix/store/jfhcwnq1852ccy9ad9nakybp2wadngnd-foo"; "md5")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "flat"; outputHashAlgo = "sha512"; outputHash = "sha512-DPkYCnZKuoY6Z7bXLwkYvBMcZ3JkLLLc5aNPCnAvlHDdwr8SXBIZixmVwjPDS0r9NGxUojNMNQqUilG26LTmtg=="; }).outPath"#, "/nix/store/as736rr116ian9qzg457f96j52ki8bm3-foo"; "sha512")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/17wgs52s7kcamcyin4ja58njkf91ipq8-foo"; "r:sha256 outputHashAlgo omitted")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#, "/nix/store/q4pkwkxdib797fhk22p0k3g1q32jmxvf-foo"; "r:sha256 outputHashAlgo and outputHashMode omitted")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; }).outPath"#, "/nix/store/xpcvxsx5sw4rbq666blz6sxqlmsqphmr-foo"; "outputHash* omitted")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; outputs = ["foo" "bar"]; system = "x86_64-linux"; }).outPath"#, "/nix/store/hkwdinvz2jpzgnjy9lv34d2zxvclj4s3-foo-foo"; "multiple outputs")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; args = ["--foo" "42" "--bar"]; system = "x86_64-linux"; }).outPath"#, "/nix/store/365gi78n2z7vwc1bvgb98k0a9cqfp6as-foo"; "args")]
    #[test_case(r#"
                   let
                     bar = builtins.derivation {
                       name = "bar";
                       builder = ":";
                       system = ":";
                       outputHash = "08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba";
                       outputHashAlgo = "sha256";
                       outputHashMode = "recursive";
                     };
                   in
                   (builtins.derivation {
                     name = "foo";
                     builder = ":";
                     system = ":";
                     inherit bar;
                   }).outPath
        "#, "/nix/store/5vyvcwah9l9kf07d52rcgdk70g2f4y13-foo"; "full")]
    #[test_case(r#"(builtins.derivation { "name" = "foo"; passAsFile = ["bar"]; bar = "baz"; system = ":"; builder = ":";}).outPath"#, "/nix/store/25gf0r1ikgmh4vchrn8qlc4fnqlsa5a1-foo"; "passAsFile")]
    // __ignoreNulls = true, but nothing set to null
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = true; }).drvPath"#, "/nix/store/xa96w6d7fxrlkk60z1fmx2ffp2wzmbqx-foo.drv"; "ignoreNulls no arg drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = true; }).outPath"#, "/nix/store/pk2agn9za8r9bxsflgh1y7fyyrmwcqkn-foo"; "ignoreNulls no arg outPath")]
    // __ignoreNulls = true, with a null arg, same paths
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = true; ignoreme = null; }).drvPath"#, "/nix/store/xa96w6d7fxrlkk60z1fmx2ffp2wzmbqx-foo.drv"; "ignoreNulls drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = true; ignoreme = null; }).outPath"#, "/nix/store/pk2agn9za8r9bxsflgh1y7fyyrmwcqkn-foo"; "ignoreNulls outPath")]
    // __ignoreNulls = false
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = false; }).drvPath"#, "/nix/store/xa96w6d7fxrlkk60z1fmx2ffp2wzmbqx-foo.drv"; "ignoreNulls false no arg drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = false; }).outPath"#, "/nix/store/pk2agn9za8r9bxsflgh1y7fyyrmwcqkn-foo"; "ignoreNulls false no arg arg outPath")]
    // __ignoreNulls = false, with a null arg
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = false; foo = null; }).drvPath"#, "/nix/store/xwkwbajfiyhdqmksrbzm0s4g4ib8d4ms-foo.drv"; "ignoreNulls false arg drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = false; foo = null; }).outPath"#, "/nix/store/2n2jqm6l7r2ahi19m58pl896ipx9cyx6-foo"; "ignoreNulls false arg arg outPath")]
    // structured attrs set to false will render an empty string inside env
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __structuredAttrs = false; foo = "bar"; }).drvPath"#, "/nix/store/qs39krwr2lsw6ac910vqx4pnk6m63333-foo.drv"; "structuredAttrs-false-drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __structuredAttrs = false; foo = "bar"; }).outPath"#, "/nix/store/9yy3764rdip3fbm8ckaw4j9y7vh4d231-foo"; "structuredAttrs-false-outPath")]
    // simple structured attrs
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __structuredAttrs = true; foo = "bar"; }).drvPath"#, "/nix/store/k6rlb4k10cb9iay283037ml1nv3xma2f-foo.drv"; "structuredAttrs-simple-drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __structuredAttrs = true; foo = "bar"; }).outPath"#, "/nix/store/6lmv3hyha1g4cb426iwjyifd7nrdv1xn-foo"; "structuredAttrs-simple-outPath")]
    // structured attrs with outputsCheck
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __structuredAttrs = true; foo = "bar"; outputChecks = {out = {maxClosureSize = 256 * 1024 * 1024; disallowedRequisites = [ "dev" ];};}; }).drvPath"#, "/nix/store/fx9qzpchh5wchchhy39bwsml978d6wp1-foo.drv"; "structuredAttrs-outputChecks-drvPath")]
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __structuredAttrs = true; foo = "bar"; outputChecks = {out = {maxClosureSize = 256 * 1024 * 1024; disallowedRequisites = [ "dev" ];};}; }).outPath"#, "/nix/store/pcywah1nwym69rzqdvpp03sphfjgyw1l-foo"; "structuredAttrs-outputChecks-outPath")]
    // structured attrs and __ignoreNulls. ignoreNulls is inactive (so foo ends up in __json, yet __ignoreNulls itself is not present.
    #[test_case(r#"(builtins.derivation { name = "foo"; system = ":"; builder = ":"; __ignoreNulls = false; foo = null; __structuredAttrs = true; }).drvPath"#, "/nix/store/rldskjdcwa3p7x5bqy3r217va1jsbjsc-foo.drv"; "structuredAttrs-and-ignore-nulls-drvPath")]

    fn test_outpath(code: &str, expected_path: &str) {
        let value = eval(code).value.expect("must succeed");

        match value {
            tvix_eval::Value::String(s) => {
                assert_eq!(expected_path, s.as_str());
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    /// construct some calls to builtins.derivation that should be rejected
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "sha256-00"; }).outPath"#; "invalid outputhash")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha1"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath"#; "sha1 and sha256")]
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; outputs = ["foo" "foo"]; system = "x86_64-linux"; }).outPath"#; "duplicate output names")]
    fn test_outpath_invalid(code: &str) {
        let resp = eval(code);
        assert!(resp.value.is_none(), "Value should be None");
        assert!(
            !resp.errors.is_empty(),
            "There should have been some errors"
        );
    }

    /// Construct two FODs with the same name, and same known output (but
    /// slightly different recipe), ensure they have the same output hash.
    #[test]
    fn test_fod_outpath() {
        let code = r#"
          (builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath ==
          (builtins.derivation { name = "foo"; builder = "/bin/aa"; system = "x86_64-linux"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath
        "#;

        let value = eval(code).value.expect("must succeed");
        match value {
            tvix_eval::Value::Bool(v) => {
                assert!(v);
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    /// Construct two FODs with the same name, and same known output (but
    /// slightly different recipe), ensure they have the same output hash.
    #[test]
    fn test_fod_outpath_different_name() {
        let code = r#"
          (builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath ==
          (builtins.derivation { name = "foo"; builder = "/bin/aa"; system = "x86_64-linux"; outputHash = "sha256-Q3QXOoy+iN4VK2CflvRulYvPZXYgF0dO7FoF7CvWFTA="; }).outPath
        "#;

        let value = eval(code).value.expect("must succeed");
        match value {
            tvix_eval::Value::Bool(v) => {
                assert!(v);
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    /// Construct two derivations with the same parameters except one of them lost a context string
    /// for a dependency, causing the loss of an element in the `inputDrvs` derivation. Therefore,
    /// making `outPath` different.
    #[test]
    fn test_unsafe_discard_string_context() {
        let code = r#"
        let
            dep = builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; };
        in
          (builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; env = "${dep}"; }).outPath !=
          (builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; env = "${builtins.unsafeDiscardStringContext dep}"; }).outPath
        "#;

        let value = eval(code).value.expect("must succeed");
        match value {
            tvix_eval::Value::Bool(v) => {
                assert!(v);
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    /// Construct an attribute set that coerces to a derivation and verify that the return type is
    /// a string.
    #[test]
    fn test_unsafe_discard_string_context_of_coercible() {
        let code = r#"
        let
            dep = builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; };
            attr = { __toString = _: dep; };
        in
            builtins.typeOf (builtins.unsafeDiscardStringContext attr) == "string"
        "#;

        let value = eval(code).value.expect("must succeed");
        match value {
            tvix_eval::Value::Bool(v) => {
                assert!(v);
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    #[test_case(r#"
                   let
                     bar = builtins.derivation {
                       name = "bar";
                       builder = ":";
                       system = ":";
                       outputHash = "08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba";
                       outputHashAlgo = "sha256";
                       outputHashMode = "recursive";
                     };
                   in
                   (builtins.derivation {
                     name = "foo";
                     builder = ":";
                     args = [ "${bar}" ];
                     system = ":";
                   }).drvPath
        "#, "/nix/store/50yl2gmmljyl0lzyrp1mcyhn53vhjhkd-foo.drv"; "input in `args`")]
    fn test_inputs_derivation_from_context(code: &str, expected_drvpath: &str) {
        let eval_result = eval(code);

        let value = eval_result.value.expect("must succeed");

        match value {
            tvix_eval::Value::String(s) => {
                assert_eq!(expected_drvpath, s.as_str());
            }

            _ => panic!("unexpected value type: {:?}", value),
        };
    }

    #[test]
    fn builtins_placeholder_hashes() {
        assert_eq!(
            hash_placeholder("out").as_str(),
            "/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9"
        );

        assert_eq!(
            hash_placeholder("").as_str(),
            "/171rf4jhx57xqz3p7swniwkig249cif71pa08p80mgaf0mqz5bmr"
        );
    }

    /// constructs calls to builtins.derivation that should succeed, but produce warnings
    #[test_case(r#"(builtins.derivation { name = "foo"; builder = "/bin/sh"; system = "x86_64-linux"; outputHashMode = "recursive"; outputHashAlgo = "sha256"; outputHash = "sha256-fgIr3TyFGDAXP5+qoAaiMKDg/a1MlT6Fv/S/DaA24S8===="; }).outPath"#, "/nix/store/xm1l9dx4zgycv9qdhcqqvji1z88z534b-foo"; "r:sha256 wrong padding")]
    fn builtins_derivation_hash_wrong_padding_warn(code: &str, expected_path: &str) {
        let eval_result = eval(code);

        let value = eval_result.value.expect("must succeed");

        match value {
            tvix_eval::Value::String(s) => {
                assert_eq!(expected_path, s.as_str());
            }
            _ => panic!("unexpected value type: {:?}", value),
        }

        assert!(
            !eval_result.warnings.is_empty(),
            "warnings should not be empty"
        );
    }
}

#![allow(non_snake_case)]
/// This struct contains the data which result from a successful
/// call to `builtins.derivationStrict`; these are exactly the data
/// needed to produce a single Nix `.drv` file.  Note that this
/// struct is not a variant of the value::Value enum.

pub struct Derivation {
    pub __impure: bool,
    pub __contentAddressed: bool,
    pub __ignoreNulls: bool,
    pub __structuredAttrs: bool,

    pub name: String,
    pub system: String,

    pub inputSrcs: Vec<String>,
    pub inputDrvs: Vec<String>,

    pub builder: String,
    pub args: Vec<String>,

    pub outputs: Vec<String>,

    pub outputHashSpec: Option<OutputHashSpec>,
}

pub struct OutputHashSpec {
    pub outputHash: String,
    pub outputHashAlgo: String,
    pub outputHashMode: String,
}

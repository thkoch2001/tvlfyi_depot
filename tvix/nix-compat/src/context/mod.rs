use serde::Serialize;

#[derive(Clone, Debug, Serialize, Hash, PartialEq, Eq)]
pub enum NixContextElement {
    /// A plain store path (e.g. source files copied to the store)
    Plain(String),

    /// Single output of a derivation, represented by its name and its derivation path.
    Single { name: String, derivation: String },

    /// A reference to a complete derivation
    /// including its source and its binary closure.
    /// It is used for the `drvPath` attribute context.
    /// The referred string is the store path to
    /// the derivation path.
    Derivation(String),
}

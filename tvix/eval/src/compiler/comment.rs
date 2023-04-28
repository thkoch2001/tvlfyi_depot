use rnix::ast::{self, AstToken};

/// Implements functions to distinguish
/// a comment from doc-comment and retrieve the content as raw string
pub trait DocComment {
    fn is_doc(&self) -> bool;
    fn doc_text(&self) -> &str;
}

impl DocComment for ast::Comment {
    /// Check whether this is a doc-comment
    /// Format: /** */
    fn is_doc(&self) -> bool {
        let text = self.syntax().text();
        // Check if the comment starts with
        // /** but not with /**/ which is a regular comment
        text.starts_with(r#"/**"#) && self.text().starts_with("*")
    }
    /// .is_doc() should be called before to check
    /// whether this is a doc-comment; otherwise this just yields the raw-text
    fn doc_text(&self) -> &str {
        let text = self.syntax().text();
        text.strip_prefix(r#"/**"#)
            .unwrap_or(text)
            .strip_suffix(r#"*/"#)
            .unwrap_or(text)
    }
}

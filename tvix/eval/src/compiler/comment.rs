use rnix::ast::{self, AstToken};

/// Implements functions for doc-comments
pub trait DocComment {
    fn doc_text(&self) -> Option<&str>;
}

impl DocComment for ast::Comment {
    /// This returns the contents of the doc-comment, if the [ast::Comment] is a
    /// doc-comment, or None otherwise.
    ///
    /// /**{content}*/
    /// -> {content}
    ///
    /// It is named `doc_text` to complement [ast::Comment::text].
    fn doc_text(&self) -> Option<&str> {
        let text = self.syntax().text();
        // Check whether this is a doc-comment
        if text.starts_with(r#"/**"#) && self.text().starts_with("*") {
            match text.strip_prefix(r#"/**"#) {
                Some(t) => t.strip_suffix(r#"*/"#),
                _ => None,
            }
        } else {
            None
        }
    }
}

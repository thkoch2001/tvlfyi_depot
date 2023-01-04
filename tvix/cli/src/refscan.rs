use aho_corasick::{AhoCorasick, FindIter, StateID};

pub struct ReferenceScanner {
    searcher: AhoCorasick,
}

impl ReferenceScanner {
    pub fn new(candidates: &[&str]) -> Self {
        let searcher = AhoCorasick::new_auto_configured(candidates);

        ReferenceScanner { searcher }
    }

    pub fn scan_str<'s, 'h>(&'s self, haystack: &'h str) -> FindIter<'s, 'h, usize> {
        self.searcher.find_iter(haystack)
    }
}

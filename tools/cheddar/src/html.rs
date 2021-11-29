// SPDX-License-Identifier: Apache-2.0
//
// This file is taken from syntect at:
// https://github.com/trishume/syntect/blob/f444f604f6b96f5f6c17cb63a84f891f69516586/src/html.rs

//! Rendering highlighted code as HTML+CSS
use std::fmt;
use std::fmt::Write;
use syntect::easy::{HighlightFile, HighlightLines};
use syntect::highlighting::{Color, FontStyle, Style, Theme};
use syntect::parsing::{
    BasicScopeStackOp, ParseState, Scope, ScopeStack, ScopeStackOp, SyntaxReference, SyntaxSet,
    SCOPE_REPO,
};
use syntect::util::LinesWithEndings;

use std::io::{self, BufRead};
use std::path::Path;

// This struct is taken from syntect, in escape.rs (same commit as
// mentioned above). Syntect contains this in a crate that is, for
// whatever reason, made private - so we carry it here instead.
//
// Copyright for this type: 2013 The Rust Project Developers.
/// Wrapper struct which will emit the HTML-escaped version of the contained
/// string when passed to a format string.
pub struct Escape<'a>(pub &'a str);

impl<'a> fmt::Display for Escape<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Because the internet is always right, turns out there's not that many
        // characters to escape: http://stackoverflow.com/questions/7381974
        let Escape(s) = *self;
        let pile_o_bits = s;
        let mut last = 0;
        for (i, ch) in s.bytes().enumerate() {
            match ch as char {
                '<' | '>' | '&' | '\'' | '"' => {
                    fmt.write_str(&pile_o_bits[last..i])?;
                    let s = match ch as char {
                        '>' => "&gt;",
                        '<' => "&lt;",
                        '&' => "&amp;",
                        '\'' => "&#39;",
                        '"' => "&quot;",
                        _ => unreachable!(),
                    };
                    fmt.write_str(s)?;
                    last = i + 1;
                }
                _ => {}
            }
        }

        if last < s.len() {
            fmt.write_str(&pile_o_bits[last..])?;
        }
        Ok(())
    }
}

/// Output HTML for a line of code with `<span>` elements using class names
/// As this has to keep track of open and closed `<span>` tags, it is a `struct`
/// with additional state.
///
/// There is a `finalize()` function that has to be called in the end in order
/// to close all open `<span>` tags.
///
/// The lines returned don't include a newline at the end.
/// # Example
///
/// ```
/// use syntect::html::{ClassedHTMLGenerator, ClassStyle};
/// use syntect::parsing::SyntaxSet;
///
/// let current_code = r#"
/// x <- 5
/// y <- 6
/// x + y
/// "#.to_string();
///
/// let syntax_set = SyntaxSet::load_defaults_newlines();
/// let syntax = syntax_set.find_syntax_by_name("R").unwrap();
/// let mut html_generator = ClassedHTMLGenerator::new_with_class_style(&syntax, &syntax_set, ClassStyle::Spaced);
/// for line in current_code.lines() {
///     html_generator.parse_html_for_line(&line);
/// }
/// let output_html = html_generator.finalize();
/// ```
pub struct ClassedHTMLGenerator<'a> {
    syntax_set: &'a SyntaxSet,
    open_spans: isize,
    parse_state: ParseState,
    html: String,
    style: ClassStyle,
}

impl<'a> ClassedHTMLGenerator<'a> {
    #[deprecated(since = "4.2.0", note = "Please use `new_with_class_style` instead")]
    pub fn new(
        syntax_reference: &'a SyntaxReference,
        syntax_set: &'a SyntaxSet,
    ) -> ClassedHTMLGenerator<'a> {
        Self::new_with_class_style(syntax_reference, syntax_set, ClassStyle::Spaced)
    }

    pub fn new_with_class_style(
        syntax_reference: &'a SyntaxReference,
        syntax_set: &'a SyntaxSet,
        style: ClassStyle,
    ) -> ClassedHTMLGenerator<'a> {
        let parse_state = ParseState::new(syntax_reference);
        let open_spans = 0;
        let html = String::new();
        ClassedHTMLGenerator {
            syntax_set,
            open_spans,
            parse_state,
            html,
            style,
        }
    }

    /// Parse the line of code and update the internal HTML buffer with tagged HTML
    pub fn parse_html_for_line(&mut self, line: &str) {
        let parsed_line = self.parse_state.parse_line(line, &self.syntax_set);
        let (formatted_line, delta) =
            tokens_to_classed_spans(line, parsed_line.as_slice(), self.style);
        self.open_spans += delta;
        self.html.push_str(formatted_line.as_str());
        // retain newline
        self.html.push_str("\n");
    }

    /// Close all open `<span>` tags and return the finished HTML string
    pub fn finalize(mut self) -> String {
        for _ in 0..self.open_spans {
            self.html.push_str("</span>");
        }
        self.html
    }
}

#[deprecated(
    since = "4.2.0",
    note = "Please use `css_for_theme_with_class_style` instead."
)]
pub fn css_for_theme(theme: &Theme) -> String {
    css_for_theme_with_class_style(theme, ClassStyle::Spaced)
}

/// Create a complete CSS for a given theme. Can be used inline, or written to
/// a CSS file.
pub fn css_for_theme_with_class_style(theme: &Theme, style: ClassStyle) -> String {
    let mut css = String::new();

    css.push_str("/*\n");
    let name = theme.name.clone().unwrap_or("unknown theme".to_string());
    css.push_str(&format!(" * theme \"{}\" generated by syntect\n", name));
    css.push_str(" */\n\n");

    match style {
        ClassStyle::Spaced => {
            css.push_str(".code {\n");
        }
        ClassStyle::SpacedPrefixed { prefix } => {
            css.push_str(&format!(".{}code {{\n", prefix));
        }
    };
    if let Some(fgc) = theme.settings.foreground {
        css.push_str(&format!(
            " color: #{:02x}{:02x}{:02x};\n",
            fgc.r, fgc.g, fgc.b
        ));
    }
    if let Some(bgc) = theme.settings.background {
        css.push_str(&format!(
            " background-color: #{:02x}{:02x}{:02x};\n",
            bgc.r, bgc.g, bgc.b
        ));
    }
    css.push_str("}\n\n");

    for i in &theme.scopes {
        for scope_selector in &i.scope.selectors {
            let scopes = scope_selector.extract_scopes();
            for k in &scopes {
                scope_to_selector(&mut css, *k, style);
                css.push_str(" {\n");

                if let Some(fg) = i.style.foreground {
                    css.push_str(&format!(" color: #{:02x}{:02x}{:02x};\n", fg.r, fg.g, fg.b));
                }

                if let Some(bg) = i.style.background {
                    css.push_str(&format!(
                        " background-color: #{:02x}{:02x}{:02x};\n",
                        bg.r, bg.g, bg.b
                    ));
                }

                if let Some(fs) = i.style.font_style {
                    if fs.contains(FontStyle::UNDERLINE) {
                        css.push_str(&format!("font-style: underline;\n"));
                    }
                    if fs.contains(FontStyle::BOLD) {
                        css.push_str(&format!("font-weight: bold;\n"));
                    }
                    if fs.contains(FontStyle::ITALIC) {
                        css.push_str(&format!("font-style: italic;\n"));
                    }
                }
                css.push_str("}\n");
            }
        }
    }

    css
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub enum ClassStyle {
    /// The classes are the atoms of the scope separated by spaces
    /// (e.g `source.php` becomes `source php`).
    /// This isn't that fast since it has to use the scope repository
    /// to look up scope names.
    Spaced,
    /// Like `Spaced`, but the given prefix will be prepended to all
    /// classes. This is useful to prevent class name collisions, and
    /// can ensure that the theme's CSS applies precisely to syntect's
    /// output.
    ///
    /// The prefix must be a valid CSS class name. To help ennforce
    /// this invariant and prevent accidental foot-shooting, it must
    /// be statically known. (If this requirement is onerous, please
    /// file an issue; the HTML generator can also be forked
    /// separately from the rest of syntect, as it only uses the
    /// public API.)
    SpacedPrefixed { prefix: &'static str },
}

fn scope_to_classes(s: &mut String, scope: Scope, style: ClassStyle) {
    let repo = SCOPE_REPO.lock().unwrap();
    for i in 0..(scope.len()) {
        let atom = scope.atom_at(i as usize);
        let atom_s = repo.atom_str(atom);
        if i != 0 {
            s.push_str(" ")
        }
        match style {
            ClassStyle::Spaced => {}
            ClassStyle::SpacedPrefixed { prefix } => {
                s.push_str(&prefix);
            }
        }
        s.push_str(atom_s);
    }
}

fn scope_to_selector(s: &mut String, scope: Scope, style: ClassStyle) {
    let repo = SCOPE_REPO.lock().unwrap();
    for i in 0..(scope.len()) {
        let atom = scope.atom_at(i as usize);
        let atom_s = repo.atom_str(atom);
        s.push_str(".");
        match style {
            ClassStyle::Spaced => {}
            ClassStyle::SpacedPrefixed { prefix } => {
                s.push_str(&prefix);
            }
        }
        s.push_str(atom_s);
    }
}

/// Convenience method that combines `start_highlighted_html_snippet`, `styled_line_to_highlighted_html`
/// and `HighlightLines` from `syntect::easy` to create a full highlighted HTML snippet for
/// a string (which can contain many lines).
///
/// Note that the `syntax` passed in must be from a `SyntaxSet` compiled for newline characters.
/// This is easy to get with `SyntaxSet::load_defaults_newlines()`. (Note: this was different before v3.0)
pub fn highlighted_html_for_string(
    s: &str,
    ss: &SyntaxSet,
    syntax: &SyntaxReference,
    theme: &Theme,
) -> String {
    let mut highlighter = HighlightLines::new(syntax, theme);
    let (mut output, bg) = start_highlighted_html_snippet(theme);

    for line in LinesWithEndings::from(s) {
        let regions = highlighter.highlight(line, ss);
        append_highlighted_html_for_styled_line(
            &regions[..],
            IncludeBackground::IfDifferent(bg),
            &mut output,
        );
    }
    output.push_str("</pre>\n");
    output
}

/// Convenience method that combines `start_highlighted_html_snippet`, `styled_line_to_highlighted_html`
/// and `HighlightFile` from `syntect::easy` to create a full highlighted HTML snippet for
/// a file.
///
/// Note that the `syntax` passed in must be from a `SyntaxSet` compiled for newline characters.
/// This is easy to get with `SyntaxSet::load_defaults_newlines()`. (Note: this was different before v3.0)
pub fn highlighted_html_for_file<P: AsRef<Path>>(
    path: P,
    ss: &SyntaxSet,
    theme: &Theme,
) -> io::Result<String> {
    let mut highlighter = HighlightFile::new(path, ss, theme)?;
    let (mut output, bg) = start_highlighted_html_snippet(theme);

    let mut line = String::new();
    while highlighter.reader.read_line(&mut line)? > 0 {
        {
            let regions = highlighter.highlight_lines.highlight(&line, ss);
            append_highlighted_html_for_styled_line(
                &regions[..],
                IncludeBackground::IfDifferent(bg),
                &mut output,
            );
        }
        line.clear();
    }
    output.push_str("</pre>\n");
    Ok(output)
}

/// Output HTML for a line of code with `<span>` elements
/// specifying classes for each token. The span elements are nested
/// like the scope stack and the scopes are mapped to classes based
/// on the `ClassStyle` (see it's docs).
///
/// See `ClassedHTMLGenerator` for a more convenient wrapper, this is the advanced
/// version of the function that gives more control over the parsing flow.
///
/// For this to work correctly you must concatenate all the lines in a `<pre>`
/// tag since some span tags opened on a line may not be closed on that line
/// and later lines may close tags from previous lines.
///
/// Returns the HTML string and the number of `<span>` tags opened
/// (negative for closed). So that you can emit the correct number of closing
/// tags at the end.
pub fn tokens_to_classed_spans(
    line: &str,
    ops: &[(usize, ScopeStackOp)],
    style: ClassStyle,
) -> (String, isize) {
    let mut s = String::with_capacity(line.len() + ops.len() * 8); // a guess
    let mut cur_index = 0;
    let mut stack = ScopeStack::new();
    let mut span_delta = 0;

    // check and skip emty inner <span> tags
    let mut span_empty = false;
    let mut span_start = 0;

    for &(i, ref op) in ops {
        if i > cur_index {
            span_empty = false;
            write!(s, "{}", Escape(&line[cur_index..i])).unwrap();
            cur_index = i
        }
        stack.apply_with_hook(op, |basic_op, _| match basic_op {
            BasicScopeStackOp::Push(scope) => {
                span_start = s.len();
                span_empty = true;
                s.push_str("<span class=\"");
                scope_to_classes(&mut s, scope, style);
                s.push_str("\">");
                span_delta += 1;
            }
            BasicScopeStackOp::Pop => {
                if span_empty == false {
                    s.push_str("</span>");
                } else {
                    s.truncate(span_start);
                }
                span_delta -= 1;
                span_empty = false;
            }
        });
    }
    write!(s, "{}", Escape(&line[cur_index..line.len()])).unwrap();
    (s, span_delta)
}

#[deprecated(since = "3.1.0", note = "please use `tokens_to_classed_spans` instead")]
pub fn tokens_to_classed_html(
    line: &str,
    ops: &[(usize, ScopeStackOp)],
    style: ClassStyle,
) -> String {
    tokens_to_classed_spans(line, ops, style).0
}

/// Determines how background color attributes are generated
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IncludeBackground {
    /// Don't include `background-color`, for performance or so that you can use your own background.
    No,
    /// Set background color attributes on every node
    Yes,
    /// Only set the `background-color` if it is different than the default (presumably set on a parent element)
    IfDifferent(Color),
}

fn write_css_color(s: &mut String, c: Color) {
    if c.a != 0xFF {
        write!(s, "#{:02x}{:02x}{:02x}{:02x}", c.r, c.g, c.b, c.a).unwrap();
    } else {
        write!(s, "#{:02x}{:02x}{:02x}", c.r, c.g, c.b).unwrap();
    }
}

/// Output HTML for a line of code with `<span>` elements using inline
/// `style` attributes to set the correct font attributes.
/// The `bg` attribute determines if the spans will have the `background-color`
/// attribute set. See the `IncludeBackground` enum's docs.
///
/// The lines returned don't include a newline at the end.
/// # Examples
///
/// ```
/// use syntect::easy::HighlightLines;
/// use syntect::parsing::SyntaxSet;
/// use syntect::highlighting::{ThemeSet, Style};
/// use syntect::html::{styled_line_to_highlighted_html, IncludeBackground};
///
/// // Load these once at the start of your program
/// let ps = SyntaxSet::load_defaults_newlines();
/// let ts = ThemeSet::load_defaults();
///
/// let syntax = ps.find_syntax_by_name("Ruby").unwrap();
/// let mut h = HighlightLines::new(syntax, &ts.themes["base16-ocean.dark"]);
/// let regions = h.highlight("5", &ps);
/// let html = styled_line_to_highlighted_html(&regions[..], IncludeBackground::No);
/// assert_eq!(html, "<span style=\"color:#d08770;\">5</span>");
/// ```
pub fn styled_line_to_highlighted_html(v: &[(Style, &str)], bg: IncludeBackground) -> String {
    let mut s: String = String::new();
    append_highlighted_html_for_styled_line(v, bg, &mut s);
    s
}

/// Like `styled_line_to_highlighted_html` but appends to a `String` for increased efficiency.
/// In fact `styled_line_to_highlighted_html` is just a wrapper around this function.
pub fn append_highlighted_html_for_styled_line(
    v: &[(Style, &str)],
    bg: IncludeBackground,
    mut s: &mut String,
) {
    let mut prev_style: Option<&Style> = None;
    for &(ref style, text) in v.iter() {
        let unify_style = if let Some(ps) = prev_style {
            style == ps || (style.background == ps.background && text.trim().is_empty())
        } else {
            false
        };
        if unify_style {
            write!(s, "{}", Escape(text)).unwrap();
        } else {
            if prev_style.is_some() {
                write!(s, "</span>").unwrap();
            }
            prev_style = Some(style);
            write!(s, "<span style=\"").unwrap();
            let include_bg = match bg {
                IncludeBackground::Yes => true,
                IncludeBackground::No => false,
                IncludeBackground::IfDifferent(c) => (style.background != c),
            };
            if include_bg {
                write!(s, "background-color:").unwrap();
                write_css_color(&mut s, style.background);
                write!(s, ";").unwrap();
            }
            if style.font_style.contains(FontStyle::UNDERLINE) {
                write!(s, "text-decoration:underline;").unwrap();
            }
            if style.font_style.contains(FontStyle::BOLD) {
                write!(s, "font-weight:bold;").unwrap();
            }
            if style.font_style.contains(FontStyle::ITALIC) {
                write!(s, "font-style:italic;").unwrap();
            }
            write!(s, "color:").unwrap();
            write_css_color(&mut s, style.foreground);
            write!(s, ";\">{}", Escape(text)).unwrap();
        }
    }
    if prev_style.is_some() {
        write!(s, "</span>").unwrap();
    }
}

/// Returns a `<pre style="...">\n` tag with the correct background color for the given theme.
/// This is for if you want to roll your own HTML output, you probably just want to use
/// `highlighted_html_for_string`.
///
/// If you don't care about the background color you can just prefix the lines from
/// `styled_line_to_highlighted_html` with a `<pre>`. This is meant to be used with `IncludeBackground::IfDifferent`.
/// As of `v3.0` this method also returns the background color to be passed to `IfDifferent`.
///
/// You're responsible for creating the string `</pre>` to close this, I'm not gonna provide a
/// helper for that :-)
pub fn start_highlighted_html_snippet(t: &Theme) -> (String, Color) {
    let c = t.settings.background.unwrap_or(Color::WHITE);
    (
        format!(
            "<pre style=\"background-color:#{:02x}{:02x}{:02x};\">\n",
            c.r, c.g, c.b
        ),
        c,
    )
}

use clap::{App, Arg};
use comrak::arena_tree::Node;
use comrak::nodes::{Ast, AstNode, NodeCodeBlock, NodeHtmlBlock, NodeValue};
use comrak::{format_html, parse_document, Arena, ComrakOptions};
use lazy_static::lazy_static;
use rouille::Response;
use rouille::{router, try_or_400};
use serde::Deserialize;
use serde_json::json;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::ffi::OsStr;
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use syntect::dumps::from_binary;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet};
use syntect::util::LinesWithEndings;

use syntect::html::{
    append_highlighted_html_for_styled_line, start_highlighted_html_snippet, IncludeBackground,
};

#[cfg(test)]
mod tests;

lazy_static! {
    // Load syntaxes & themes lazily. Initialisation might not be
    // required in the case of Markdown rendering (if there's no code
    // blocks within the document).
    static ref SYNTAXES: SyntaxSet = from_binary(include_bytes!(env!("BAT_SYNTAXES")));
    static ref THEMES: ThemeSet = ThemeSet::load_defaults();

    // Configure Comrak's Markdown rendering with all the bells &
    // whistles!
    static ref MD_OPTS: ComrakOptions = {
        let mut options = ComrakOptions::default();

        // Enable non-standard Markdown features:
        options.extension.strikethrough = true;
        options.extension.tagfilter = true;
        options.extension.table = true;
        options.extension.autolink = true;
        options.extension.tasklist = true;
        options.extension.header_ids = Some(String::new()); // yyeeesss!
        options.extension.footnotes = true;
        options.extension.description_lists = true;

        // Required for tagfilter
        options.render.unsafe_ = true;

        options
    };

    // Configures a map of specific filenames to languages, for cases
    // where the detection by extension or other heuristics fails.
    static ref FILENAME_OVERRIDES: HashMap<&'static str, &'static str> = {
        let mut map = HashMap::new();
        // rules.pl is the canonical name of the submit rule file in
        // Gerrit, which is written in Prolog.
        map.insert("rules.pl", "Prolog");
        map
    };
}

// HTML fragment used when rendering inline blocks in Markdown documents.
// Emulates the GitHub style (subtle background hue and padding).
const BLOCK_PRE: &str = "<pre style=\"background-color:#f6f8fa;padding:16px;\">\n";

fn should_continue(res: &io::Result<usize>) -> bool {
    match *res {
        Ok(n) => n > 0,
        Err(_) => false,
    }
}

// This function is taken from the Comrak documentation.
fn iter_nodes<'a, F>(node: &'a AstNode<'a>, f: &F)
where
    F: Fn(&'a AstNode<'a>),
{
    f(node);
    for c in node.children() {
        iter_nodes(c, f);
    }
}

// Many of the syntaxes in the syntax list have random capitalisations, which
// means that name matching for the block info of a code block in HTML fails.
//
// Instead, try finding a syntax match by comparing case insensitively (for
// ASCII characters, anyways).
fn find_syntax_case_insensitive(info: &str) -> Option<&'static SyntaxReference> {
    // TODO(tazjin): memoize this lookup
    SYNTAXES
        .syntaxes()
        .iter()
        .rev()
        .find(|&s| info.eq_ignore_ascii_case(&s.name))
}

// Replaces code-block inside of a Markdown AST with HTML blocks rendered by
// syntect. This enables static (i.e. no JavaScript) syntax highlighting, even
// of complex languages.
fn highlight_code_block(code_block: &NodeCodeBlock) -> NodeValue {
    let theme = &THEMES.themes["InspiredGitHub"];
    let info = String::from_utf8_lossy(&code_block.info);

    let syntax = find_syntax_case_insensitive(&info)
        .or_else(|| SYNTAXES.find_syntax_by_extension(&info))
        .unwrap_or_else(|| SYNTAXES.find_syntax_plain_text());

    let code = String::from_utf8_lossy(&code_block.literal);

    let rendered = {
        // Write the block preamble manually to get exactly the
        // desired layout:
        let mut hl = HighlightLines::new(syntax, theme);
        let mut buf = BLOCK_PRE.to_string();

        for line in LinesWithEndings::from(&code) {
            let regions = hl.highlight(line, &SYNTAXES);
            append_highlighted_html_for_styled_line(&regions[..], IncludeBackground::No, &mut buf);
        }

        buf.push_str("</pre>");
        buf
    };

    let mut block = NodeHtmlBlock::default();
    block.literal = rendered.into_bytes();

    NodeValue::HtmlBlock(block)
}

// Supported callout elements (which each have their own distinct rendering):
enum Callout {
    Todo,
    Warning,
    Question,
    Tip,
}

// Determine whether the first child of the supplied node contains a text that
// should cause a callout section to be rendered.
fn has_callout<'a>(node: &Node<'a, RefCell<Ast>>) -> Option<Callout> {
    match node.first_child().map(|c| c.data.borrow()) {
        Some(child) => match &child.value {
            NodeValue::Text(text) => {
                if text.starts_with(b"TODO") {
                    return Some(Callout::Todo);
                } else if text.starts_with(b"WARNING") {
                    return Some(Callout::Warning);
                } else if text.starts_with(b"QUESTION") {
                    return Some(Callout::Question);
                } else if text.starts_with(b"TIP") {
                    return Some(Callout::Tip);
                }

                None
            }
            _ => None,
        },
        _ => None,
    }
}

enum LinkType {
    BUG,
    CHANGELIST,
}

enum FragmentType {
    Text(String),
    Link {
        type_: LinkType,
        target: String,
        text: String,
    },
}

fn split_shortlinks(input: String) -> Vec<FragmentType> {
    unimplemented!()
}

fn format_callout_paragraph(callout: Callout) -> NodeValue {
    let class = match callout {
        Callout::Todo => "cheddar-todo",
        Callout::Warning => "cheddar-warning",
        Callout::Question => "cheddar-question",
        Callout::Tip => "cheddar-tip",
    };

    let mut block = NodeHtmlBlock::default();
    block.literal = format!("<p class=\"cheddar-callout {}\">", class).into_bytes();
    NodeValue::HtmlBlock(block)
}

// Detect TVL short links and make them clickable.
fn linkify(text: &[u8]) -> Option<NodeValue> {
    let mut out = String::new();

    unimplemented!()
}

fn format_markdown<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let document = {
        let mut buffer = String::new();
        reader
            .read_to_string(&mut buffer)
            .expect("reading should work");
        buffer
    };

    let arena = Arena::new();
    let root = parse_document(&arena, &document, &MD_OPTS);

    println!("before:\n{:?}", root);

    // This node must exist with a lifetime greater than that of the parsed AST
    // in case that callouts are encountered (otherwise insertion into the tree
    // is not possible).
    let mut p_close_value = NodeHtmlBlock::default();
    p_close_value.literal = b"</p>".to_vec();

    let p_close_node = Ast::new(NodeValue::HtmlBlock(p_close_value));
    let p_close = Node::new(RefCell::new(p_close_node));

    // Special features of Cheddar are implemented by traversing the
    // arena and reacting on nodes that we might want to modify.
    iter_nodes(root, &|node| {
        let mut ast = node.data.borrow_mut();
        match &ast.value {
            // Syntax highlighting is implemented by replacing the
            // code block node with literal HTML.
            NodeValue::CodeBlock(code) => {
                ast.value = highlight_code_block(code);
            }

            NodeValue::Paragraph => {
                if let Some(callout) = has_callout(node) {
                    node.insert_after(&p_close);
                    ast.value = format_callout_paragraph(callout)
                }
            }

            NodeValue::Text(text) => {
                // Do nothing if the parent is already a link.
                // if node.parent()

                if let Some(nodes) = linkify(text) {
                    unimplemented!()
                }
            }
            // NodeValue::Text(text) => Some(linkify(text)),
            _ => {}
        };
    });

    println!("after:\n{:?}", root);

    format_html(root, &MD_OPTS, writer).expect("Markdown rendering failed");
}

fn find_syntax_for_file(filename: &str) -> &'static SyntaxReference {
    (*FILENAME_OVERRIDES)
        .get(filename)
        .and_then(|name| SYNTAXES.find_syntax_by_name(name))
        .or_else(|| {
            Path::new(filename)
                .extension()
                .and_then(OsStr::to_str)
                .and_then(|s| SYNTAXES.find_syntax_by_extension(s))
        })
        .unwrap_or_else(|| SYNTAXES.find_syntax_plain_text())
}

fn format_code<R: BufRead, W: Write>(
    theme: &Theme,
    reader: &mut R,
    writer: &mut W,
    filename: &str,
) {
    let mut linebuf = String::new();

    // Get the first line, we might need it for syntax identification.
    let mut read_result = reader.read_line(&mut linebuf);
    let syntax = find_syntax_for_file(filename);

    let mut hl = HighlightLines::new(syntax, theme);
    let (mut outbuf, bg) = start_highlighted_html_snippet(theme);

    // Rather than using the `lines` iterator, read each line manually
    // and maintain buffer state.
    //
    // This is done because the syntax highlighter requires trailing
    // newlines to be efficient, and those are stripped in the lines
    // iterator.
    while should_continue(&read_result) {
        let regions = hl.highlight(&linebuf, &SYNTAXES);

        append_highlighted_html_for_styled_line(
            &regions[..],
            IncludeBackground::IfDifferent(bg),
            &mut outbuf,
        );

        // immediately output the current state to avoid keeping
        // things in memory
        write!(writer, "{}", outbuf).expect("write should not fail");

        // merry go round again
        linebuf.clear();
        outbuf.clear();
        read_result = reader.read_line(&mut linebuf);
    }

    writeln!(writer, "</pre>").expect("write should not fail");
}

// Server endpoint for rendering the syntax of source code. This
// replaces the 'syntect_server' component of Sourcegraph.
fn code_endpoint(request: &rouille::Request) -> rouille::Response {
    #[derive(Deserialize)]
    struct SourcegraphQuery {
        filepath: String,
        theme: String,
        code: String,
    }

    let query: SourcegraphQuery = try_or_400!(rouille::input::json_input(request));
    let mut buf: Vec<u8> = Vec::new();

    // We don't use syntect with the sourcegraph themes bundled
    // currently, so let's fall back to something that is kind of
    // similar (tm).
    let theme = &THEMES.themes[match query.theme.as_str() {
        "Sourcegraph (light)" => "Solarized (light)",
        _ => "Solarized (dark)",
    }];

    format_code(theme, &mut query.code.as_bytes(), &mut buf, &query.filepath);

    Response::json(&json!({
        "is_plaintext": false,
        "data": String::from_utf8_lossy(&buf)
    }))
}

// Server endpoint for rendering a Markdown file.
fn markdown_endpoint(request: &rouille::Request) -> rouille::Response {
    let mut texts: HashMap<String, String> = try_or_400!(rouille::input::json_input(request));

    for text in texts.values_mut() {
        let mut buf: Vec<u8> = Vec::new();
        format_markdown(&mut text.as_bytes(), &mut buf);
        *text = String::from_utf8_lossy(&buf).to_string();
    }

    Response::json(&texts)
}

fn highlighting_server(listen: &str) {
    println!("Starting syntax highlighting server on '{}'", listen);

    rouille::start_server(listen, move |request| {
        router!(request,
                // Markdown rendering route
                (POST) (/markdown) => {
                    markdown_endpoint(request)
                },

                // Code rendering route
                (POST) (/) => {
                    code_endpoint(request)
                },

                _ => {
                    rouille::Response::empty_404()
                },
        )
    });
}

fn main() {
    // Parse the command-line flags passed to cheddar to determine
    // whether it is running in about-filter mode (`--about-filter`)
    // and what file extension has been supplied.
    let matches = App::new("cheddar")
        .about("TVL's syntax highlighter")
        .arg(
            Arg::with_name("about-filter")
                .help("Run as a cgit about-filter (renders Markdown)")
                .long("about-filter")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("sourcegraph-server")
                .help("Run as a Sourcegraph compatible web-server")
                .long("sourcegraph-server")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("listen")
                .help("Address to listen on")
                .long("listen")
                .takes_value(true),
        )
        .arg(Arg::with_name("filename").help("File to render").index(1))
        .get_matches();

    if matches.is_present("sourcegraph-server") {
        highlighting_server(
            matches
                .value_of("listen")
                .expect("Listening address is required for server mode"),
        );
        return;
    }

    let filename = matches.value_of("filename").expect("filename is required");

    let stdin = io::stdin();
    let mut in_handle = stdin.lock();

    let stdout = io::stdout();
    let mut out_handle = stdout.lock();

    if matches.is_present("about-filter") && filename.ends_with(".md") {
        format_markdown(&mut in_handle, &mut out_handle);
    } else {
        format_code(
            &THEMES.themes["InspiredGitHub"],
            &mut in_handle,
            &mut out_handle,
            filename,
        );
    }
}

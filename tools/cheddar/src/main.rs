use clap::{Arg, App};
use comrak::arena_tree::Node;
use comrak::nodes::{Ast, AstNode, NodeValue, NodeCodeBlock, NodeHtmlBlock};
use comrak::{Arena, parse_document, format_html, ComrakOptions};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::ffi::OsStr;
use std::io::BufRead;
use std::io::Write;
use std::io;
use std::path::Path;
use syntect::dumps::from_binary;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::{SyntaxSet, SyntaxReference};
use syntect::util::LinesWithEndings;

use syntect::html::{
    IncludeBackground,
    append_highlighted_html_for_styled_line,
    start_highlighted_html_snippet,
};

lazy_static! {
    // Load syntaxes & themes lazily. Initialisation might not be
    // required in the case of Markdown rendering (if there's no code
    // blocks within the document).
    static ref SYNTAXES: SyntaxSet = from_binary(include_bytes!(env!("BAT_SYNTAXES")));
    static ref THEMES: ThemeSet = ThemeSet::load_defaults();

    // Configure Comrak's Markdown rendering with all the bells &
    // whistles!
    static ref MD_OPTS: ComrakOptions = ComrakOptions{
        ext_strikethrough: true,
        ext_tagfilter: true,
        ext_table: true,
        ext_autolink: true,
        ext_tasklist: true,
        ext_header_ids: Some(String::new()), // yyeeesss!
        ext_footnotes: true,
        ext_description_lists: true,
        unsafe_: true, // required for tagfilter
        ..ComrakOptions::default()
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

#[derive(Debug, Default)]
struct Args {
    /// Should Cheddar run as an about filter? (i.e. give special
    /// rendering treatment to Markdown documents)
    about_filter: bool,

    /// What file extension has been supplied (if any)?
    extension: Option<String>,

    /// Which language to override the detection to (if any)?
    lang_override: Option<&'static str>,
}

fn should_continue(res: &io::Result<usize>) -> bool {
    match *res {
        Ok(n) => n > 0,
        Err(_) => false,
    }
}

// This function is taken from the Comrak documentation.
fn iter_nodes<'a, F>(node: &'a AstNode<'a>, f: &F) where F : Fn(&'a AstNode<'a>) {
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
    SYNTAXES.syntaxes().iter().rev().find(|&s| info.eq_ignore_ascii_case(&s.name))
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
            append_highlighted_html_for_styled_line(
                &regions[..], IncludeBackground::No, &mut buf,
            );
        }

        buf.push_str("</pre>");
        buf
    };

    let block = NodeHtmlBlock {
        block_type: 1, // It's unclear what behaviour is toggled by this
        literal: rendered.into_bytes(),
    };

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
                if text.starts_with("TODO".as_bytes()) {
                    return Some(Callout::Todo)
                } else if text.starts_with("WARNING".as_bytes()) {
                    return Some(Callout::Warning)
                } else if text.starts_with("QUESTION".as_bytes()) {
                    return Some(Callout::Question)
                } else if text.starts_with("TIP".as_bytes()) {
                    return Some(Callout::Tip)
                }

                return None
            },
            _ => return None,
        },
        _ => return None,
    }
}

fn format_callout_paragraph(callout: Callout) -> NodeValue {
    let class = match callout {
        Callout::Todo => "cheddar-todo",
        Callout::Warning => "cheddar-warning",
        Callout::Question => "cheddar-question",
        Callout::Tip => "cheddar-tip",
    };

    NodeValue::HtmlBlock(NodeHtmlBlock {
        block_type: 1,
        literal: format!("<p class=\"cheddar-callout {}\">", class).into_bytes(),
    })
}

fn format_markdown<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let document = {
        let mut buffer = String::new();
        reader.read_to_string(&mut buffer).expect("reading should work");
        drop(reader);
        buffer
    };

    let arena = Arena::new();
    let root = parse_document(&arena, &document, &MD_OPTS);

    // This node must exist with a lifetime greater than that of the parsed AST
    // in case that callouts are encountered (otherwise insertion into the tree
    // is not possible).
    let p_close = Node::new(RefCell::new(Ast {
        start_line: 0, // TODO(tazjin): hrmm
        content: vec![],
        open: false,
        last_line_blank: false,
        value: NodeValue::HtmlBlock(NodeHtmlBlock {
            block_type: 1,
            literal: "</p>".as_bytes().to_vec(),
        }),
    }));

    // Syntax highlighting is implemented by traversing the arena and
    // replacing all code blocks with HTML blocks rendered by syntect.
    iter_nodes(root, &|node| {
        let mut ast = node.data.borrow_mut();
        let new = match &ast.value {
            NodeValue::CodeBlock(code) => Some(highlight_code_block(code)),
            NodeValue::Paragraph => if let Some(callout) = has_callout(node) {
                node.insert_after(&p_close);
                Some(format_callout_paragraph(callout))
            } else {
                None
            },
            _ => None,
        };

        if let Some(new_value) = new {
            ast.value = new_value
        }
    });

    format_html(root, &MD_OPTS, writer)
        .expect("Markdown rendering failed");
}

fn format_code<R: BufRead, W: Write>(reader: &mut R, writer: &mut W, args: &Args) {
    let mut linebuf = String::new();

    // Get the first line, we might need it for syntax identification.
    let mut read_result = reader.read_line(&mut linebuf);

    // Set up the highlighter
    let theme = &THEMES.themes["InspiredGitHub"];

    let syntax = args.lang_override
        .and_then(|l| SYNTAXES.find_syntax_by_name(l))
        .or_else(|| match args.extension {
            Some(ref ext) => SYNTAXES.find_syntax_by_extension(ext),
            None => None,
        })
        .or_else(|| SYNTAXES.find_syntax_by_first_line(&linebuf))
        .unwrap_or_else(|| SYNTAXES.find_syntax_plain_text());

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

fn main() {
    // Parse the command-line flags passed to cheddar to determine
    // whether it is running in about-filter mode (`--about-filter`)
    // and what file extension has been supplied.
    let matches = App::new("cheddar")
        .about("TVL's syntax highlighter")
        .arg(Arg::with_name("about-filter")
             .help("Run as a cgit about-filter (renders Markdown)")
             .long("about-filter")
             .takes_value(false))
        .arg(Arg::with_name("filename")
             .help("File to render")
             .index(1))
        .get_matches();

    let mut args = Args::default();
    args.about_filter = matches.is_present("about-filter");

    let filename = matches.value_of("filename").expect("filename is required");
    if let Some(lang) = (*FILENAME_OVERRIDES).get(filename) {
        args.lang_override = Some(lang);
    }

    args.extension = Path::new(&filename)
        .extension()
        .and_then(OsStr::to_str)
        .map(|s| s.to_string());

    let stdin = io::stdin();
    let mut in_handle = stdin.lock();

    let stdout = io::stdout();
    let mut out_handle = stdout.lock();

    match args.extension.as_ref().map(String::as_str) {
        Some("md") if args.about_filter => format_markdown(&mut in_handle, &mut out_handle),
        _  => format_code(&mut in_handle, &mut out_handle, &args),
    }
}

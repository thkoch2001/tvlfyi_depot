use super::*;
use std::io::BufReader;

// Markdown rendering expectation, ignoring leading and trailing
// whitespace in the input and output.
fn expect_markdown(input: &str, expected: &str) {
    let mut input_buf = BufReader::new(input.trim().as_bytes());
    let mut out_buf: Vec<u8> = vec![];
    format_markdown(&mut input_buf, &mut out_buf);

    let out_string = String::from_utf8(out_buf).expect("output should be UTF8");
    assert_eq!(out_string.trim(), expected.trim());
}

#[test]
fn renders_simple_markdown() {
    expect_markdown("hello", "<p>hello</p>\n");
}

#[test]
fn renders_callouts() {
    expect_markdown(
        "TODO some task.",
        r#"<p class="cheddar-callout cheddar-todo">
TODO some task.
</p>
"#,
    );

    expect_markdown(
        "WARNING: be careful",
        r#"<p class="cheddar-callout cheddar-warning">
WARNING: be careful
</p>
"#,
    );

    expect_markdown(
        "TIP: note the thing",
        r#"<p class="cheddar-callout cheddar-tip">
TIP: note the thing
</p>
"#,
    );
}

#[test]
fn renders_code_snippets() {
    expect_markdown(
        r#"
Code:
```nix
toString 42
```
"#,
        r#"
<p>Code:</p>
<pre style="background-color:#f6f8fa;padding:16px;">
<span style="color:#62a35c;">toString </span><span style="color:#0086b3;">42
</span></pre>
"#,
    );
}

#[test]
fn linkifies_bugs() {
    // expect_markdown("b/1234", "<p><a>b/1234</a>");
    expect_markdown("before [foo](bar) after", "");
}

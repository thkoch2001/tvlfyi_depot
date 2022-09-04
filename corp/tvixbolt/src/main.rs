use std::fmt::Write;

use std::rc::Rc;
use tvix_eval::observer::DisassemblingObserver;
use web_sys::HtmlTextAreaElement;
use yew::prelude::*;
use yew::TargetCast;

enum Msg {
    CodeChange(String),
}

struct Model {
    code: String,
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            code: String::new(),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::CodeChange(new_code) => {
                self.code = new_code;
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        // This gives us a component's "`Scope`" which allows us to send messages, etc to the component.
        let link = ctx.link();
        html! {
            <div class="container">
                <h1>{"tvixbolt"}</h1>
                <form>
                  <fieldset>
                    <legend>{"Input"}</legend>

                    <div class="form-group">
                        <label for="code">{"Nix code:"}</label>
                        <textarea
                         oninput={link.callback(|e: InputEvent| {
                             let ta = e.target_unchecked_into::<HtmlTextAreaElement>().value();
                             Msg::CodeChange(ta)

                         })}
                         id="code" cols="30" rows="10">
                        </textarea>
                    </div>

                <div class="form-group">
                <label for="disable-bytecode">{"Disassemble:"}</label>
                <input for="disable-bytecode" type="checkbox" checked=true disabled=true />
                </div>
                  </fieldset>
                </form>
                <hr />
                <h2>{"Result:"}</h2>
                {eval(&self.code).display()}
            </div>
        }
    }
}

#[derive(Default)]
struct Output {
    parse_errors: String,
    warnings: String,
    compiler_errors: String,
    runtime_errors: String,
    output: String,
    bytecode: Vec<u8>,
}

fn maybe_show(title: &str, s: &str) -> Html {
    if s.is_empty() {
        html! {}
    } else {
        html! {
            <>
              <h3>{title}</h3>
              <pre>{s}</pre>
            </>
        }
    }
}

impl Output {
    fn display(self) -> Html {
        html! {
            <>
            {maybe_show("Parse errors:", &self.parse_errors)}
            {maybe_show("Warnings:", &self.warnings)}
            {maybe_show("Compiler errors:", &self.compiler_errors)}
            {maybe_show("Bytecode:", &String::from_utf8_lossy(&self.bytecode))}
            {maybe_show("Runtime errors:", &self.runtime_errors)}
            {maybe_show("Output:", &self.output)}
            </>
        }
    }
}

fn eval(code: &str) -> Output {
    let mut out = Output::default();

    if code.is_empty() {
        return out;
    }

    let mut codemap = codemap::CodeMap::new();
    let file = codemap.add_file("nixbolt".to_string(), code.into());

    let parsed = rnix::ast::Root::parse(code);
    let errors = parsed.errors();

    if !errors.is_empty() {
        for err in errors {
            writeln!(&mut out.parse_errors, "parse error: {}", err).unwrap();
        }

        return out;
    }

    // If we've reached this point, there are no errors.
    let root_expr = parsed
        .tree()
        .expr()
        .expect("expression should exist if no errors occured");

    let codemap = Rc::new(codemap);
    let mut compilation_observer = DisassemblingObserver::new(codemap, &mut out.bytecode);

    let result = tvix_eval::compile(
        root_expr,
        Some("/nixbolt".into()),
        &file,
        tvix_eval::global_builtins(),
        &mut compilation_observer,
    )
    .unwrap();

    for warning in result.warnings {
        writeln!(
            &mut out.warnings,
            "warning: {:?} at `{}` [line {}]",
            warning.kind,
            file.source_slice(warning.span),
            file.find_line(warning.span.low()) + 1
        )
        .unwrap();
    }

    if !result.errors.is_empty() {
        for error in &result.errors {
            writeln!(
                &mut out.compiler_errors,
                "error: {:?} at `{}` [line {}]",
                error.kind,
                file.source_slice(error.span),
                file.find_line(error.span.low()) + 1
            )
            .unwrap();
        }

        return out;
    }

    let result = tvix_eval::run_lambda(result.lambda);

    match result {
        Ok(value) => writeln!(&mut out.output, "{}", value).unwrap(),
        Err(err) => writeln!(&mut out.runtime_errors, "runtime error: {:?}", err).unwrap(),
    };

    out
}

fn main() {
    yew::start_app::<Model>();
}

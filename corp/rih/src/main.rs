use std::collections::BTreeSet;
use web_sys::{HtmlInputElement, KeyboardEvent};
use yew::html::Scope;
use yew::prelude::*;
use yew_router::prelude::*;

const VISTA_URL: &'static str = "https://vista-immigration.ru/";

/// Represents a single record as filled in by a user. This is the
/// primary data structure we want to populate and persist somewhere.
#[derive(Default, Debug)]
struct Record {
    // Personal information
    name: String,
    email: String,
    citizenship: String, // TODO
    personal_details: String,

    // Job information
    position: String,
    technologies: BTreeSet<String>,
    job_details: String,
}

fn tech_list() -> Vec<&'static str> {
    vec!["C++", "Rust", "C#", "Swift", "Java"]
}

#[derive(Default)]
struct App {
    // The record being populated.
    record: Record,
}

#[derive(Clone)]
enum Msg {
    NoOp,
    AddTechnology(String),
    RemoveTechnology(String),
}

/// Callback handler for adding a technology.
fn add_tech(e: KeyboardEvent) -> Msg {
    if e.key_code() != 13 {
        return Msg::NoOp;
    }

    let input = e.target_unchecked_into::<HtmlInputElement>();
    let tech = input.value();
    input.set_value("");
    Msg::AddTechnology(tech)
}

/// Creates a list of technologies which can be deleted again by clicking them.
fn render_technologies(link: &Scope<App>, technologies: &BTreeSet<String>) -> Html {
    if technologies.is_empty() {
        return html! {};
    }

    let items = technologies.iter().map(|tech| {
        let msg: Msg = Msg::RemoveTechnology(tech.to_string());
        html! {
            <>
              <span class="btn btn-secondary btn-sm"
                onclick={link.callback(move |_| msg.clone())}>
                {tech}
                <span class="mx-auto text-center text-black">{" x"}</span>
              </span>{" "}
            </>
        }
    });
    html! {
        <div class="p-1">
            { items.collect::<Html>() }
        </div>
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        let mut new = Self::default();
        new.record.technologies.insert("Java".into());
        new.record.technologies.insert("Rust".into());
        new
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::NoOp => false,

            Msg::AddTechnology(tech) => {
                self.record.technologies.insert(tech);
                true
            }

            Msg::RemoveTechnology(tech) => {
                self.record.technologies.remove(&tech);
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        include!("home.html")
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

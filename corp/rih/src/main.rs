use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use gloo::console;
use gloo::storage::{LocalStorage, Storage};
use rand::seq::IteratorRandom;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use static_markdown::markdown;
use std::collections::BTreeSet;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::{HtmlInputElement, HtmlTextAreaElement, KeyboardEvent};
use yew::html::Scope;
use yew::prelude::*;

/// This code ends up being compiled for the native and for the
/// webassembly architectures during the build & test process.
/// However, the `rust_iso3166` crate exposes a different API (!)
/// based on the platform.
///
/// This trait acts as a platform-independent wrapper for the crate.
///
/// Upstream issue: https://github.com/rust-iso/rust_iso3166/issues/7
trait CountryCodeName {
    fn country_name(&self) -> String;
}

#[cfg(target_arch = "wasm32")]
impl CountryCodeName for rust_iso3166::CountryCode {
    fn country_name(&self) -> String {
        self.name()
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl CountryCodeName for rust_iso3166::CountryCode {
    fn country_name(&self) -> String {
        self.name.to_string()
    }
}

const VISTA_URL: &'static str = "https://vista-immigration.ru/";

/// Represents a single record as filled in by a user. This is the
/// primary data structure we want to populate and persist somewhere.
#[derive(Default, Debug, Deserialize, Serialize)]
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
    work_background: String,
}

#[derive(Default)]
struct App {
    // The record being populated.
    record: Record,

    // Is the citizenship input focused?
    citizenship_focus: bool,

    // Current query in the citizenship field.
    citizenship_query: String,
}

#[derive(Clone, Debug)]
enum Msg {
    NoOp,
    AddTechnology(String),
    RemoveTechnology(String),

    FocusCitizenship,
    BlurCitizenship,
    QueryCitizenship(String),
    SetCitizenship(String),

    SetName(String),
    SetEmail(String),
    SetPersonalDetails(String),
    SetPosition(String),
    SetJobDetails(String),
    SetWorkBackground(String),
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

fn select_country_enter(event: KeyboardEvent) -> Msg {
    if event.key_code() != 13 {
        return Msg::NoOp;
    }

    let input = event.target_unchecked_into::<HtmlInputElement>();
    if let Some(country) = fuzzy_country_matches(&input.value()).next() {
        input.set_value(&country.country_name());
        return Msg::SetCitizenship(country.country_name());
    }

    Msg::NoOp
}

fn fuzzy_country_matches(query: &str) -> Box<dyn Iterator<Item = rust_iso3166::CountryCode> + '_> {
    if query.is_empty() {
        let rng = &mut thread_rng();
        return Box::new(
            rust_iso3166::ALL
                .iter()
                .choose_multiple(rng, 5)
                .into_iter()
                .map(Clone::clone),
        );
    }

    let matcher = SkimMatcherV2::default();
    let query = query.to_lowercase();
    let query_len = query.len();

    let mut results: Vec<_> = rust_iso3166::ALL
        .iter()
        .filter_map(|code| {
            let mut score = None;
            // Prioritize exact matches for country codes if query length <= 3
            if query_len <= 3 {
                if code.alpha2().eq_ignore_ascii_case(&query)
                    || code.alpha3().eq_ignore_ascii_case(&query)
                {
                    score = Some(100);
                }
            }
            // If no exact match, do a fuzzy match
            if score.is_none() {
                score = matcher.fuzzy_match(&code.country_name().to_lowercase(), &query);
            }

            score.map(|score| (score, code))
        })
        .collect();

    // Sort by score in descending order
    results.sort_by(|a, b| b.0.cmp(&a.0));

    // Get iterator over the best matches
    Box::new(results.into_iter().map(|(_score, code)| *code))
}

// Callback for an input element's value being mapped straight into a
// message.
fn input_message(e: InputEvent, msg: fn(String) -> Msg) -> Msg {
    let input = e.target_unchecked_into::<HtmlInputElement>();
    msg(input.value())
}

// Callback for a text area's value being mapped straight into a
// message.
fn textarea_message(e: InputEvent, msg: fn(String) -> Msg) -> Msg {
    let textarea = e.target_unchecked_into::<HtmlTextAreaElement>();
    msg(textarea.value())
}

fn schedule_blur(event: FocusEvent, link: Scope<App>) -> Msg {
    let input = event.target_unchecked_into::<HtmlInputElement>();
    let closure = Closure::once_into_js(Box::new(move || {
        if let Some(app) = link.get_component() {
            input.set_value(&app.record.citizenship);
        }

        link.send_message(Msg::BlurCitizenship);
    }) as Box<dyn FnOnce()>);

    let window = web_sys::window().expect("no global `window` exists");
    let _ =
        window.set_timeout_with_callback_and_timeout_and_arguments_0(closure.unchecked_ref(), 100);

    Msg::NoOp
}

/// Creates an input field for citizenship selection with suggestions.
fn citizenship_input(app: &App, link: &Scope<App>) -> Html {
    let dropdown_classes = if app.citizenship_focus {
        "dropdown-menu show"
    } else {
        "dropdown-menu"
    };

    let choices = fuzzy_country_matches(&app.citizenship_query).map(|country| {
        let msg = Msg::SetCitizenship(country.country_name());
        html! {
            <li><a class="dropdown-item" onclick={link.callback(move |_| msg.clone())}>{country.country_name()}</a></li>
        }
    });

    let blur_link = link.clone();
    html! {
      <div class="dropdown">
        <input type="text" class="form-control" id="citizenship" aria-describedby="citizenshipHelp"
            autocomplete="off"
            oninput={link.callback(|event| input_message(event, Msg::QueryCitizenship))}
            onkeypress={link.callback(select_country_enter)}
            onfocus={link.callback(|_| Msg::FocusCitizenship)}
            onblur={link.callback(move |event| schedule_blur(event, blur_link.clone()))} />
            <ul class={dropdown_classes} style="position: absolute; inset: 0px auto auto 0px; margin: 0px; transform: translate(0px, 40px);" data-popper-placement="bottom-start">
          { choices.collect::<Html>() }
        </ul>
      </div>
    }
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

        if let Ok(record) = LocalStorage::get("record") {
            new.record = record;
        }

        new
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        console::log!("handling ", format!("{:?}", msg));
        let (state_change, view_change) = match msg {
            Msg::NoOp => (false, false),

            Msg::AddTechnology(tech) => {
                console::log!("adding technology", &tech);
                self.record.technologies.insert(tech);
                (true, true)
            }

            Msg::RemoveTechnology(tech) => {
                console::log!("removing technology ", &tech);
                self.record.technologies.remove(&tech);
                (true, true)
            }

            Msg::QueryCitizenship(query) => {
                self.citizenship_query = query;
                (false, true)
            }

            Msg::FocusCitizenship => {
                self.citizenship_focus = true;
                (false, true)
            }

            Msg::BlurCitizenship => {
                self.citizenship_focus = false;
                (false, true)
            }

            Msg::SetCitizenship(country) => {
                self.record.citizenship = country;
                (true, false)
            }

            Msg::SetName(name) => {
                self.record.name = name;
                (true, false)
            }

            Msg::SetEmail(email) => {
                self.record.email = email;
                (true, false)
            }

            Msg::SetPersonalDetails(details) => {
                self.record.personal_details = details;
                (true, false)
            }

            Msg::SetPosition(position) => {
                self.record.position = position;
                (true, false)
            }

            Msg::SetJobDetails(details) => {
                self.record.job_details = details;
                (true, false)
            }

            Msg::SetWorkBackground(background) => {
                self.record.work_background = background;
                (true, false)
            }
        };

        if state_change {
            if let Err(err) = LocalStorage::set("record", &self.record) {
                console::warn!(
                    "failed to persist record in local storage: ",
                    err.to_string()
                );
            }
        }

        view_change
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        include!("home.html")
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

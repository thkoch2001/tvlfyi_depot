use std::collections::hash_set::HashSet;
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
    technologies: HashSet<String>,
    job_details: String,
}

#[derive(Default)]
struct State {
    record: Record,
}

#[derive(Clone, Routable, PartialEq)]
enum Route {
    #[at("/")]
    Home,

    #[at("/faq")]
    FAQ,

    #[at("/form")]
    Form,

    #[not_found]
    #[at("/404")]
    NotFound,
}

fn tech_list() -> Vec<&'static str> {
    vec!["C++", "Rust", "C#", "Swift", "Java"]
}

fn switch(route: Route) -> Html {
    let actual_state = State::default();
    let state = &actual_state;

    match route {
        Route::Home => include!("home.html"),

        _ => html! {
            "TODO"
        },
    }
}

#[function_component(App)]
fn app() -> Html {
    html! {
        <BrowserRouter>
            <Switch<Route> render={switch} />
        </BrowserRouter>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

use yew::prelude::*;
use yew_router::prelude::*;

const ARE_YOU_1: &'static str = "Are you an IT-specialist on the hunt for a job? Well, ";
// "times are tough" link
const ARE_YOU_2: &'static str = " in Western countries at the moment. Meanwhile tech is booming in Russia, and national support programs make life as an IT-specialist very comfortable. Why not look East?";

const WE_CAN: &'static str = "We can help you find an employer in Russia, sort out the formalities and get you started. Sign up and tell us a bit about your profile, or read on below about the benefits of life in Russia.";

pub fn render_home() -> Html {
    include!("home.html")
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
    match route {
        Route::Home => render_home(),

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

    // let tech_options = tech_list()
    //     .into_iter()
    //     .map(|tech| html! { <option value={tech}>{tech}</option> })
    //     .collect::<Html>();

    // html! {
    //     <>
    //         <h1>{"Russia is Hiring"}</h1>
    //         <p>{ "Discover exciting career opportunities in Russia! With our government's support programs for IT specialists and companies, there has never been a better time to explore your options. Join our growing network and let us connect you with leading IT firms looking for international talent." }</p>
    //         <p>{ "We want to make it easy to help you find a job that matches your interests in Russia by pairing you with companies that are interested in hiring experts from abroad. Let's start by having you tell us a little bit about yourself:" }</p>

    //         <form>
    //         <fieldset>
    //         <legend>{"Specialisation"}</legend>

    //         <label for="specialisation">{"Choose your specialisation:"}</label>
    //         <select id="specialisation" name="specialisation" required=true>
    //         <option value="developer">{"Developer"}</option>
    //         <option value="administrator">{"Administrator"}</option>
    //         <option value="qa">{"QA / Testing"}</option>
    //         <option value="other">{"Other, non-IT"}</option>
    //         </select>

    //         <label for="contact-info">{"Contact Information"}</label>
    //         <input id="contact-info" type="text" required=true/>
    //         </fieldset>

    //         <fieldset>
    //         <legend>{"Relocation Information"}</legend>

    //         <label for="current-location">{"Current Location:"}</label>
    //         <input id="current-location" type="text" required=true/>

    //         <label for="desired-location">{"Desired Location in Russia:"}</label>
    //         <input id="desired-location" type="text" required=true/>
    //         </fieldset>

    //         <fieldset>
    //         <legend>{"Relevant Technologies"}</legend>

    //         <label for="tech">{"Select relevant technologies (hold Ctrl or Cmd to select multiple):"}</label>
    //         <select id="tech" name="tech" multiple=true>
    //     { tech_options }
    //     </select>
    //         </fieldset>

    //         <fieldset>
    //         <legend>{"Other Information"}</legend>
    //         <textarea id="other-information" rows="4" cols="50"></textarea>
    //         </fieldset>

    //         <button type="submit">{"Submit"}</button>
    //         </form>
    //         </>
    // }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

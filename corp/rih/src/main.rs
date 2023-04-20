use yew::prelude::*;

fn tech_list() -> Vec<&'static str> {
    vec![
        "C++",
        "Rust",
        "C#",
        "Swift",
        "Java",
    ]
}

#[function_component(App)]
fn app() -> Html {
    let tech_options = tech_list()
        .into_iter()
        .map(|tech| html! { <option value={tech}>{tech}</option> })
        .collect::<Html>();

    html! {
        <>
            <h1>{"Russia is Hiring"}</h1>
            <p>{ "Discover exciting career opportunities in Russia! With our government's support programs for IT specialists and companies, there has never been a better time to explore your options. Join our growing network and let us connect you with leading IT firms looking for international talent." }</p>
            <p>{ "We want to make it easy to help you find a job that matches your interests in Russia by pairing you with companies that are interested in hiring experts from abroad. Let's start by having you tell us a little bit about yourself:" }</p>

            <form>
            <fieldset>
            <legend>{"Specialisation"}</legend>

            <label for="specialisation">{"Choose your specialisation:"}</label>
            <select id="specialisation" name="specialisation" required=true>
            <option value="developer">{"Developer"}</option>
            <option value="administrator">{"Administrator"}</option>
            <option value="qa">{"QA / Testing"}</option>
            <option value="other">{"Other, non-IT"}</option>
            </select>

            <label for="contact-info">{"Contact Information"}</label>
            <input id="contact-info" type="text" required=true/>
            </fieldset>

            <fieldset>
            <legend>{"Relocation Information"}</legend>

            <label for="current-location">{"Current Location:"}</label>
            <input id="current-location" type="text" required=true/>

            <label for="desired-location">{"Desired Location in Russia:"}</label>
            <input id="desired-location" type="text" required=true/>
            </fieldset>

            <fieldset>
            <legend>{"Relevant Technologies"}</legend>

            <label for="tech">{"Select relevant technologies (hold Ctrl or Cmd to select multiple):"}</label>
            <select id="tech" name="tech" multiple=true>
        { tech_options }
        </select>
            </fieldset>

            <fieldset>
            <legend>{"Other Information"}</legend>
            <textarea id="other-information" rows="4" cols="50"></textarea>
            </fieldset>

            <button type="submit">{"Submit"}</button>
            </form>
            </>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

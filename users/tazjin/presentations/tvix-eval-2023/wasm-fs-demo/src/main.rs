use std::time::{SystemTime, UNIX_EPOCH};
use yew::prelude::*;

fn time_example() -> Html {
    let epoch = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => duration.as_secs(),
        Err(err) => {
            return html! {
                format!("failed to calculate duration: {}", err)
            }
        }
    };

    html! {
        <p>
          {"Seconds since epoch: "}{epoch}
        </p>
    }
}

struct App;
impl Component for App {
    type Message = ();
    type Properties = ();

    fn create(_: &Context<Self>) -> Self {
        Self
    }

    fn update(&mut self, _: &Context<Self>, _: Self::Message) -> bool {
        false
    }

    fn view(&self, _: &Context<Self>) -> Html {
        time_example()
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

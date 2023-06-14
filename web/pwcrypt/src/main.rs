use argon2::password_hash::{PasswordHasher, SaltString};
use argon2::Argon2;
use gloo::console::log;
use rand_core::OsRng;
use web_sys::HtmlInputElement;
use yew::prelude::*;

fn hash_password(pw: &str) -> String {
    let salt = SaltString::generate(&mut OsRng);
    let argon2 = Argon2::default();
    argon2
        .hash_password(pw.as_bytes(), &salt)
        .expect("failed to hash pw")
        .to_string()
}

enum Msg {
    NoOp,
    SetEmail(String),
    SetPassword(String),
    SetUsername(String),
    UpdateCredentials,
}

#[derive(Default)]
struct App {
    email: Option<String>,
    password: Option<String>,
    username: Option<String>,
    hashed: Option<String>,
}

impl App {
    fn whats_missing(&self) -> Option<String> {
        let mut missing = vec![];

        if self.username.is_none() {
            missing.push("username");
        }

        if self.email.is_none() {
            missing.push("email");
        }

        if self.password.is_none() {
            missing.push("password");
        }

        match missing.len() {
            0 => None,
            1 => Some(missing[0].to_string()),
            2 => Some(format!("{} and {}", missing[0], missing[1])),
            3 => Some(format!("{}, {} and {}", missing[0], missing[1], missing[2])),
            _ => unreachable!(),
        }
    }

    fn update_credentials(&mut self) {
        if self.password.is_none() {
            log!("error: password unset, but credentials requested");
            return;
        }

        let pw = self.password.as_ref().unwrap();
        let hashed = hash_password(pw);
        log!("hashed password to", &hashed);
        self.hashed = Some(hashed);
    }

    fn display_credentials(&self) -> Html {
        if let (Some(username), Some(email), Some(hash)) =
            (&self.username, &self.email, &self.hashed)
        {
            html! {
              <>
                <hr />
                <p>{"Your credentials are as follows: "}</p>
                <pre>
                  {"  {\n"}
                  {"    username = \""}{username}{"\";\n"}
                  {"    email = \""}{email}{"\";\n"}
                  {"    password = \"{ARGON2}"}{hash}{"\";\n"}
                  {"  }"}
                </pre>
                <p>
                  {"Please propose a CL to "}
                  <a href="https://at.tvl.fyi/?q=//ops/users/default.nix">
                    <code>{"//ops/users/default.nix"}</code>
                  </a>
                  {", or submit your patch via email to "}
                  <a href="mailto:depot@tvl.su">{"depot@tvl.su"}</a>
                  {"."}
                </p>
              </>
            }
        } else {
            html! {}
        }
    }
}

fn input_to_message(event: InputEvent, msg: fn(String) -> Msg) -> Msg {
    let input = event.target_unchecked_into::<HtmlInputElement>();
    if input.check_validity() {
        msg(input.value())
    } else {
        Msg::NoOp
    }
}

fn set_if_present(s: String, target: &mut Option<String>) {
    if s.is_empty() {
        *target = None;
    } else {
        *target = Some(s);
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_: &Context<Self>) -> Self {
        Self::default()
    }

    fn update(&mut self, _: &Context<Self>, msg: Self::Message) -> bool {
        log!(
            "handling message ",
            match msg {
                Msg::NoOp => "NoOp",
                Msg::SetEmail(_) => "SetEmail",
                Msg::SetUsername(_) => "SetUsername",
                Msg::SetPassword(_) => "SetPassword",
                Msg::UpdateCredentials => "UpdateCredentials",
            }
        );

        match msg {
            Msg::NoOp => return false,
            Msg::SetEmail(email) => set_if_present(email, &mut self.email),
            Msg::SetUsername(username) => set_if_present(username, &mut self.username),
            Msg::SetPassword(password) => set_if_present(password, &mut self.password),
            Msg::UpdateCredentials => {
                self.update_credentials();
            }
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();
        include!("main.html")
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

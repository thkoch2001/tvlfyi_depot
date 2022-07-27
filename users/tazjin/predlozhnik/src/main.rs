use yew::prelude::*;

use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Hash, PartialEq, Eq)]
enum Падеж {
    Именительный,
    Родительный,
    Дательный,
    Винительный,
    Творительный,
    Предложный,
}

impl Падеж {
    const ВСЕ: [Self; 6] = [
        Self::Именительный,
        Self::Родительный,
        Self::Дательный,
        Self::Винительный,
        Self::Творительный,
        Self::Предложный,
    ];

    fn вопрос(&self) -> &str {
        use Падеж::*;
        match self {
            Именительный => "Кто? Что?",
            Родительный => "Кого? Чего?",
            Дательный => "Кому? Чему?",
            Винительный => "Кого? Что?",
            Творительный => "Кем? Чем?",
            Предложный => "О ком? О чём?",
        }
    }
}

lazy_static! {
    static ref ПО_ПРЕДЛОГУ: HashMap<&'static str, Vec<Падеж>> = {
        use Падеж::*;

        hashmap! {
            "без" => vec![Родительный],
            "близ" => vec![Родительный],
            "в" => vec![Винительный, Предложный],
            "вместо" => vec![Родительный],
            "вне" => vec![Родительный],
            "возле" => vec![Родительный],
            "вокруг" => vec![Родительный],
            "вроде" => vec![Родительный],
            "для" => vec![Родительный],
            "до" => vec![Родительный],
            "за" => vec![Винительный, Творительный],
            "из" => vec![Родительный],
            "из-за" => vec![Родительный],
            "из-под" => vec![Родительный],
            "к" => vec![Дательный],
            "кроме" => vec![Родительный],
            "между" => vec![Творительный, Родительный],
            "на" => vec![Винительный, Предложный],
            "над" => vec![Творительный],
            "нет" => vec![Именительный],
            "о" => vec![Винительный],
            "обо" => vec![Винительный],
            "около" => vec![Родительный],
            "от" => vec![Родительный],
            "перед" => vec![Творительный],
            "по" => vec![Винительный, Дательный, Предложный],
            "под" => vec![Винительный, Творительный],
            "при" => vec![Предложный],
            "про" => vec![Винительный],
            "ради" => vec![Родительный],
            "с" => vec![Родительный, Винительный, Творительный],
            "сквозь" => vec![Винительный],
            "среди" => vec![Родительный],
            "у" => vec![Родительный],
            "через" => vec![Винительный],
        }
    };
    static ref ПО_ПАДЕЖУ: HashMap<Падеж, Vec<&'static str>> = {
        let mut m = hashmap!();

        for c in Падеж::ВСЕ {
            let mut предлоги: Vec<&'static str> = vec![];
            for (k, v) in &*ПО_ПРЕДЛОГУ {
                if v.contains(&c) {
                    предлоги.push(k);
                }
            }

            m.insert(c, предлоги);
        }

        m
    };
}

fn example_output() -> String {
    let mut out = String::new();

    for (пд, пги) in &*ПО_ПАДЕЖУ {
        write!(out, "Падеж: {:?}\n", пд).ok();
        for п in пги {
            write!(out, "\t{}\n", п).ok();
        }
    }

    out
}

struct Model(());

impl Component for Model {
    type Message = ();
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self(())
    }

    fn update(&mut self, _ctx: &Context<Self>, _msg: Self::Message) -> bool {
        false
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <pre>{example_output()}</pre>
        }
    }
}

fn main() {
    yew::start_app::<Model>();
}

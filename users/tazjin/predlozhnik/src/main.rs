use yew::prelude::*;

use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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
    static ref ПО_ПРЕДЛОГУ: HashMap<&'static str, BTreeSet<Падеж>> = {
        use Падеж::*;

        hashmap! {
            "без" => BTreeSet::from([Родительный]),
            "близ" => BTreeSet::from([Родительный]),
            "в" => BTreeSet::from([Винительный, Предложный]),
            "вместо" => BTreeSet::from([Родительный]),
            "вне" => BTreeSet::from([Родительный]),
            "возле" => BTreeSet::from([Родительный]),
            "вокруг" => BTreeSet::from([Родительный]),
            "вроде" => BTreeSet::from([Родительный]),
            "для" => BTreeSet::from([Родительный]),
            "до" => BTreeSet::from([Родительный]),
            "за" => BTreeSet::from([Винительный, Творительный]),
            "из" => BTreeSet::from([Родительный]),
            "из-за" => BTreeSet::from([Родительный]),
            "из-под" => BTreeSet::from([Родительный]),
            "к" => BTreeSet::from([Дательный]),
            "кроме" => BTreeSet::from([Родительный]),
            "между" => BTreeSet::from([Творительный, Родительный]),
            "на" => BTreeSet::from([Винительный, Предложный]),
            "над" => BTreeSet::from([Творительный]),
            "нет" => BTreeSet::from([Именительный]),
            "о" => BTreeSet::from([Винительный]),
            "обо" => BTreeSet::from([Винительный]),
            "около" => BTreeSet::from([Родительный]),
            "от" => BTreeSet::from([Родительный]),
            "перед" => BTreeSet::from([Творительный]),
            "по" => BTreeSet::from([Винительный, Дательный, Предложный]),
            "под" => BTreeSet::from([Винительный, Творительный]),
            "при" => BTreeSet::from([Предложный]),
            "про" => BTreeSet::from([Винительный]),
            "ради" => BTreeSet::from([Родительный]),
            "с" => BTreeSet::from([Родительный, Винительный, Творительный]),
            "сквозь" => BTreeSet::from([Винительный]),
            "среди" => BTreeSet::from([Родительный]),
            "у" => BTreeSet::from([Родительный]),
            "через" => BTreeSet::from([Винительный]),
        }
    };
    static ref ПО_ПАДЕЖУ: HashMap<Падеж, BTreeSet<&'static str>> = {
        let mut m = hashmap!();

        for c in Падеж::ВСЕ {
            let mut предлоги: BTreeSet<&'static str> = BTreeSet::new();
            for (k, v) in &*ПО_ПРЕДЛОГУ {
                if v.contains(&c) {
                    предлоги.insert(k);
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

enum Сообщение {
    ВыбралПадеж(Option<Падеж>),
    ВыбралПредлог(Option<&'static str>),
}

#[derive(Default)]
struct Модель {
    падеж: Option<Падеж>,
    предлог: Option<&'static str>,
}

struct Вывод {
    доступные_падежи: BTreeSet<Падеж>,
    доступные_предлоги: BTreeSet<&'static str>,
    объяснение: Option<Html>,
}

fn объяснить(падеж: Падеж, предлог: &str) -> Html {
    html! {
        {"NYI"}
    }
}

fn ограничить(м: &Модель) -> Вывод {
    match (м.падеж, &м.предлог) {
        (Some(пж), Some(пл)) => Вывод {
            доступные_падежи: BTreeSet::from([пж]),
            доступные_предлоги: BTreeSet::from([*пл]),
            объяснение: Some(объяснить(пж, пл)),
        },

        (Some(пж), None) => Вывод {
            доступные_падежи: BTreeSet::from([пж]),
            доступные_предлоги: (*ПО_ПАДЕЖУ)[&пж].clone(),
            объяснение: None,
        },

        (None, Some(пл)) => Вывод {
            доступные_падежи: (*ПО_ПРЕДЛОГУ)[пл].clone(),
            доступные_предлоги: BTreeSet::from([*пл]),
            объяснение: None,
        },

        (None, None) => Вывод {
            доступные_падежи: BTreeSet::new(),
            доступные_предлоги: BTreeSet::new(),
            объяснение: None,
        },
    }
}

impl Component for Модель {
    type Message = Сообщение;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Default::default()
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Сообщение::ВыбралПадеж(пж) => self.падеж = пж,
            Сообщение::ВыбралПредлог(пл) => self.предлог = пл,
        }

        true
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <pre>{example_output()}</pre>
        }
    }
}

fn main() {
    yew::start_app::<Модель>();
}

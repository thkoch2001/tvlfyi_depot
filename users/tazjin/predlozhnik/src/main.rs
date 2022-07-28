use yew::html::Scope;
use yew::prelude::*;

use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::BTreeSet;
use std::collections::HashMap;

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
            Именительный => "кто? Что?",
            Родительный => "кого? Чего?",
            Дательный => "кому? Чему?",
            Винительный => "кого? Что?",
            Творительный => "кем? Чем?",
            Предложный => "ком? Чём?",
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
            "нет" => BTreeSet::from([Родительный]),
            "о" => BTreeSet::from([Винительный, Предложный]),
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
    static ref ПАДЕЖИ: BTreeSet<Падеж> = BTreeSet::from(Падеж::ВСЕ);
    static ref ПРЕДЛОГИ: BTreeSet<&'static str> = {
        let mut s: BTreeSet<&'static str> = BTreeSet::new();

        for п in ПО_ПРЕДЛОГУ.keys() {
            s.insert(п);
        }

        s
    };
    static ref EXCEPTIONS: HashMap<(&'static str, Падеж), &'static str> = {
        use Падеж::*;

        hashmap! {
            ("в", Винительный) => "Во что? В кого?",
            ("о", Винительный) => "О кого? Обо что? (редко используется)"

        }
    };
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

fn объясни(падеж: Падеж, предлог: &str) -> Html {
    let exp = match EXCEPTIONS.get(&(предлог, падеж)) {
        Some(exp) => html! { exp },
        None => html! { format!("{} {}", предлог, падеж.вопрос()) },
    };

    html! {
        <div id="obyasnenie">
          <hr/>
          <h2>{"Пример:"}</h2>
          {exp}
        </div>
    }
}

fn ограничить(м: &Модель) -> Вывод {
    match (м.падеж, &м.предлог) {
        (Some(пж), Some(пл)) => Вывод {
            доступные_падежи: (*ПО_ПРЕДЛОГУ)[пл].clone(),
            доступные_предлоги: (*ПО_ПАДЕЖУ)[&пж].clone(),
            объяснение: Some(объясни(пж, пл)),
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
            доступные_падежи: ПАДЕЖИ.clone(),
            доступные_предлоги: ПРЕДЛОГИ.clone(),
            объяснение: None,
        },
    }
}

fn класс_кнопки(выбран: bool, доступен: bool) -> String {
    let класс = "btn ".to_string();
    класс
        + match (выбран, доступен) {
            (true, _) => "btn-primary",
            (false, true) => "btn-ghost btn-primary",
            (false, false) => "btn-ghost btn-default",
        }
}

fn покажи_предлог(
    link: &Scope<Модель>,
    м: &Модель,
    вв: &Вывод,
    п: &'static str,
) -> Html {
    let выбран = м.предлог == Some(п);
    let доступен = вв.доступные_предлоги.contains(п);
    let класс = класс_кнопки(выбран, доступен);

    html! {
        <button class={класс}
         onclick={link.callback(move |_| if выбран {
             Сообщение::ВыбралПредлог(None)
         } else {
             Сообщение::ВыбралПредлог(Some(п))
         })}
         disabled={!доступен}>
        {п}
        </button>
    }
}

fn покажи_падеж(
    link: &Scope<Модель>, м: &Модель, вв: &Вывод, п: Падеж
) -> Html {
    let выбран = м.падеж == Some(п);
    let доступен = вв.доступные_падежи.contains(&п);
    let класс = класс_кнопки(выбран, доступен);

    html! {
        <button class={класс}
         onclick={link.callback(move |_| if выбран {
             Сообщение::ВыбралПадеж(None)
         } else {
             Сообщение::ВыбралПадеж(Some(п))
         })}
         disabled={!доступен}>
        {format!("{:?}", п)}
        </button>
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

    fn view(&self, ctx: &Context<Self>) -> Html {
        let вв = ограничить(self);
        let link = ctx.link();

        let кнопки_предлогов = ПРЕДЛОГИ
            .iter()
            .map(|п| покажи_предлог(link, self, &вв, п))
            .collect::<Html>();

        let кнопки_падежов = ПАДЕЖИ
            .iter()
            .map(|п| покажи_падеж(link, self, &вв, *п))
            .collect::<Html>();

        let объяснение = вв.объяснение.map(|exp| exp).unwrap_or_else(|| html! {});

        let footer = html! {
            <footer>
              <hr/>
              <p class="footer">
                <a href="https://code.tvl.fyi/tree/users/tazjin/predlozhnik">{"код"}</a>
                {" | "}
                {"сделано "}<a href="https://tvl.su">{"ООО \"ТВЛ\""}</a>
              </p>
            </footer>
        };

        let код_рекламы = r#"
window.yaContextCb.push(()=>{
  Ya.Context.AdvManager.render({
    renderTo: 'yandex_rtb_R-A-1773485-1',
    blockId: 'R-A-1773485-1'
  })
})
"#;

        let реклама = html! {
            <div id="ad">
              <div id="yandex_rtb_R-A-1773485-1"></div>
              <script>{код_рекламы}</script>
            </div>
        };

        html! {
            <>
                <div id="header">
                  <h1>{"Предложник"}</h1>
                  <p>{"... показывает с какими падежами употребляются предлоги в русском языке."}</p>
                </div>

                <h2>{"Выбирай предлог:"}</h2>
                <div id="predlogi">
                  {кнопки_предлогов}
                </div>
                <hr/>

                <h2>{"Выбирай падеж:"}</h2>
                <div id="padezhi">
                  {кнопки_падежов}
                </div>

                {объяснение}
                {footer}
                {реклама}
            </>
        }
    }
}

fn main() {
    yew::start_app::<Модель>();
}

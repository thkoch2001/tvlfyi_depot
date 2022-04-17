use anyhow::{anyhow, Context, Result};
use std::collections::HashMap;
use std::sync::RwLock;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct TgLink {
    username: String,
    message_id: usize,
}

impl TgLink {
    fn human_friendly_url(&self) -> String {
        format!("t.me/{}/{}", self.username, self.message_id)
    }

    fn to_url(&self) -> String {
        format!("https://t.me/{}/{}?embed=1", self.username, self.message_id)
    }

    fn parse(url: &str) -> Option<Self> {
        let url = url.strip_prefix("/")?;
        let parsed = url::Url::parse(url).ok()?;

        if parsed.host()? != url::Host::Domain("t.me") {
            // only t.me links are supported
            return None;
        }

        let parts = parsed.path_segments()?.collect::<Vec<&str>>();
        if parts.len() != 2 {
            // only message links are supported
            return None;
        }

        Some(TgLink {
            username: parts[0].into(),
            message_id: parts[1].parse().ok()?,
        })
    }
}

fn fetch_embed(link: &TgLink) -> Result<String> {
    println!("fetching {}#{}", link.username, link.message_id);
    let response = crimp::Request::get(&link.to_url())
        .send()
        .context("failed to fetch embed data")?
        .as_string()
        .context("failed to decode embed data")?
        .error_for_status(|resp| {
            anyhow!("telegram request failed: {} ({})", resp.body, resp.status)
        })?;

    Ok(response.body)
}

#[derive(Debug)]
struct TgMessage {
    author: String,
    message: Option<String>,
    photos: Vec<String>,
    videos: Vec<String>,
}

fn extract_photo_url(style: &str) -> Option<&str> {
    let url_start = style.find("url('")? + 5;
    let url_end = style.find("')")?;

    Some(&style[url_start..url_end])
}

fn parse_tgmessage(embed: &str) -> Result<TgMessage> {
    use scraper::{Html, Selector};

    let doc = Html::parse_document(embed);

    let author_sel = Selector::parse("a.tgme_widget_message_owner_name").unwrap();
    let author = doc
        .select(&author_sel)
        .next()
        .ok_or_else(|| anyhow!("failed to find message author"))?
        .text()
        .collect::<Vec<&str>>()
        .concat();

    let msg_sel = Selector::parse("div.tgme_widget_message_text.js-message_text").unwrap();
    let message = doc
        .select(&msg_sel)
        .next()
        .map(|m| m.text().collect::<Vec<&str>>().concat());

    let photo_sel = Selector::parse("a.tgme_widget_message_photo_wrap").unwrap();
    let mut photos = vec![];

    for photo in doc.select(&photo_sel) {
        if let Some(style) = photo.value().attr("style") {
            if let Some(url) = extract_photo_url(style) {
                photos.push(url.to_string())
            }
        }
    }

    let video_sel = Selector::parse("i.tgme_widget_message_video_thumb").unwrap();
    let mut videos = vec![];

    for video in doc.select(&video_sel) {
        if let Some(style) = video.value().attr("style") {
            if let Some(url) = extract_photo_url(style) {
                videos.push(url.to_string())
            }
        }
    }

    Ok(TgMessage {
        author,
        message,
        photos,
        videos,
    })
}

fn shorten_link(link: &str) -> Result<String> {
    let mut url = url::Url::parse("https://tinyurl.com/api-create.php")?;
    url.query_pairs_mut().clear().append_pair("url", link);

    let request = url.as_str();

    let response = crimp::Request::get(request)
        .send()
        .context("failed to shorten URL")?
        .as_string()
        .context("failed to decode shortened URL")?
        .error_for_status(|resp| {
            anyhow!("tinyurl request failed: {} ({})", resp.body, resp.status)
        })?;

    Ok(response.body.trim().into())
}

fn to_bbcode(link: &TgLink, msg: &TgMessage) -> Result<String> {
    let mut out = String::new();

    out.push_str(&format!("[quote=\"{}\"]\n", msg.author));

    for video in &msg.videos {
        out.push_str(&format!("[url=\"{}\"]", link.to_url()));
        out.push_str(&format!("[img]{}[/img]", shorten_link(video)?));
        out.push_str("[/url]\n");
        out.push_str("[sub](Click thumbnail to open video)[/sub]\n")
    }

    for photo in &msg.photos {
        out.push_str(&format!("[timg]{}[/timg]\n", shorten_link(photo)?));
    }

    if let Some(message) = &msg.message {
        out.push_str(message);
    }

    out.push_str(&format!(
        "\n\n[sub](from [url=\"{}\"]{}[/url], via [url=\"https://tgsa.tazj.in\"]tgsa[/url])[/sub]",
        link.to_url(),
        link.human_friendly_url(),
    ));

    out.push_str("\n[/quote]\n");

    Ok(out)
}

type Cache = RwLock<HashMap<TgLink, String>>;

fn handle_tg_link(cache: &Cache, link: &TgLink) -> Result<String> {
    if let Some(bbcode) = cache.read().unwrap().get(&link) {
        println!("serving {}#{} from cache", link.username, link.message_id);
        return Ok(bbcode.to_string());
    }

    let embed = fetch_embed(&link)?;
    let msg = parse_tgmessage(&embed)?;
    let bbcode = to_bbcode(&link, &msg).context("failed to make bbcode")?;

    cache.write().unwrap().insert(link.clone(), bbcode.clone());

    Ok(bbcode)
}

fn main() {
    crimp::init();

    let cache: Cache = RwLock::new(HashMap::new());

    rouille::start_server("0.0.0.0:8472", move |request| {
        match TgLink::parse(request.raw_url()) {
            None => rouille::Response::text(
                r#"tgsa
----

this is a stupid program that lets you turn telegram message links
into BBcode suitable for pasting on somethingawful dot com

you can use it by putting a valid telegram message link in the url and
waiting a few seconds (yes it's currently slow, yes it's SA's fault,
yes I could work around but can't be bothered atm)

for example:

  https://tgsa.tazj.in/https://t.me/RWApodcast/113

yes, that looks stupid, but it works

if you see this message and think you did the above correctly, you
didn't. try again. idiot.

pm me on the forums if this makes you mad or something.
"#,
            ),
            Some(link) => {
                let result = handle_tg_link(&cache, &link);
                match result {
                    Ok(bbcode) => rouille::Response::text(bbcode),
                    Err(err) => {
                        println!("something failed: {}", err);
                        rouille::Response::text(format!(
                            r#"something broke: {}

nobody has been alerted about this and it has probably not been
logged. pm me on the forums if you think it's important enough."#,
                            err
                        ))
                    }
                }
            }
        }
    });
}

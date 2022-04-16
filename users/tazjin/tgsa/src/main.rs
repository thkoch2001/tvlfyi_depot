use anyhow::{anyhow, Context, Result};

#[derive(Debug)]
struct TgLink {
    username: String,
    message_id: usize,
}

impl TgLink {
    fn to_url(&self) -> String {
        format!("https://t.me/{}/{}?embed=1", self.username, self.message_id)
    }
}

fn fetch_embed(link: &TgLink) -> Result<String> {
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

    Ok(TgMessage {
        author,
        message,
        photos,
    })
}

fn shorten_photo_links(msg: &mut TgMessage) -> Result<()> {
    let mut url = url::Url::parse("https://tinyurl.com/api-create.php")?;
    let mut shortened = vec![];

    for photo in &msg.photos {
        url.query_pairs_mut().clear().append_pair("url", &photo);
        let request = url.as_str();

        let response = crimp::Request::get(request)
            .send()
            .context("failed to shorten URL")?
            .as_string()
            .context("failed to decode shortened URL")?
            .error_for_status(|resp| {
                anyhow!("tinyurl request failed: {} ({})", resp.body, resp.status)
            })?;

        shortened.push(response.body.trim().into());
    }

    msg.photos = shortened;

    Ok(())
}

fn to_bbcode(link: &TgLink, msg: &TgMessage) -> String {
    let mut out = String::new();

    out.push_str(&format!("[quote=\"{}\"]\n", msg.author));

    for photo in &msg.photos {
        out.push_str(&format!("[timg]{}[/timg]\n", photo));
    }

    if let Some(message) = &msg.message {
        out.push_str(message);
    }

    out.push_str("\n[/quote]\n");
    out.push_str(&format!(
        "[i](via [url=\"{}\"]Telegram[/url])[/i]",
        link.to_url(),
    ));

    return out;
}

fn main() {
    crimp::init();

    let link = TgLink {
        username: "RWApodcast".into(),
        message_id: 113,
    };

    let embed = fetch_embed(&link).unwrap();
    let mut msg = parse_tgmessage(&embed).unwrap();
    shorten_photo_links(&mut msg).unwrap();

    println!("{}", to_bbcode(&link, &msg));
}

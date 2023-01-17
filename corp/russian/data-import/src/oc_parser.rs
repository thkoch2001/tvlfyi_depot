use super::{bail, Ensure};
use log::info;
use xml::attribute::OwnedAttribute;
use xml::name::OwnedName;
use xml::reader::XmlEvent;
use xml::EventReader;

#[derive(Default, Debug)]
pub struct Grammeme {
    parent: Option<String>,
    name: String,
    alias: String,
    description: String,
}

#[derive(Debug)]
pub struct Lemma {}

#[derive(Debug)]
pub enum OcElement {
    Grammeme(Grammeme),
    Lemma(Lemma),
}

#[derive(Debug, PartialEq)]
enum ParserState {
    /// Parser is not parsing any particular section and waiting for a
    /// start tag instead.
    Init,

    /// Parser is parsing grammemes.
    Grammemes,

    /// Parser is parsing lemmata.
    Lemmata,

    /// Parser has seen the end of the line and nothing more is
    /// available.
    Ended,
}

pub struct OpenCorporaParser<R: std::io::Read> {
    reader: EventReader<R>,
    state: ParserState,
}

#[derive(PartialEq)]
enum SectionState {
    /// Actively interested in parsing this section.
    Active,

    /// Section is known, but currently ignored.
    Inactive,

    /// Section is unknown (probably a bug).
    Unknown,
}

fn section_state(section: &str) -> SectionState {
    match section {
        "grammemes" | "lemmata" => SectionState::Active,
        "restrictions" | "link_types" | "links" => SectionState::Inactive,
        _ => SectionState::Unknown,
    }
}

impl<R: std::io::Read> OpenCorporaParser<R> {
    pub fn new(reader: R) -> Self {
        let config = xml::ParserConfig::new().trim_whitespace(true);
        let reader = EventReader::new_with_config(reader, config);

        Self {
            reader,
            state: ParserState::Init,
        }
    }

    /// Pull an `OcElement` out of the parser. Returns `None` if the
    /// parser stream has ended.
    pub fn next_element(&mut self) -> Option<OcElement> {
        if self.state == ParserState::Ended {
            return None;
        }

        // Pull the next element to determine what context to enter
        // next.
        loop {
            match &self.next() {
                // no-op events that do not affect parser state
                XmlEvent::Comment(_)
                | XmlEvent::Whitespace(_)
                | XmlEvent::ProcessingInstruction { .. }
                | XmlEvent::StartDocument { .. } => continue,
                XmlEvent::StartElement { name, .. } | XmlEvent::EndElement { name }
                    if name.local_name == "dictionary" =>
                {
                    continue
                }

                // end of the file, nothing more to return
                XmlEvent::EndDocument => {
                    self.state = ParserState::Ended;
                    return None;
                }

                // some sections are skipped
                XmlEvent::StartElement { name, .. } | XmlEvent::EndElement { name }
                    if section_state(&name.local_name) == SectionState::Inactive =>
                {
                    info!("skipping {} section", name.local_name);
                    self.skip_section(&name.local_name);
                }

                // active section events start specific parser states ...
                XmlEvent::StartElement { name, .. }
                    if section_state(&name.local_name) == SectionState::Active =>
                {
                    self.state = match name.local_name.as_str() {
                        "grammemes" => ParserState::Grammemes,
                        "lemmata" => ParserState::Lemmata,
                        _ => unreachable!(),
                    };
                }

                // ... or end them
                XmlEvent::EndElement { name, .. }
                    if section_state(&name.local_name) == SectionState::Active =>
                {
                    // TODO: assert that the right section ended
                    self.state = ParserState::Init;
                }

                // actual beginning of an actual element, dispatch accordingly
                event @ XmlEvent::StartElement {
                    name, attributes, ..
                } => match self.state {
                    ParserState::Grammemes => {
                        return Some(OcElement::Grammeme(self.parse_grammeme(name, attributes)))
                    }
                    ParserState::Lemmata => {
                        return Some(OcElement::Lemma(self.parse_lemma(name, attributes)))
                    }

                    ParserState::Init | ParserState::Ended => bail(format!(
                        "parser received an unexpected start element while in state {:?}: {:?}",
                        self.state, event
                    )),
                },

                // finally, events that indicate a bug if they're
                // encountered here
                event @ XmlEvent::EndElement { .. }
                | event @ XmlEvent::CData(_)
                | event @ XmlEvent::Characters(_) => {
                    bail(format!("unexpected XML event: {:?}", event))
                }
            }
        }
    }

    /// Skip a section by advancing the parser state until we see an
    /// end element for the skipped section.
    fn skip_section(&mut self, section: &str) {
        loop {
            match self.next() {
                XmlEvent::EndElement { name } if name.local_name == section => return,
                _ => continue,
            }
        }
    }

    fn next(&mut self) -> XmlEvent {
        self.reader.next().ensure("XML parsing failed")
    }

    /// Parse a tag that should have plain string content.
    fn parse_string(&mut self, tag_name: &str) -> String {
        let mut out = String::new();

        loop {
            match self.next() {
                // ignore irrelevant things
                XmlEvent::Comment(_) | XmlEvent::Whitespace(_) => continue,

                // set the content
                XmlEvent::Characters(content) => {
                    out = content;
                }

                // expect the end of the element
                XmlEvent::EndElement { name } if name.local_name == tag_name => return out,

                // fail on everything unexpected
                event => bail(format!(
                    "unexpected element while parsing <{}>: {:?}",
                    tag_name, event
                )),
            }
        }
    }

    fn parse_grammeme(&mut self, name: &OwnedName, attributes: &[OwnedAttribute]) -> Grammeme {
        if name.local_name != "grammeme" {
            bail(format!(
                "expected to parse a grammeme, but found <{}>",
                name.local_name
            ));
        }

        let mut grammeme = Grammeme::default();

        for attr in attributes {
            if attr.name.local_name == "parent" && !attr.value.is_empty() {
                grammeme.parent = Some(attr.value.clone());
            }
        }

        loop {
            match self.next() {
                // ignore irrelevant things
                XmlEvent::Comment(_) | XmlEvent::Whitespace(_) => continue,

                // expect known tags
                XmlEvent::StartElement { name, .. } if name.local_name == "name" => {
                    grammeme.name = self.parse_string("name");
                }

                XmlEvent::StartElement { name, .. } if name.local_name == "alias" => {
                    grammeme.alias = self.parse_string("alias");
                }

                XmlEvent::StartElement { name, .. } if name.local_name == "description" => {
                    grammeme.description = self.parse_string("description");
                }

                // handle end of the grammeme
                XmlEvent::EndElement { name } if name.local_name == "grammeme" => break,

                // fail on everything unexpected
                event => bail(format!(
                    "unexpected element while parsing <grammeme>: {:?}",
                    event
                )),
            }
        }

        grammeme
    }

    fn parse_lemma(&mut self, name: &OwnedName, _attributes: &[OwnedAttribute]) -> Lemma {
        if name.local_name != "lemma" {
            bail(format!(
                "expected to parse a lemma, but found <{}>",
                name.local_name
            ));
        }

        self.skip_section("lemma");

        Lemma {}
    }
}

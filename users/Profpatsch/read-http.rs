extern crate httparse;
extern crate netencode;
extern crate arglib_netencode;
extern crate ascii;
extern crate exec_helpers;

use std::os::unix::io::FromRawFd;
use std::io::Read;
use std::io::Write;
use exec_helpers::{die_user_error, die_expected_error, die_temporary};

use netencode::{U, T};

enum What {
    Request,
    Response
}

fn main() -> std::io::Result<()> {

    let what : What = match arglib_netencode::arglib_netencode(None).unwrap() {
        T::Record(rec) => match rec.get("what") {
            Some(T::Text(t)) => match t.as_str() {
                "request" => What::Request,
                "response" => What::Response,
                _ => die_user_error("read-http arglib", "`what` should be either t:request or t:response"),
            },
            Some(o) => die_user_error("read-http arglib", format!("expected a record of text, got {:#?}", o)),
            None => {
                eprintln!("read-http arglib: no `what` given, defaulting to Response");
                What::Response
            }
        }
        o => die_user_error("read-http arglib", format!("expected a record, got {:#?}", o))
    };

    fn read_stdin_to_complete<F>(mut parse: F) -> ()
        where F: FnMut(&[u8]) -> httparse::Result<usize>
    {
        let mut res = httparse::Status::Partial;
        loop {
            if let httparse::Status::Complete(_) = res {
                return;
            }
            let mut buf = [0; 2048];
            match std::io::stdin().read(&mut buf[..]) {
                Ok(size) => if size == 0 {
                    break;
                },
                Err(err) => die_temporary("read-http", format!("could not read from stdin, {:?}", err))
            }
            match parse(&buf) {
                Ok(status) => {
                    res = status;
                }
                Err(err) => die_temporary("read-http", format!("httparse parsing failed: {:#?}", err))
            }
        }
    }


    fn normalize_headers<'a>(headers: &'a [httparse::Header]) -> Vec<(String, &'a str)> {
        let mut res = vec![];
        for httparse::Header { name, value } in headers {
            let val = ascii::AsciiStr::from_ascii(*value)
                .expect(&format!("read-http: we require header values to be ASCII, but the header {} was {:?}", name, value));
            // lowercase the headers, since the standard doesn’t care
            // and we want unique strings to match agains
            res.push((name.to_lowercase(), val.as_str()))
        }
        res
    }

    // tries to read until the end of the http header (deliniated by two newlines "\r\n\r\n")
    fn read_till_end_of_header<R: Read>(buf: &mut Vec<u8>, reader: R) -> Option<()> {
        let mut chunker = Chunkyboi::new(reader, 4096);
        loop {
            match chunker.next() {
                Some(Ok(chunk)) => {
                    buf.extend_from_slice(&chunk);
                    if chunk.windows(4).any(|c| c == b"\r\n\r\n" ) {
                        return Some(());
                    }
                },
                Some(Err(err)) => die_temporary("read-http", format!("error reading from stdin: {:?}", err)),
                None => return None
            }
        }
    }

    // max header size chosen arbitrarily
    let mut headers = [httparse::EMPTY_HEADER; 128];
    let stdin = std::io::stdin();

    match what {
        Request => {
            let mut req = httparse::Request::new(&mut headers);
            let mut buf: Vec<u8> = vec![];
            match read_till_end_of_header(&mut buf, stdin.lock()) {
                Some(()) => match req.parse(&buf) {
                    Ok(httparse::Status::Complete(_body_start)) => {},
                    Ok(httparse::Status::Partial) => die_expected_error("read-http", "httparse should have gotten a full header"),
                    Err(err) => die_expected_error("read-http", format!("httparse response parsing failed: {:#?}", err))
                },
                None => die_expected_error("read-http", format!("httparse end of stdin reached before able to parse request headers"))
            }
            let method = req.method.expect("method must be filled on complete parse");
            let path = req.path.expect("path must be filled on complete parse");
            write_dict_req(method, path, &normalize_headers(req.headers))
        },
        Response => {
            let mut resp = httparse::Response::new(&mut headers);
            let mut buf: Vec<u8> = vec![];
            match read_till_end_of_header(&mut buf, stdin.lock()) {
                Some(()) => match resp.parse(&buf) {
                    Ok(httparse::Status::Complete(_body_start)) => {},
                    Ok(httparse::Status::Partial) => die_expected_error("read-http", "httparse should have gotten a full header"),
                    Err(err) => die_expected_error("read-http", format!("httparse response parsing failed: {:#?}", err))
                },
                None => die_expected_error("read-http", format!("httparse end of stdin reached before able to parse response headers"))
            }
            let code = resp.code.expect("code must be filled on complete parse");
            let reason = resp.reason.expect("reason must be filled on complete parse");
            write_dict_resp(code, reason, &normalize_headers(resp.headers))
        }
    }
}

fn write_dict_req<'buf>(method: &'buf str, path: &'buf str, headers: &[(String, &str)]) -> std::io::Result<()> {
    let mut http = vec![
        ("method", U::Text(method)),
        ("path", U::Text(path)),
    ];
    write_dict(http, headers)
}

fn write_dict_resp<'buf>(code: u16, reason: &'buf str, headers: &[(String, &str)]) -> std::io::Result<()> {
    let mut http = vec![
        ("status", U::N6(code as u64)),
        ("status-text", U::Text(reason)),
    ];
    write_dict(http, headers)
}


fn write_dict<'buf, 'a>(mut http: Vec<(&str, U<'a>)>, headers: &'a[(String, &str)]) -> std::io::Result<()> {
    http.push(("headers", U::Record(
        headers.iter().map(
            |(name, value)|
            (name.as_str(), U::Text(value))
        ).collect::<Vec<_>>()
    )));

    netencode::encode(
        &mut std::io::stdout(),
        U::Record(http)
    )?;
    Ok(())
}


// iter helper

struct Chunkyboi<T> {
    inner: T,
    buf: Vec<u8>,
}

impl<R: Read> Chunkyboi<R> {
    fn new(inner: R, chunksize: usize) -> Self {
        let buf = vec![0; chunksize];
        Chunkyboi {
            inner,
            buf
        }
    }
}

impl<R: Read> Iterator for Chunkyboi<R> {
    type Item = std::io::Result<Vec<u8>>;

    fn next(&mut self) -> Option<std::io::Result<Vec<u8>>> {
        match self.inner.read(&mut self.buf) {
            Ok(0) => None,
            Ok(read) => {
                // clone a new buffer so we can reuse the internal one
                Some(Ok(self.buf[..read].to_owned()))
            }
            Err(err) => Some(Err(err))
        }
    }
}

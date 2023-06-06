use rouille::{Request, Response};
use std::env;

fn main() {
    let port = env::var("PORT").unwrap_or_else(|_| /* rihb = */ "7442".to_string());
    let listen = format!("0.0.0.0:{port}");
    rouille::start_server(&listen, move |_request| Response::text("hello world"));
}

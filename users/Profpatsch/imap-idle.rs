extern crate exec_helpers;
// extern crate arglib_netencode;
// extern crate netencode;
extern crate imap;
extern crate epoll;

// use netencode::dec;
use std::convert::TryFrom;
use std::io::{Read, Write};
use std::fs::File;
use std::os::unix::io::{FromRawFd, AsRawFd, RawFd};
use std::time::Duration;
use imap::extensions::idle::SetReadTimeout;

/// Implements an UCSPI client that wraps fd 6 & 7
/// and implements Write and Read with a timeout.
/// See https://cr.yp.to/proto/ucspi.txt
#[derive(Debug)]
struct UcspiClient {
    read: File,
    read_epoll_fd: RawFd,
    read_timeout: Option<Duration>,
    write: File,
}

impl UcspiClient {
    /// Use fd 6 and 7 to connect to the net, as is specified.
    /// Unsafe because fd 6 and 7 are global resources and we don’t mutex them.
    pub unsafe fn new_from_6_and_7() -> std::io::Result<Self> {
        unsafe {
            let read_epoll_fd = epoll::create(false)?;
            Ok(UcspiClient {
                read: File::from_raw_fd(6),
                read_epoll_fd,
                read_timeout: None,
                write: File::from_raw_fd(7)
            })
        }
    }
}

/// Emulates set_read_timeout() like on a TCP socket with an epoll on read.
/// The BSD socket API is rather bad, so fd != fd,
/// and if we cast the `UcspiClient` fds to `TcpStream` instead of `File`,
/// we’d break any UCSPI client programs that *don’t* connect to TCP.
/// Instead we use the (linux) `epoll` API in read to wait on the timeout.
impl SetReadTimeout for UcspiClient {
    fn set_read_timeout(&mut self, timeout: Option<Duration>) -> imap::Result<()> {
        self.read_timeout = timeout;
        Ok(())
    }
}

impl Read for UcspiClient {
    // TODO: test the epoll code with a short timeout
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        const NO_DATA : u64 = 0;
        // in order to implement the read_timeout,
        // we use epoll to wait for either data or time out
        epoll::ctl(
            self.read_epoll_fd,
            epoll::ControlOptions::EPOLL_CTL_ADD,
            self.read.as_raw_fd(),
            epoll::Event::new(epoll::Events::EPOLLIN, NO_DATA)
        )?;
        let UNUSED = epoll::Event::new(epoll::Events::EPOLLIN, NO_DATA);
        let wait = epoll::wait(
            self.read_epoll_fd,
            match self.read_timeout {
                Some(duration) => i32::try_from(duration.as_millis()).expect("duration too big for epoll"),
                None => -1 // infinite
            },
            // event that was generated; but we don’t care
            &mut vec![UNUSED; 1][..],
        );
        // Delete the listen fd from the epoll fd before reacting
        // (otherwise it fails on the next read with `EPOLL_CTL_ADD`)
        epoll::ctl(
            self.read_epoll_fd,
            epoll::ControlOptions::EPOLL_CTL_DEL,
            self.read.as_raw_fd(),
            UNUSED
        )?;
        match wait {
            // timeout happened (0 events)
            Ok(0) => Err(std::io::Error::new(std::io::ErrorKind::TimedOut, "ucspi read timeout")),
            // its ready for reading, we can read
            Ok(_) => self.read.read(buf),
            // error
            err => err,
        }
    }
}

/// Just proxy through the `Write` of the write fd.
impl Write for UcspiClient {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.write.write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.write.flush()
    }
}

/// Connect to IMAP account and listen for new mails on the INBOX.
fn main() {
    exec_helpers::no_args("imap-idle");

    // TODO: use arglib_netencode
    let username = std::env::var("IMAP_USERNAME").expect("username");
    let password = std::env::var("IMAP_PASSWORD").expect("password");

    let net = unsafe {
        UcspiClient::new_from_6_and_7().expect("no ucspi client for you")
    };
    let client = imap::Client::new(net);
    let mut session = client.login(username, password).map_err(|(err, _)| err).expect("unable to login");
    eprintln!("{:#?}", session);
    let list = session.list(None, Some("*"));
    eprintln!("{:#?}", list);
    let mailbox = session.examine("INBOX");
    eprintln!("{:#?}", mailbox);
    fn now() -> String {
        String::from_utf8_lossy(&std::process::Command::new("date").output().unwrap().stdout).trim_right().to_string()
    }
    loop {
        eprintln!("{}: idling on INBOX", now());
        let mut handle = session.idle().expect("cannot idle on INBOX");
        let () = handle.wait_keepalive().expect("waiting on idle failed");
        eprintln!("{}: The mailbox has changed!", now());
    }
}

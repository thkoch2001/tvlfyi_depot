extern crate git2;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;

const DEFAULT_BRANCH : &str = "refs/heads/main";

fn main() {
    let git_db_dir = std::env::var_os("GIT_DB_DIR").expect("set GIT_DB_DIR");
    let git_db = PathBuf::from(git_db_dir).join("git");

    std::fs::create_dir_all(&git_db).unwrap();

    let repo = git2::Repository::init_opts(
        &git_db,
        git2::RepositoryInitOptions::new()
            .bare(true)
            .mkpath(true)
            .description("git-db database")
            .initial_head(DEFAULT_BRANCH)
    ).expect(&format!("unable to create or open bare git repo at {}", &git_db.display()));

    let mut index = repo.index().expect("cannot get the git index file");
    eprintln!("{:#?}", index.version());
    index.clear().expect("could not clean the index");

    let now = std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .expect("unable to get system time");

    let now_git_time = git2::IndexTime::new(
        now.as_secs() as i32, // lol
        u32::from(now.subsec_nanos()),
    );

    let data = "hi, it’s me".as_bytes();

    index.add_frombuffer(
        &git2::IndexEntry {
            mtime: now_git_time,
            ctime: now_git_time,
            // don’t make sense
            dev: 0,
            ino: 0,
            mode: /*libc::S_ISREG*/ 0b1000 << (3+9) | /* read write for owner */ 0o644,
            uid: 0,
            gid: 0,
            file_size: data.len() as u32, // lol again
            id: git2::Oid::zero(),
            flags: 0,
            flags_extended: 0,
            path: "hi.txt".as_bytes().to_owned(),
        },
        data
    ).expect("could not add data to index");

    let oid = index.write_tree().expect("could not write index tree");

    let to_add_tree = repo.find_tree(oid)
        .expect("we just created this tree, where did it go?");

    let parent_commits = match repo.find_reference(DEFAULT_BRANCH) {
        Ok(ref_) => vec![
            ref_
            .peel_to_commit()
            .expect(&format!("reference {} does not point to a commit", DEFAULT_BRANCH))
        ],
        Err(err) => match err.code() {
            // no commit exists yet
            git2::ErrorCode::NotFound => vec![],
            _ => panic!("could not read latest commit from {}", DEFAULT_BRANCH),
        }
    };
    repo.commit(
        Some(DEFAULT_BRANCH),
        &git2::Signature::now("Mr. Authorboy", "author@example.com").unwrap(),
        &git2::Signature::now("Mr. Commiterboy", "committer@example.com").unwrap(),
        "This is my first commit!\n\
         \n\
         I wonder if it supports extended commit descriptions?\n",
        &to_add_tree,
        &parent_commits.iter().collect::<Vec<_>>()[..],
    ).expect("could not commit the index we just wrote");


}

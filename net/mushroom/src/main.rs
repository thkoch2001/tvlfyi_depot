use git2::Repository;

fn main() {
    let repo = Repository::init("/depot").expect("should be able to open depot");
}

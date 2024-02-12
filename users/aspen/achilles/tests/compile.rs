use std::process::Command;

use crate_root::root;

struct Fixture {
    name: &'static str,
    exit_code: i32,
    expected_output: &'static str,
}

const FIXTURES: &[Fixture] = &[
    Fixture {
        name: "simple",
        exit_code: 5,
        expected_output: "",
    },
    Fixture {
        name: "functions",
        exit_code: 9,
        expected_output: "",
    },
    Fixture {
        name: "externs",
        exit_code: 0,
        expected_output: "foobar\n",
    },
    Fixture {
        name: "units",
        exit_code: 0,
        expected_output: "hi\n",
    },
];

#[test]
fn compile_and_run_files() {
    let ach = root().unwrap().join("ach");

    println!("Running: `make clean`");
    assert!(
        Command::new("make")
            .arg("clean")
            .current_dir(&ach)
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .success(),
        "make clean failed"
    );

    for Fixture {
        name,
        exit_code,
        expected_output,
    } in FIXTURES
    {
        println!(">>> Testing: {}", name);

        println!("    Running: `make {}`", name);
        assert!(
            Command::new("make")
                .arg(name)
                .current_dir(&ach)
                .spawn()
                .unwrap()
                .wait()
                .unwrap()
                .success(),
            "make failed"
        );

        let out_path = ach.join(name);
        println!("    Running: `{}`", out_path.to_str().unwrap());
        let output = Command::new(out_path).output().unwrap();
        assert_eq!(output.status.code().unwrap(), *exit_code,);
        assert_eq!(output.stdout, expected_output.as_bytes());
        println!("    OK");
    }
}

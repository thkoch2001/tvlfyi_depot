// AoC 2022 - day 1.

fn sum_elf(elf: &str) -> usize {
    elf.lines()
        .map(|s| s.parse::<usize>().expect("invalid input"))
        .sum()
}

fn group_by_elf(input: &str) -> Vec<usize> {
    input.rsplit("\n\n").map(sum_elf).collect()
}

fn top_elf(input: &str) -> usize {
    group_by_elf(&input).into_iter().max().unwrap()
}

fn top_n_elves(n: usize, input: &str) -> usize {
    let mut by_elf = group_by_elf(input);
    by_elf.sort_by(|a, b| b.cmp(a)); // high->low
    (by_elf[..n]).iter().sum()
}

fn main() {
    let input = std::fs::read_to_string("input").expect("input should be in file named 'input'");
    println!("top elf: {}", top_elf(&input));
    println!("top 3 elves: {}", top_n_elves(3, &input));
}

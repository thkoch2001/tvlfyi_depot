use anyhow::anyhow;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

const PART_2: bool = true;

fn day01(is_part2: bool, numbers: &Vec<i64>) -> Result<String, anyhow::Error> {
    //println!("{:?}", numbers);

    for n1 in numbers.iter() {
        for n2 in numbers.iter() {
            if is_part2 {
                for n3 in numbers.iter() {
                    if n1 + n2 + n3 == 2020 {
                        return Ok((n1 * n2 * n3).to_string());
                    }
                }
            } else {
                if n1 + n2 == 2020 {
                    return Ok((n1 * n2).to_string());
                }
            }
        }
    }

    Err(anyhow!("no solution found"))
}

fn parse(filename: &str) -> Result<Vec<i64>, anyhow::Error> {
    let f = File::open(filename)?;
    let mut reader = BufReader::new(f);

    let mut values = Vec::<i64>::new();

    let mut line = String::new();
    loop {
        line.clear();
        reader.read_line(&mut line)?;
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            break;
        }

        values.push(trimmed_line.parse()?);
    }
    Ok(values)
}

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    //println!("{:?}", args);
    if args.len() != 2 {
        return Err(anyhow!("usage: day01 input_file"));
    }
    let filename = args.into_iter().skip(1).next().expect("args len == 1");

    let numbers = parse(&filename)?;

    println!("{}", day01(PART_2, &numbers)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::day01;

    #[test]
    fn test_part1() {
        let vec = vec![1721, 979, 366, 299, 675, 1456];
        let result = day01(false, &vec).unwrap();

        assert_eq!(result, 514579.to_string());
    }

    #[test]
    fn test_part2() {
        let vec = vec![1721, 979, 366, 299, 675, 1456];
        let result = day01(true, &vec).unwrap();

        assert_eq!(result, 241861950.to_string());
    }
}

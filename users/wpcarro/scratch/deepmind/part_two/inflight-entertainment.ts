function canTwoMoviesFillFlightBonus(
  xs: Array<number>,
  duration: number
): boolean {
  // Returns true if two movies exist that can fill the flight duration +/- 20
  // minutes.
  const seeking = {};

  for (let x of xs) {
    for (let i = 0; i < 40; i += 1) {
      if (seeking[x + i + 1]) {
        return true;
      }
    }
    for (let i = 1; i <= 20; i += 1) {
      seeking[duration - x - i] = true;
      seeking[duration - x + i] = true;
    }
  }

  return false;
}

function canTwoMoviesFillFlight(xs: Array<number>, duration: number): boolean {
  const seeking = {};

  for (let x of xs) {
    if (seeking[x]) {
      return true;
    } else {
      seeking[duration - x] = true;
    }
  }

  return false;
}

// Tests
let desc = "short flight";
let actual = canTwoMoviesFillFlight([2, 4], 1);
let expected = false;
assertEquals(actual, expected, desc);

desc = "long flight";
actual = canTwoMoviesFillFlight([2, 4], 6);
expected = true;
assertEquals(actual, expected, desc);

desc = "one movie half flight length";
actual = canTwoMoviesFillFlight([3, 8], 6);
expected = false;
assertEquals(actual, expected, desc);

desc = "two movies half flight length";
actual = canTwoMoviesFillFlight([3, 8, 3], 6);
expected = true;
assertEquals(actual, expected, desc);

desc = "lots of possible pairs";
actual = canTwoMoviesFillFlight([1, 2, 3, 4, 5, 6], 7);
expected = true;
assertEquals(actual, expected, desc);

desc = "not using first movie";
actual = canTwoMoviesFillFlight([4, 3, 2], 5);
expected = true;
assertEquals(actual, expected, desc);

desc = "only one movie";
actual = canTwoMoviesFillFlight([6], 6);
expected = false;
assertEquals(actual, expected, desc);

desc = "no movies";
actual = canTwoMoviesFillFlight([], 2);
expected = false;
assertEquals(actual, expected, desc);

function assertEquals(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`);
  }
}

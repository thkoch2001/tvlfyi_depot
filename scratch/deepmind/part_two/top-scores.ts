function sortScores(xs: Array<number>, highest: number): Array<number> {
  const counts: Array<number> = [];
  const result: Array<number> = [];

  // Initialize counts
  for (let i = 0; i <= highest; i += 1) {
    counts.push(0);
  }

  for (let i = 0; i < xs.length; i += 1) {
    counts[xs[i]] += 1;
  }

  for (let i = highest; i >= 0; i -= 1) {
    let count: number = counts[i];

    for (let j = 0; j < count; j += 1) {
      result.push(i);
    }
  }

  return result;
}

// Tests
let desc = "no scores";
let actual = sortScores([], 100);
let expected = [];
assertEqual(JSON.stringify(actual), JSON.stringify(expected), desc);

desc = "one score";
actual = sortScores([55], 100);
expected = [55];
assertEqual(JSON.stringify(actual), JSON.stringify(expected), desc);

desc = "two scores";
actual = sortScores([30, 60], 100);
expected = [60, 30];
assertEqual(JSON.stringify(actual), JSON.stringify(expected), desc);

desc = "many scores";
actual = sortScores([37, 89, 41, 65, 91, 53], 100);
expected = [91, 89, 65, 53, 41, 37];
assertEqual(JSON.stringify(actual), JSON.stringify(expected), desc);

desc = "repeated scores";
actual = sortScores([20, 10, 30, 30, 10, 20], 100);
expected = [30, 30, 20, 20, 10, 10];
assertEqual(JSON.stringify(actual), JSON.stringify(expected), desc);

function assertEqual(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`);
  }
}

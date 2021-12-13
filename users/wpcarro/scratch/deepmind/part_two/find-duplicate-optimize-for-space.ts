function findRepeatBruteForce(xs: Array<number>): number {
  // InterviewCake asks us to write a function that optimizes for space. Using
  // brute force, we can write a function that returns an answer using constant
  // (i.e. O(1)) space at the cost of a quadratic (i.e. O(n^2)) runtime.
  //
  // I did not think of this myself; InterviewCake's "Tell me more" hints
  // did. Since I think this idea is clever, I wrote a solution from memory to
  // help me internalize the solution.
  for (let i = 0; i < xs.length; i += 1) {
    let seeking = xs[i];
    for (let j = i + 1; j < xs.length; j += 1) {
      if (xs[j] === seeking) {
        return seeking;
      }
    }
  }
}

function findRepeatSort(xs: Array<number>): number {
  // This version first sorts xs, which gives the function a time-complexity of
  // O(n*log(n)), which is better than the quadratic complexity of the
  // brute-force solution. The space requirement here is constant.
  //
  // Since we need to sort xs in-place to avoid paying a O(n) space cost for
  // storing the newly sorted xs, we're mutating our input. InterviewCake
  // advises us to not mutate our input.
  xs.sort();
  let i = 0;
  let j = 1;
  for (; j < xs.length; ) {
    if (xs[i] === xs[j]) {
      return xs[i];
    }
    i += 1;
    j += 1;
  }
}

function findRepeat(xs: Array<number>): number {
  return 0;
}

// Tests
let desc = "just the repeated number";
let actual = findRepeat([1, 1]);
let expected = 1;
assertEqual(actual, expected, desc);

desc = "short array";
actual = findRepeat([1, 2, 3, 2]);
expected = 2;
assertEqual(actual, expected, desc);

desc = "medium array";
actual = findRepeat([1, 2, 5, 5, 5, 5]);
expected = 5;
assertEqual(actual, expected, desc);

desc = "long array";
actual = findRepeat([4, 1, 4, 8, 3, 2, 7, 6, 5]);
expected = 4;
assertEqual(actual, expected, desc);

function assertEqual(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`);
  }
}

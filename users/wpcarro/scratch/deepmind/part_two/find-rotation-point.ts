function findRotationPoint(xs: Array<string>): number {
  // Find the rotation point in the vector.
  let beg = 0;
  let end = xs.length - 1;

  while (beg != end) {
    let mid = beg + Math.floor((end - beg) / 2);

    if (beg === mid) {
      return xs[beg] < xs[end] ? beg : end;
    }

    if (xs[end] <= xs[mid]) {
      beg = mid;
      end = end;
    } else {
      beg = beg;
      end = mid;
    }
  }

  return beg;
}

// Tests
let desc;
let actual;
let expected;

desc = "small array one";
actual = findRotationPoint(["cape", "cake"]);
expected = 1;
assertEquals(actual, expected, desc);

desc = "small array two";
actual = findRotationPoint(["cake", "cape"]);
expected = 0;
assertEquals(actual, expected, desc);

desc = "medium array";
actual = findRotationPoint(["grape", "orange", "plum", "radish", "apple"]);
expected = 4;
assertEquals(actual, expected, desc);

desc = "large array";
actual = findRotationPoint([
  "ptolemaic",
  "retrograde",
  "supplant",
  "undulate",
  "xenoepist",
  "asymptote",
  "babka",
  "banoffee",
  "engender",
  "karpatka",
  "othellolagkage"
]);
expected = 5;
assertEquals(actual, expected, desc);

function assertEquals(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`);
  }
}

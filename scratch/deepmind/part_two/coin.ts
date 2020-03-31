// The denomination of a coin.
type Coin = number;

// The amount of change remaining.
type Amount = number;

// Mapping of Coin -> Int
type CoinBag = Map<Coin, number>;

function createCoinBag(coins: Coin[]): CoinBag {
  const result = new Map();

  for (const coin of coins) {
    result.set(coin, 0);
  }

  return result;
}

// This algorithm should work conceptual, but it does not actually
// work. JavaScript uses reference equality when constructing a Set<Map<A,B>>,
// so my result.size returns a higher number than I expect because it contains
// many duplicate entries.
//
// Conceptually, I'm not sure this solution is optimal either -- even after I
// can dedupe the entries in `result`.
function changePossibilities(amt: Amount, coins: Coin[]): number {
  if (amt === 0) {
    return 1;
  }
  const result: Set<CoinBag> = new Set();

  const q: [Coin, Amount, CoinBag][] = [];

  for (const coin of coins) {
    const bag = createCoinBag(coins);
    bag.set(coin, 1);
    q.push([coin, amt - coin, bag]);
  }

  while (q.length > 0) {
    const [coin, amt, bag] = q.shift();

    console.log([coin, amt, bag]);

    if (amt === 0) {
      result.add(bag);
    } else if (amt < 0) {
      continue;
    } else {
      for (const c of coins) {
        const bagCopy = new Map(bag);
        const value = bagCopy.get(c);
        bagCopy.set(c, value + 1);
        q.push([c, amt - c, bagCopy]);
      }
    }
  }
  console.log(result);
  return result.size;
}

// Tests
let desc = "sample input";
let actual = changePossibilities(4, [1, 2, 3]);
let expected = 4;
assertEqual(actual, expected, desc);

desc = "one way to make zero cents";
actual = changePossibilities(0, [1, 2]);
expected = 1;
assertEqual(actual, expected, desc);

desc = "no ways if no coins";
actual = changePossibilities(1, []);
expected = 0;
assertEqual(actual, expected, desc);

desc = "big coin value";
actual = changePossibilities(5, [25, 50]);
expected = 0;
assertEqual(actual, expected, desc);

desc = "big target amount";
actual = changePossibilities(50, [5, 10]);
expected = 6;
assertEqual(actual, expected, desc);

// I think InterviewCake designed this assertion to be computationally
// expensive.
desc = "change for one dollar";
actual = changePossibilities(100, [1, 5, 10, 25, 50]);
expected = 292;
assertEqual(actual, expected, desc);

function assertEqual(a, b, desc) {
  if (a === b) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL: ${a} != ${b}`);
  }
}

// Returns a new string comprised of every characters in `xs` except for the
// character at `i`.
function everyOtherChar(xs: string, i: number): string[] {
  const result = [];

  for (let j = 0; j < xs.length; j += 1) {
    if (i !== j) {
      result.push(xs[j]);
    }
  }

  return [xs[i], result.join('')];
}

function getPermutations(xs: string): Set<string> {
  if (xs === '') {
    return new Set(['']);
  }

  const result: Set<string> = new Set;

  for (let i = 0; i < xs.length; i += 1) {
    const [char, rest] = everyOtherChar(xs, i);
    const perms = getPermutations(rest);

    for (const perm of perms) {
      result.add(char + perm);
    }
  }

  return result;
}

// Tests
let desc = 'empty string';
let input = '';
let actual = getPermutations(input);
let expected = new Set(['']);
assert(isSetsEqual(actual, expected), desc);

desc = 'one character string';
input = 'a';
actual = getPermutations(input);
expected = new Set(['a']);
assert(isSetsEqual(actual, expected), desc);

desc = 'two character string';
input = 'ab';
actual = getPermutations(input);
expected = new Set(['ab', 'ba']);
assert(isSetsEqual(actual, expected), desc);

desc = 'three character string';
input = 'abc';
actual = getPermutations(input);
expected = new Set(['abc', 'acb', 'bac', 'bca', 'cab', 'cba']);
assert(isSetsEqual(actual, expected), desc);

desc = 'four character string';
input = 'abca';
actual = getPermutations(input);
expected = new Set([
  'abca', 'abac', 'acba', 'acab', 'aabc', 'aacb', 'baca', 'baac', 'bcaa',
  'bcaa', 'baac', 'baca', 'caba', 'caab', 'cbaa', 'cbaa', 'caab', 'caba',
  'aabc', 'aacb', 'abac', 'abca', 'acab', 'acba'
]);
assert(isSetsEqual(actual, expected), desc);

function isSetsEqual(as, bs) {
  if (as.size !== bs.size) {
    return false;
  }
  for (let a of as) {
    if (!bs.has(a)) return false;
  }
  return true;
}

function assert(condition, desc) {
  if (condition) {
    console.log(`${desc} ... PASS`);
  } else {
    console.log(`${desc} ... FAIL`);
  }
}

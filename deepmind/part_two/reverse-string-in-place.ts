// Reverse array of characters, `xs`, mutatively.
function reverse(xs: Array<string>) {
  let i: number = 0;
  let j: number = xs.length - 1;

  while (i < j) {
    let tmp = xs[i];
    xs[i] = xs[j]
    xs[j] = tmp
    i += 1
    j -= 1
  }
}

# Netstring

Netstrings are a djb invention. They are intended as a serialization format. Instead of inline control characters like `\n` or `\0` to signal the end of a string, they use a run-length encoding given as the number of bytes, encoded in ASCII, at the beginning of the string.

```
hello -> 5:hello,
foo! -> 4:foo!,
こんにちは -> 15:こんにちは,
```

They can be used to encode e.g. lists by simply concatenating and reading them in one-by-one.

If you need a more complex encoding, you could start encoding e.g. tuples as netstrings-in-netstrings, or you could use [`netencode`](../netcencode/spec.md) instead, which is what-if-json-but-netstrings, and takes the idea of netstrings to their logical conclusion.

Resources:

Spec: http://cr.yp.to/proto/netstrings.txt
Wiki: https://en.wikipedia.org/wiki/Netstring

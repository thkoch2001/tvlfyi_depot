These are the slides for a talk at the Moscow Rust User Group /
ProgMSK on 2023-09-07.

After building, the presentation can be launched with `pdfpc`
(available in `nixpkgs`), like this:

```
pdfpc --windowed=both result/presentation.pdf -R presentation.pdfpc -d 40
```

I keep the JSON file formatted using `jq . presentation.pdfpc | sponge
presentation.pdfpc` for easier diffs.

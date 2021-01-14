(in-package :panettone.tests)
(declaim (optimize (safety 3)))

(defmacro inline-markdown-unit-test (name input expected)
  `(test ,name
     (is (equal
           ,expected
           (with-output-to-string (*standard-output*)
             (render-inline-markdown ,input))))))

(inline-markdown-unit-test
  inline-markdown-typical-test
  "hello _world_, here is ~~no~~ `code`!"
  "hello <em>world</em>, here is <del>no</del> <code>code</code>!")

(inline-markdown-unit-test
  inline-markdown-two-emphasize-types-test
  "_stress_ *this*"
  "<em>stress</em> <em>this</em>")

(inline-markdown-unit-test
  inline-markdown-html-escaping-test
  "<tag>öäü"
  "&lt;tag&gt;&#246;&#228;&#252;")

(inline-markdown-unit-test
  inline-markdown-nesting-test
  "`inside code *anything* goes`, but also ~~*here*~~"
  "<code>inside code *anything* goes</code>, but also <del>*here*</del>")

(inline-markdown-unit-test
  inline-markdown-escaping-test
  "A backslash \\\\ shows: \\*, \\_, \\` and \\~~"
  "A backslash \\ shows: *, _, ` and ~~")

(inline-markdown-unit-test
  inline-markdown-nested-escaping-test
  "`prevent \\`code\\` from ending, but never stand alone \\\\`"
  "<code>prevent `code` from ending, but never stand alone \\</code>")

(inline-markdown-unit-test
  inline-markdown-escape-normal-tokens-test
  "\\Normal tokens \\escaped?"
  "\\Normal tokens \\escaped?")

(inline-markdown-unit-test
  inline-markdown-no-unclosed-tags-test
  "A tag, once opened, _must be closed"
  "A tag, once opened, <em>must be closed</em>")

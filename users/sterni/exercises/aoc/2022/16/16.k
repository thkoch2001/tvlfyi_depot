/example
b:`AA `DD `DD `CC `BB `BB `AA `II `JJ `JJ `II `AA `DD `EE `FF `GG `HH `HH `GG `FF `EE `EE `DD `CC `CC

/ parsing
(f;r;t):+{x[1 4],,9_x^'","}'" "\' 0:"input"
(f;t):`s$''(f;t)
r:f!{`I$x[&(x<58)&47<x]}' r
g:f!t

/ nplic: {[n;x] u[n<+/'x=/:u:?x]}

/ total flow scoring
tf: {+/+\{x,(30-#x)#0}((r.)'x)*`XX=':x}

/ initial state
i:,,`AA
/ valves to open
vto: f^(=r).0;

best: {x[&s>(?s)[>?s:tf'x]@10]}
best: {x[(1000&#x)#>tf'x]}

p:{[n;ps]
  ms:ps[&~fin:(#vto)={#?x[&`X=':x]}'ps];
  rt: best[ps[&fin],(ms[w],'(,*|)'ms[w:&{(0|/vto=l)&~|/0&':x=l:*|x}'ms]),,/{x,/:,'g[*|x]}' ms];
  $[n>1;o[n-1;rt];rt]}

*tf'p[29;i]

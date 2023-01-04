/ parsing
(f;r;t):+{x[1 4],,9_x^'","}'" "\' 0:"input"
(f;t):`s$''(f;t)
r:f!{`I$x[&(x<58)&47<x]}' r
g:f!t

/ total flow scoring
tf: {+/+\{x,(30-#x)#0}((r.)'x)*`XX=':x}

/ valves to open
vto: f^(=r).0;

/ paths to keep after each step
best: {x[(1000&#x)#>tf'x]}

p:{[n;ps]
  ms:ps[&~fin:(#vto)={#?x[&`X=':x]}'ps];
  rt: best[ps[&fin],(ms[w],'(,*|)'ms[w:&{(0|/vto=l)&~|/0&':x=l:*|x}'ms]),,/{x,/:,'g[*|x]}' ms];
  $[n>1;o[n-1;rt];rt]}

*tf'p[29;,,`AA]

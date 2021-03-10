{ ... }:

let
  mainWidth = "800px";
in ''
  body {
    font-size: 1em;
    line-height: 1.5;
    font-family: serif;
    background-color: #efefef;
  }

  h1, h2, h3, h4, h5, h6 {
    font-family: sans-serif;
    font-size: 1em;
    margin: 5px 0;
  }

  h1 {
    margin-top: 0;
  }

  a:link, a:visited {
    color: #3e7eff;
  }

  h1 a, h2 a, h3 a, h4 a, h5 a, h6 a {
    text-decoration: none;
  }

  .manual-text, .index-text {
    padding: 20px;
    max-width: ${mainWidth};
    background-color: white;
    margin: 0 auto;
  }

  table.head, table.foot {
    display: none;
  }

  .Nd {
    display: inline;
  }
''

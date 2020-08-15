# Dotted Squares

This is my second attempt at solving this problem. I had an hour to solve it the
first time, and I unfortunately came up short although I made good progress.

The problem asks to read input from a text file that looks like this:

```
1     -- board width
1     -- board height
4     -- number of lines of "moves" (below)
0 0 R -- create a unit vector (0,0) facing right
0 0 U -- create a unit vector (0,0) facing up
0 1 L -- create a unit vector (0,1) facing left
1 1 D -- create a unit vector (1,1) facing down
```

After parsing and validating the input, score the outcome a game where players
one and two alternatively take turns drawing lines on a board. Anytime one of
the players draws a line that creates a square from existing lines, they get a
point.

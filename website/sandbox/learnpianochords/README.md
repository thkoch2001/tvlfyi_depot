# Learn Piano Chords (LPC)

Are you a musician looking for a more effective way to improve your craft? Maybe
you're a music teacher looking to create useful exercises to give your students.

Studying music theory can be a fruitful undertaking, but it can often overwhelm
or bore students. I think that if practicing is enjoyable, students will
practice more. Practice doesn't make perfect; *perfect* practice makes perfect.
Learn Piano Chords is a web app that lowers the barrier to practicing and
internalizing music theory.

## How does it work?

1. Grab a cell phone or a laptop and your instrument.
2. Open a web browser and visit the Learn Piano Chords app (URL and app
   forthcoming).
3. Set the tempo at which you would like to practice.
4. Set the target duration of your session.
5. Select the key(s) and chord(s) you would like to practice.
6. Set the tempo (i.e. pace) at which you would like to practice.
7. LPC will display chords at various rhythmic intervals during your practice
   session. It is your job to play these chords in time before the next chord
   appears.

## Highlights

Here are some useful features of LPC:
- Tempo: Set the rate at which LPC displays chords.
- Predefined practice sessions: LPC offers users a few practice sessions to get
  users started. The goal, however, is to teach users to create their own
  bespoke practice sessions. LPC aims to foster a community of practitioners who
  curate and share their practice sessions.
- Whitelist / blacklist: Construct the set of chords you would like to
  practice. Let's say you only want to practice triads in the keys of F, C, and
  G. Would you also like to avoid diminished chords? Or maybe you *only* want to
  practice major-7th chords for *all* keys. LPC supports all of these scenarios
  and many others. You can save these chord configurations to reuse them at any
  time. You can also share chord configurations with other LPC users if you find
  the practice useful.
- Inversions: Every chord has inversions. For instance, every triad (i.e. chord
  composed of three notes) has three inversions: root, second, and third
  positions. LPC acknowledges all of the positions in which chords may appear
  and helps you study all, some, or none of these inversions.
- Harmony: LPC understands basic harmony and can sort the chords you would like
  to train in various harmonious permutations.
- Chaos-mode: Feeling confident? Throw the classical notions of harmony to the
  wayside and use LPC in "chaos-mode" where LPC samples randomly from the Circle
  of Fifths.

## Developing

If you're interested in contributing, the following will create an environment
in which you can develop:

```shell
$ nix-shell
$ elm-live -- src/Main.elm --output=elm.js
```

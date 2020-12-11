# Habit Screens

Problem: I would like to increase the rate at which I complete my daily, weekly,
monthly, yearly habits.

Solution: Habit Screens are mounted in strategic locations throughout my
apartment. Each Habit Screen displays the habits that I should complete that
day, and I can tap each item to mark it as complete. I will encounter the Habit
Screens in my bedroom, kitchen, and bathroom, so I will have adequate "cues" to
focus my attention. By marking each item as complete and tracking the results
over time, I will have more incentive to maintain my consistency
(i.e. "reward").

## Elm

Elm has one of the best developer experiences that I'm aware of. The error
messages are helpful and the entire experience is optimized to improve the ease
of writing web applications.

### Developing

If you're interested in contributing, the following will create an environment
in which you can develop:

```shell
$ nix-shell
$ npx tailwindcss build index.css -o output.css
$ elm-live -- src/Main.elm --output=Main.min.js
```

You can now view your web client at `http://localhost:8000`!

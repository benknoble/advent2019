Execute the SML solution with

```
sml solution.cm
- Solution.solve "input";
```

Requires a `RedBlackMapFn` (such as from SML/NJ).

You can play the game with

```
sml solution.cm
- Solution.manualFile "input";
```

The input controls are l/L to move the paddle left, r/R to move right, anything
else to stay stationary. You must hit enter (1).

1. Input is line-buffered from the console, so we can't even detect that you've
   pressed 'r' until *after* you hit enter.

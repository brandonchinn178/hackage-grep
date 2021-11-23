# hackage-grep

`grep` packages on Hackage.

```
stack build
stack exec -- hackage-grep '^class [[:alpha:]]* =' --limit 10
```

Shells out to `grep`, so look at `man grep` to see what format the pattern should be.

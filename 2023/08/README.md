First of all I tried just extending my part one solution to advance
multiple nodes concurrently. That was taking aeges. My first
optimisation was to (incorrectly) assume that at some point, ghosts
would end up on the same `Z`, which would allow you to treat them as a
single unit. Of course, that never happened. So I figured: let me add
some memoization. My approach would be to have a minimum priority
queue (conceptually) of ghosts. At each step, I would pop one off the
stack, and advance it to the next `Z`. I would then make a cache entry
where `cache{INITIAL} = {FINAL, DELTA}`. I then realised that this
wouldn't actually work as I had to remember the location the ghost was
in the path when it was at `INITIAL`, so I then had
`cache{INITIAL-LOCATION} = {FINAL, DELTA}`. I was quite shocked when
`LOCATION`, which was described by `STEPS % LENGTH(PATH)` was always
zero. Every `DELTA` was divisible by `263`. That `263` was a prime
number did not really occur to me at this point. That it was
universally true that the path length would be a factor of the delta
was also dubious due to it not being the case for some of the test
inputs. I then started dumping the cache out and I realised that each
`Z` mapped to itself, i.e. ghosts only had a single possible final
destination. I also realisd that the `A → Z` distances were equal to
the `Z → Z` distances. I thought, maybe I can use the chinese
remainder theorem? I couldn't figure out a way to get the mapping from
`STEPS → Ai*Mi*mi` and thought that it was too dificult. Then I
started thinking about it as each ghost's distance just being the
result of repeatedly adding the same delta i.e. multiplication. So we
just had to find the LCM of each ghost's delta. I copied each delta
into my terminal, stripped out the cruft, and executed:

```zsh
print -l PRIMES... | xargs factor
```

I then multiplied all the factors together and got the correct
answer. I stripped the code that gave me the deltas in `2.pl` to
`2-short.pl`.

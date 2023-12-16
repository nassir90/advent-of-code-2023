# Notes

I wanted to refine my brute force solution to work for part 2 but it
took me longer than it would have taken me to make my optimal
solution. That said, the main trick here was recognising the cycle,
which happened after only 84 cycles in my case. It would actually be a
better approach to keep the brute force logic and focus on cycle
recognition as the brute force approach probably won't take an
unreasonable amount of time to complete.

Also attatched is a rust project which does about 2.7 million cycles
per second and can therefore brute force part 2 in six minutes. It
uses the same rock representation as the perl codebase (read the file
for details), but all hashmaps have been replaced with arrays and most
of the math is done using the `i8` type. That these optimisations
would be useful were made possible by using the `perf`
program. Basically, to profile a rust codebase:

```
perf record -F99 --call-graph dawrf cargo … # generate dataset
perf report --no-inline                     # read dataset (quickly)
```

Even something as simple as looping over the hashset which I was using
to store each block (`#`) became a bottleneck and showed up in the
performance trace. I replaced it with an simple vector afterwards.

Anyway, run the rust version via:

```
TARGET=TARGET cargo run --release < INPUT
```

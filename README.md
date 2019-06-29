# Haskell vs. Rust benchmark on a regex matcher

Regex matcher algorithm courtesy of [A Play on Regular Expressions](https://sebfisch.github.io/haskell-regexp/regexp-play.pdf).


### Note

I don't really know what I'm doing with the haskell code optimisations
(strictness, inlining, etc.) All I know is how to use the time and heap
profiler, and then everything was basically trial and error.

Any optimisation tips would be greatly appreciated.


## Haskell results

```
Benchmark regex-bench: RUNNING...
benchmarking ab/1_000
time                 166.6 μs   (138.8 μs .. 198.7 μs)
                     0.903 R²   (0.863 R² .. 1.000 R²)
mean                 143.6 μs   (139.6 μs .. 153.2 μs)
std dev              26.50 μs   (11.82 μs .. 47.24 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking ab/10_000
time                 1.390 ms   (1.385 ms .. 1.395 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.452 ms   (1.414 ms .. 1.527 ms)
std dev              263.7 μs   (157.1 μs .. 409.4 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking ab/100_000
time                 13.97 ms   (13.89 ms .. 14.08 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.26 ms   (14.16 ms .. 14.46 ms)
std dev              623.7 μs   (413.9 μs .. 1.024 ms)
variance introduced by outliers: 32% (moderately inflated)

Benchmark regex-bench: FINISH
```


## Rust results

```
1_000                   time:   [87.270 us 87.279 us 87.290 us]
Found 8 outliers among 100 measurements (8.00%)
  3 (3.00%) high mild
  5 (5.00%) high severe

10_000                  time:   [874.88 us 874.95 us 875.03 us]
Found 10 outliers among 100 measurements (10.00%)
  1 (1.00%) low mild
  6 (6.00%) high mild
  3 (3.00%) high severe

100_000                 time:   [8.8100 ms 8.8111 ms 8.8122 ms]
Found 7 outliers among 100 measurements (7.00%)
  1 (1.00%) low mild
  3 (3.00%) high mild
  3 (3.00%) high severe
```

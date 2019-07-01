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
time                 101.6 μs   (101.5 μs .. 101.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 101.7 μs   (101.7 μs .. 101.9 μs)
std dev              374.6 ns   (162.1 ns .. 755.6 ns)

benchmarking ab/10_000
time                 1.032 ms   (1.031 ms .. 1.032 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.033 ms   (1.033 ms .. 1.034 ms)
std dev              1.982 μs   (1.327 μs .. 3.379 μs)

benchmarking ab/100_000
time                 10.28 ms   (10.27 ms .. 10.29 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.30 ms   (10.30 ms .. 10.31 ms)
std dev              29.24 μs   (19.88 μs .. 50.67 μs)

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

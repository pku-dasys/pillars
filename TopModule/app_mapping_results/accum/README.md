Mapping Results of Accumulate
=====================

## C++ codes
``` cpp
for (i = 0; i < N - 1; i++) {
        c[i] *= a[i] + b[i + 1];
        sum += c[i];
    }
```

## DFG node and corresponding values
* const1 <= 1
* const3 <= a_base
* const6 <= b_base
* const8 <= 1
* const12 <= c_base

## Throughput

throughput = add0.skew / II + 1

The "throughput" indicates after how many times of such repeats, a new result can be obtained.
For example, if II = 2 and throughput = 2, we can obtain a new result every 4 cycles.
However, it is not recommended to use this parameter, and we suggest users to
update the target architecture or mapping parameters.
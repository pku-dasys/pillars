DFG of Vector Addition
=====================

## C++ codes
``` cpp
for (i = 0; i < N; i++) {
        c[i] = a[i] + b[i];
    }
```

## DFG node and corresponding values
* const1 <= a_base
* const4 <= b_base
* const8 <= c_base
* const11 <= 1

## Throughput
throughput = add10.skew / II + 1

The "throughput" indicates after how many times of such repeats, a new result can be obtained.
For example, if II = 2 and throughput = 2, we can obtain a new result every 4 cycles.
However, it is not recommended to use this parameter, and we suggest users to
update the target architecture or mapping parameters.
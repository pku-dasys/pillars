Mapping Results of Sum
=====================

## C++ codes
``` cpp
for (i = 0; i < N; i++) {
        sum += a[i];
    }
```

## DFG node and corresponding values
* const1 <= a_base
* const6 <= 1

## Throughput
throughput = add5.skew / II + 1

The "throughput" indicates after how many times of such repeats, a new result can be obtained.
For example, if II = 2 and throughput = 2, we can obtain a new result every 4 cycles.
However, it is not recommended to use this parameter, and we suggest users to
update the target architecture or mapping parameters.

NOTE:
If add3.skew is not equal to add5.skew,
we suggest GRB.IntParam.MIPFocus in gurobimap_java should be set to 2 to make a time-quality trade off.
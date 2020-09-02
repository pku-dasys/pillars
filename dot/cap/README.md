DFG of Cap
=====================

## C++ codes
``` cpp
for (i = 0; i < N; i++) {
        int bb = (((((a[i] * 3 * c1) >> 2)) * c1 ))
               * (((((m[i] * 3 * a[i]) >> 2)) * a[i]));
        b[i] = bb;
    }
```

## DFG node and corresponding values
* const1 <= a_base
* const4 <= 3
* const6 <= c1_addr
* const9 <= 2
* const11 <= m_base
* const15 <= 2
* const20 <= b_base
* const23 <= 1

## Throughput
throughput = add22.skew / II + 1

The "throughput" indicates after how many times of such repeats, a new result can be obtained.
For example, if II = 2 and throughput = 2, we can obtain a new result every 4 cycles.
However, it is not recommended to use this parameter, and we suggest users to
update the target architecture or mapping parameters.
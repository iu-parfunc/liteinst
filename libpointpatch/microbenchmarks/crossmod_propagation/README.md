
Cross-modification latency
==========================

We observe that writing a single, non-straddling instruction such as
INT3 (0xCC) is eventually picked up by other processors, in spite of
micro-op caching and other effects.

This microbenchmark measures the latency for other threads, at various
points in the machine topology, to pick up the changed instruction.


General microbenchmarks for gathering platform information
----------------------------------------------------------

This directory used to contain our instrumentation microbenchmarks.

Those have been retired to ../archived/3rd_gen/microbenchmarks

What remains tests basic overheads on the target Linux platform, or
overheads of underlying libraries we use.

 * run-benchmarks.hs - the HSBencher harness that runs the benchmarks.

 * `sigtrap/` - cost to follow an interrupt to sig handler
 * `backtrace/` - cost to get length of backtrace
 * `dynainst/` - report basic dyninst instrumentation overhead
 * `papi/` - ??

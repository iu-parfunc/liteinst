
Implementation
==============

    Feature                               Depends on              

[1] Code JIT Implementation
    (JIT code for trampolines, 
     springboards and callouts etc.)
[2] Relocator 
[3] Data structures
   [3.1] Reader, Writer Lock                
   [3.2] Concurrent hash map                [3.1]
[4] Control Routing Implementation           [3]  
[5] Instruction Pun Implementation      [1], [2], [3], [4]   
[6] Liteprobe provider implementation   [1], [2], [3], [4], [5]
[7] Liteprof implementation             [1], ... , [6]

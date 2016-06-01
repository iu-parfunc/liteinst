
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
[4] Arena allocator                          
[5] Control Routing Implementation           [3]  
[6] Instruction Pun Implementation      [1], [2], [3], [4], [5]   
[7] Liteprobe provider implementation   [1], [2], [3], [4], [5], [6]
[8] Liteprof implementation             [1], ... , [7]

1. Finish process.hpp : Done
2. Implement process_analyzer.cpp  : Done
    - Consolidate proc.cpp
    - Consolidate elf.cpp
    - Consolidate analysis.cpp
3. Incoporate boostrap trickery. : Done
4. Check failure cases for the punning?   
5. Commit libaudit fixes.          : Done
6. Get numbers from libaudit on the applications. : Done
7. Implement rprobes.
    Ideas
     - Springboard -> Trampoline -> Instrumentation
     - Monotonically growing springboards
     - Eventual probe deactivation`;
8. Incoporate rprobes in to liteinst framework.
9. Refactor out libinfra (both C and C++ interface)
    - Process analyzer
    - Function analyzer
    - Signal handling (Signal chaining)

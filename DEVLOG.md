### 6-29-13 ###

#### Self-reading binary ####
Working on getting the program to its binary through fopen(). Was unable to open in r+ (read/write). Also attempted to use ios:out with fstream, but that did not work eiter. Going to copy contents to buffer, modify buffer, and write to a new binary until I can
figure out a solution.

Keep in mind: the location of the probe-ready site will change if code is inserted in front of
it, so all the instrumentation code must come afterwards.

#### ToDo ####
1. Get r+ working.
2. Determine why the location of the probesite is far higher than the total size of the binary.

------------------

### 6-5-13 ####

#### DWARF encoding ####
Address of tag argument is DWARF encoded. Looking into libdwarf to read this data.

#### Pintool ####
Previously created a pintool utilizing libzca to insert function calls into test program.
Working on fixing "undefined reference" compiler error.

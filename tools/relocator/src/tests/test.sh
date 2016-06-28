#!/bin/sh 

fail=0; 
succ=0; 

for EXE in *.exe 
do 
    ./${EXE}
    if [ "$?" == "0" ]; 
      then fail=$((fail + 1)); 
      else succ=$((succ + 1)); 
    fi

done 

echo "Failed tests: $fail"
echo "Passed tests: $succ" 

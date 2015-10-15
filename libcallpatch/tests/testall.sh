#!/bin/bash

success=0;
failed=0;


echo "------------ [Wait Free] Running Straddler Tests ----------"
echo ""
for f in *.exe ; do
  for i in {1..4}; do
    echo '*******************************************'
    echo "RUNNING TEST $f - $i"
    echo '*******************************************'
    ./${f} $i 
    if [ $? -eq 0 ]; then
      echo '*****************************************'
      echo "PASSED"
      echo '*****************************************'
      echo ""
      success=$((success+1))
    else
      echo '*****************************************'
      echo "FAILED! "
      echo '*****************************************'
      echo ""

      fails="$fails $f-$i"
      failed=$((failed+1)) 
    fi
  done
done

echo ""
echo ""

echo "***** Test summary *****"
echo "Num passed tests: " $success
echo "Num failed tests: " $failed
echo "Failed tests: " $fails

if [ "$failed" == "0" ];
then
  exit 0;
else
  exit 1;
fi


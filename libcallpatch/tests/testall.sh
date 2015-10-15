#!/bin/bash

success=0;
failed=0;


echo "------------ [Wait Free] Running Straddler Tests ----------"
echo ""
for i in {1..4}; do
  echo '******************************'
  echo "RUNNING FOR STRDDLER POINT $i"
  echo '******************************'
  STRADDLER_POINT=$i make test
  if [ $? -eq 0 ]; then
    echo '****************************'
    echo "PASSED"
    echo '****************************'
    echo ""
    success=$((success+1))
  else
    echo '****************************'
    echo "FAILED! "
    echo '****************************'
    echo ""

    fails="$fails $i"
    failed=$((failed+1)) 
  fi
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


#! /bin/bash

success=0;
failed=0;

fails=""


for f in *.exe ; do
  if [ $f  =  '*.exe' ] ;
  then echo "Build tests first"
    exit;
  fi;
  echo ""
  echo ""

  echo '****************************'
  echo 'RUNNING TEST'
  echo '****************************'

  readelf -s ${f} |grep FUNC | awk -F '[[:space:]]+' '{print $3 "," $9}' > \
    functions.txt

  LD_LIBRARY_PATH=../../build/lib:$LD_LIBRARY_PATH ./${f}
  if [ $? -eq 0 ]; then
    echo '****************************'
    echo "$f PASSED"
    echo '****************************'
    success=$((success+1))
  else
    echo '****************************'
    echo "$f FAILED! "
    echo '****************************'

    fails="$fails $f"
    failed=$((failed+1))
  fi

  rm -f functions.txt
done;

echo -e "\n***** Test summary *****"
echo "Num passed tests: " $success
echo "Num failed tests: " $failed
echo "Failed tests: " $fails

if [ "$failed" == "0" ];
then
  exit 0;
else
  exit 1;
fi
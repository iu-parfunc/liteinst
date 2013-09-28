#!/bin/bash

rm -f run_triad.csv run_triad.avg run_time.csv run_time.avg
export VT_UNIFY=no
for i in {1..10}
do
    rm -f out outt *.z *.otf *.thumb *.uctl
    /usr/bin/time -p -o outt numactl -C 2 -l ./stream_c.exe >out
    num=`cat out | grep Triad: | awk '{print $2'}`
    good=`echo $num'>'4800 | bc -l`
    if [ "$good" = "1" ]; then 
	cat out | grep Triad: | awk '{print $2","$3","$4","$5}' >>run_triad.csv
	elap=`cat outt | grep real | cut -d" " -f 2`
	user=`cat outt | grep user | cut -d" " -f 2`
	syst=`cat outt | grep sys | cut -d" " -f 2`
	echo $elap,$user,$syst>>run_time.csv
    fi
done
rm -f out outt *.z *.otf *.thumb *.uctl
count=`cat run_triad.csv | wc -l`

sum=`cat run_triad.csv | cut -d"," -f1 | awk '{s+=$1} END {print s}'`
avg1=`bc <<< "scale = 2; ($sum / $count)"`
sum=`cat run_triad.csv | cut -d"," -f2 | awk '{s+=$1} END {print s}'`
avg2=`bc <<< "scale = 7; ($sum / $count)"` 
sum=`cat run_triad.csv | cut -d"," -f3 | awk '{s+=$1} END {print s}'`
avg3=`bc <<< "scale = 7; ($sum / $count)"`
sum=`cat run_triad.csv | cut -d"," -f4 | awk '{s+=$1} END {print s}'`
avg4=`bc <<< "scale = 7; ($sum / $count)"`

echo $avg1,$avg2,$avg3,$avg4>>run_triad.avg

sum=`cat run_time.csv | cut -d"," -f1 | awk '{s+=$1} END {print s}'`
avg1=`bc <<< "scale = 2; ($sum / $count)"`
sum=`cat run_time.csv | cut -d"," -f2 | awk '{s+=$1} END {print s}'`
avg2=`bc <<< "scale = 2; ($sum / $count)"` 
sum=`cat run_time.csv | cut -d"," -f3 | awk '{s+=$1} END {print s}'`
avg3=`bc <<< "scale = 2; ($sum / $count)"`

echo $avg1,$avg2,$avg3>>run_time.avg
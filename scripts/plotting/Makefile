
run:
	mkdir -p ../../plots
	(cd throughput; ./extract.sh ../../../results/Toggle_Throughput-Fig5/raw/toggles.out; \
	 	gnuplot toggle-throughput-new.gpl > throughput.pdf; mv throughput.pdf ../../../plots/)

clean:
	rm -rf throughput/freq_* throughput/*.pdf

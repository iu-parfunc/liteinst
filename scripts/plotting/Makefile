
run:
	mkdir -p ../../plots
	(cd throughput; ./extract.sh ../../../results/Toggle_Throughput-Fig5/raw/toggles.out; \
	 	gnuplot toggle-throughput-new.gpl > throughput.pdf; mv throughput.pdf ../../../plots/)
	(cd injection; cp ../../../results/Injection_Costs-Fig4/injection_costs_fig4.csv .; \
		gnuplot plot_injection.gpl > injection.jpg; mv injection.jpg ../../../plots)

clean:
	rm -rf throughput/freq_* throughput/*.pdf
	rm -rf injection/*.csv injection/*.pdf

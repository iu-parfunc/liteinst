all:
		$(CC) -o looper looper.c
			sudo time dtrace -n 'pid$target:a.out:func:entry { @[copyinstr(arg0)] = count(); }' -c ./looper

run: all

clean:
		rm -f looper


# TODO: Rewrite completely when more done 

all: test

# just an example 
test: test.c patcher.c 
	gcc -std=gnu99 -pedantic -Wall -o test patcher.c test.c ../../../distorm/distorm3.a


run: test 
	./test 

clean:  test 
	rm test

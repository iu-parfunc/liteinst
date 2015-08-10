.PHONY: clean
# TODO: Rewrite completely when more done 

all: test_patch_write test_patch_code1 test_patch_code2


# TODO: Make this more efficient (all tests in a subdir... ) 
test_patch_write: test_patch_write.c patcher.c 
	gcc -std=gnu99 -pedantic -Wall -o test_patch_write patcher.c test_patch_write.c ../../../distorm/distorm3.a

test_patch_code1: test_patch_code1.c patcher.c 
	gcc -std=gnu99 -pedantic -Wall -o test_patch_code1 patcher.c test_patch_code1.c ../../../distorm/distorm3.a

test_patch_code2: test_patch_code2.c patcher.c 
	gcc -std=gnu99 -pedantic -Wall -o test_patch_code2 patcher.c test_patch_code2.c ../../../distorm/distorm3.a



run: test 
	./test 

clean:  
	rm test_patch_write
	rm test_patch_code1
	rm test_patch_code2

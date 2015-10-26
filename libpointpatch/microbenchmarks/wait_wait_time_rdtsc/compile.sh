USE_CC=$1
USE_CXX=$2

compileLib() { 
    echo "Compiling Library" 
    
    make cleanLib
    case "$1" in 
	nonatomic) 
	    make installLib CC=$USE_CC CXX=$USE_CXX CFLAGS=-DNONATOMIC_WRITE
	    ;; 
	*) 
	    make installLib CC=$USE_CC CXX=$USE_CXX 
	    ;;
    esac 

} 


compileTests() {
    echo "Compiling Tests" 
    
    make cleanTests
    make buildTests CC=$USE_CC CXX=$USE_CXX
    
}

# One time compile before running test workload
compileLib $3; 
compileTests;

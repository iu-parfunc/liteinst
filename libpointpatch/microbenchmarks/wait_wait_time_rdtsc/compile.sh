USE_CC=$1
USE_CXX=$2

compileLib() { 
    echo "Compiling Library" 
    
    make cleanLib
    make installLib CC=$USE_CC CXX=$USE_CXX 
} 


compileTests() {
    echo "Compiling Tests" 
    
    make cleanTests
    make buildTests CC=$USE_CC CXX=$USE_CXX
    
}

# One time compile before running test workload
compileLib; 
compileTests;

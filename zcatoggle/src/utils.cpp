#include "utils.hpp"

int x_utils;

void empty_function() {

  __notify_intrinsic((void*)"empty_function:start", (void *)&x_utils);

  __notify_intrinsic((void*)"empty_function:end", (void *)&x_utils);

}

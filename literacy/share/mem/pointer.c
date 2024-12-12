#include "memory.h"

// raco nanomon snap -h --show --split --fx-radix 10 --pad-limit 16

/*************************************************************************************************/
int main(int argc, char** argv) {
    define_init(int, var, 20241206);
    int *ptr = &var;

    register_variable("ptr", "uintptr_t", &ptr, "stack");
    take_snapshot("done");

    return 0;
}

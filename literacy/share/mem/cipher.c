#include <stdio.h>
#include <string.h>

#include "memory.h"

// argv: China

/*************************************************************************************************/
// raco nanomon snap -h --show --split --fx-radix 10 --pad-limit 16 --entry main_scanf
__ffi__ int main_scanf(int argc, char** argv) {
    define(char, c1);
    define(char, c2);
    define(char, c3);
    define(char, c4);
    define(char, c5);

    take_snapshot("variables defined");

    if (argc > 1) {
        sscanf(argv[1], "%c%c%c%c%c", &c1, &c2, &c3, &c4, &c5);
        take_snapshot("initialized");

        c1 += 4;
        c2 += 4;
        c3 += 4;
        c4 += 4;
        c5 += 4;

        take_snapshot("encrypted (done)");
    } else {
        take_snapshot("missing arguments");
    }

    return 0;
}

// raco nanomon snap -h --show --split --fx-radix 10 --pad-limit 16 --entry main_array
__ffi__ int main_array(int argc, char** argv) {
    define_array(char, c, 5);
    
    take_snapshot("array defined");
    
    if (argc > 1) {
        sscanf(argv[1], "%5c", c);
        take_snapshot("initialized");

        for (size_t idx = 0; idx < 5; idx ++) {
            c[idx] += 4;
        }
        take_snapshot("encrypted (done)");
    } else {
        take_snapshot("missing arguments");
    }
    
    return 0;
}

// raco nanomon snap -h --show --split --fx-radix 10 --pad-limit 16 --ahead 1 --no-state
__ffi__ int main(int argc, char** argv) {
    define_array(char, cipher, 5);
    define_pointer(char, plain, cipher);
    
    if (argc > 1) {
        sscanf(argv[1], "%5c", cipher);
        take_snapshot("initialized");

        for (size_t idx = 0; idx < 5; idx ++) {
            plain[0] = 0[plain] + 4;
            plain ++;

            if (idx < 4) {
                take_snapshot("zipping");
            } else {
                take_snapshot("done");
            }
        }
    } else {
        take_snapshot("missing arguments");
    }

    return 0;
}

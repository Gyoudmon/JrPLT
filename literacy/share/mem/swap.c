#include "memory.h"

// raco nanomon snap -h --show --split --fx-radix 10 --pad-limit 16 --behind 3 --ahead 3

/*************************************************************************************************/
/** NOTICE
 * If worked with `char`,
 * the memory might not have been filled with random values
 * since the dirty value might only occupy at the lowest byte
 * and leaving the higher 3 bytes to 0s.
 */
__ffi__ int main(int argc, char** argv) {
    define(short, a);
    define(short, b);
    define(short, c);

    take_snapshot("defined");

    a = 32;
    b = 64;
    take_snapshot("initialized");

    c = a;
    take_snapshot("a -> c");
    a = b;
    take_snapshot("b -> a");
    b = c;
    take_snapshot("c -> b (done)");

    return 0;
}

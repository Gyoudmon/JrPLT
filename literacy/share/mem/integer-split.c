#include "memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

/*************************************************************************************************/
int main(int argc, char** argv) {
    define_init(uint8_t, n, 123);
    define(uint8_t, r);

    take_snapshot("initialized");

    while (n > 0) {
        r = n % 10;
        n = n / 10;

        if (n > 0) {
            take_snapshot("splitting");
        } else {
            take_snapshot("splitting(done)");
        }
    }

    return 0;
}

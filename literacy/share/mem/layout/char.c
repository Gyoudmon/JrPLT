#include "../memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

/*************************************************************************************************/
int main(int argc, char** argv) {
    define(char, a);
    define(char, b);
    define_init(char, num, 0);

    take_snapshot("char");

    return 0;
}

#include "memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

/*************************************************************************************************/
int main(int argc, char** argv) {
    define_init(uint8_t, x, 24);
    define(uint8_t, a);
    define(uint8_t, b);
    define(uint8_t, c);

    take_snapshot("initialized");

    // 倒推
    a = b = c = x / 3;

    take_snapshot("均分");

    a = a / 2;
    b = b / 2;
    c = a + b + c;

    take_snapshot("丙给甲、乙");

    a = a / 2;
    c = c / 2;
    b = a + b + c;

    take_snapshot("乙给甲、丙");

    b = b / 2;
    c = c / 2;
    a = a + b + c;

    take_snapshot("甲给乙、丙");

    return 0;
}

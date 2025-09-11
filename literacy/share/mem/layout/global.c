#include "../memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

short a = 0;
short b = 0;
short num = 0;

/*************************************************************************************************/
int main(int argc, char** argv) {
    watch_data(short, a);
    watch_data(short, b);
    watch_data(short, num);

    take_snapshot("global");

    return 0;
}

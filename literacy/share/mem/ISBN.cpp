// raco nanomon snap -h --show --split --no-pad --fx-radix 10

#include <iostream>
#include <string>

#include "memory.h"

int main(int argc, char* argv[]) {
    define_init(int, sum, 0);
    define_array(char, ISBN, 10);
    define_init(char, checksum, 'X');
    define_pointer(char, publisher, ISBN + 1);
    define_pointer(char, book, ISBN + 4);
    define_pointer(char, id, ISBN + 9);
    
    take_snapshot("defined");

    if (argc > 1) {
        sscanf(argv[1], "%c-%3c-%5c-%c", ISBN, publisher, book, id);
    } else {
        sscanf("0-670-82162-X", "%c-%3c-%5c-%c", ISBN, publisher, book, id);
    }

    take_snapshot("initialized");

    for (int i = 0; i < 9; i ++) {
        sum += int(ISBN[i] - '0') * (i + 1);
    }

    take_snapshot("sum");

    sum %= 11;
    if (sum < 10) {
        checksum = char(sum + '0');
    }

    if (id[0] == checksum) {
        printf("Right\n");
    } else {
        id[0] = checksum;
        printf("%c-%.3s-%.5s-%c\n", ISBN[0], publisher, book, id[0]);
    }

    take_snapshot("done");

    return 0;
}

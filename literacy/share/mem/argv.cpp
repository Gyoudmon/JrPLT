// raco nanomon snap -h --show --split --no-pad --fx-radix 10

#include <iostream>
#include <string>

#include "memory.h"

int main(int argc, char* argv[]) {
    char name[16];

    take_snapshot("enter");

    for (int i = 0; i < argc; i ++) {
        snprintf(name, 16, "argv[%d]", i);
        register_array(name, "char", argv[i], "stack", strlen(argv[i]));
        take_snapshot(name);

        printf("argv[%d]: %s\n", i, argv[i]);
    }

    take_snapshot("done");

    return 0;
}

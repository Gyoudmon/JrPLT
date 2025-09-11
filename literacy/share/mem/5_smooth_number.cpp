#include <iostream>

#include "memory.h"

// raco nanomon snap -h --no-pad --show --split --fx-radix 10

int main(int argc, char* argv[]) {
    unsigned int N = 35;
    if (argc > 1) {
        sscanf(argv[1], "%u", &N);
    }

    if (N > 35) N = 35;

    define_array(int, uglies, N);
    define_init(int, ptr, 0);
    int factors[] = { 2, 3, 5 };
    int fptrs[sizeof(factors) / sizeof(int)] = { 0 };
    int nexts[sizeof(factors) / sizeof(int)];
    
    uglies[0] = 1;

    watch_array(int, fptrs);
    watch_array(int, nexts);

    take_snapshot("initialized");

    while (true) {
        ptr ++;
        uglies[ptr] = N + 1;

        for (int pi = 0; pi < sizeof(factors) / sizeof(int); pi ++) {
            nexts[pi] = uglies[fptrs[pi]] * factors[pi];

            if (uglies[ptr] > nexts[pi]) {
                uglies[ptr] = nexts[pi];
            }
        }

        if (ptr >= N) break;

        for (int pi = 0; pi < sizeof(factors) / sizeof(int); pi ++) {
            if (uglies[ptr] == nexts[pi]) {
                fptrs[pi] ++;
            }
        }

        take_snapshot("next");
    }

    take_snapshot("done");

    return 0;
}

#include "memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

int main(int argc, char** argv) {
    define(int, idx);
    define(int, L_neighbor);
    define(int, R_neighbor);
    int sugars[] = { 8, 9, 10, 11, 12 };
    
    watch_array(int, sugars);
    take_snapshot("Initialized");

    idx = 0;
    while (idx < 5) {
        L_neighbor = (idx - 1 + 5) % 5;
        R_neighbor = (idx + 1) % 5;

        sugars[idx] /= 3;

        sugars[L_neighbor] += sugars[idx];
        sugars[R_neighbor] += sugars[idx];

        take_snapshot("Loop");
        idx ++;
    }

    take_snapshot("Done");

    return 0;
}

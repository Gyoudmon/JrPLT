#include "memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

int main(int argc, char** argv) {
    define(int, idx);
    define(int, L_neighbor);
    define(int, R_neighbor);
    int S[] = { 8, 9, 10, 11, 12 };
    
    watch_array(int, S);
    take_snapshot("Initialized");

    idx = 0;
    while (idx < 5) {
        L_neighbor = (idx - 1 + 5) % 5;
        R_neighbor = (idx + 1) % 5;

        S[idx] /= 3;

        S[L_neighbor] += S[idx];
        S[R_neighbor] += S[idx];

        take_snapshot("Loop");
        idx ++;
    }

    take_snapshot("Done");

    return 0;
}

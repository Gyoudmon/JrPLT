#include "memory.h"

#include <stdio.h>

// raco nanomon snap -h --show --split --fx-radix 10

int main(int argc, char* argv[]) {
    const int N = 20;
    define_array(int, nums, N + 1);
    define_init(int, idx, 1);
    define_init(int, sifted, 1);
    
    nums[0] = nums[1] = 0;
    for (int i = 2; i <= N; i ++) {
        nums[i] = 1;
    }

    take_snapshot("initialized!");

    idx ++;
    while (idx <= N) {
        int multiple = idx * 2;

        while (multiple <= N) {
            if (nums[multiple]) {
                nums[multiple] = 0;
                sifted ++;
            }

            multiple += idx;
        }

        take_snapshot("next");

        while (nums[++ idx] == 0) {
            if (idx == N) {
                break;
            }
        }
    }

    printf("%d\n", N - sifted);

    take_snapshot("done");

    return 0;
}

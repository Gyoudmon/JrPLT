#include "../memory.h"

// raco nanomon snap -h --show --split --fx-radix 10

/*************************************************************************************************/
int main(int argc, char** argv) {
    define_init(int, a, 1);
    define_init(int, b, 10);
    define_init(int, num, 0);

    for(short i=a;i<=b;i++){
        if(i%2==1){
            num+=i;
        }
    }

    take_snapshot("int");

    return 0;
}

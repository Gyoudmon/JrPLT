#include "village/CAE/JrPLTCR.hpp"

using namespace WarGrey::CAE;

/*************************************************************************************************/
int main(int argc, char* args[]) {
    JrPLTCRCosmos universe;

    universe.construct(argc, args);
    universe.big_bang();

    return 0;
}

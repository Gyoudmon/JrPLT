#include "stem.hpp"

using namespace GYDM;
using namespace WarGrey::STEM;

/*************************************************************************************************/
WarGrey::STEM::TheSCSMPlane::TheSCSMPlane(const char* name, uint32_t title_color) : TheBigBang(name, title_color) {
    // Επιστήμη(Science) in greek
    this->the_name("[Σ]交互式科学");
}

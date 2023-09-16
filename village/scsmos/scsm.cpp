#include "scsm.hpp"

using namespace WarGrey::STEM;
using namespace WarGrey::SCSM;

/*************************************************************************************************/
WarGrey::SCSM::TheSCSMPlane::TheSCSMPlane(const char* name, uint32_t title_color) : TheBigBang(name, title_color) {
    // Επιστήμη(Science) in greek
    this->the_name("[Σ]交互式科学");
}

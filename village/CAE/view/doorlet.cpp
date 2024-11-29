#include "doorlet.hpp"

using namespace WarGrey::CAE;
using namespace Plteen;

/*************************************************************************************************/
#define DOOR_PATH digimon_path("CAE/door", "")

/*************************************************************************************************/
WarGrey::CAE::DoorSprite::DoorSprite(uint64_t seq, const char* nickname)
    : Sprite(DOOR_PATH), seq(seq), _name(nickname) {
        this->_name += "[" + std::to_string(this->seq) + "]";
}

void WarGrey::CAE::DoorSprite::open() {
    this->play("door", 1);
}

void WarGrey::CAE::DoorSprite::close() {
    this->stop();
    this->switch_to_costume("door");
}

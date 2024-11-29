#include "class.hpp"

#include <strings.h>

using namespace WarGrey::CAE;
using namespace Plteen;

/*************************************************************************************************/
static const char* class_room_type_to_name(ClassRoomType type) {
    switch (type) {
    case ClassRoomType::ComputerColumn: return "Computer Room/C";
    case ClassRoomType::STEM: return "STEM";
    default: return "Blank";
    }
}

static ClassRoomType name_to_class_room_type(const char* name) {
    if (strcasecmp(name, "Computer Room/C") == 0) {
        return ClassRoomType::ComputerColumn;
    } else if (strcasecmp(name, "STEM") == 0) {
        return ClassRoomType::STEM;
    } else {
        return ClassRoomType::Blank;
    }
}

/*************************************************************************************************/
bool WarGrey::CAE::ClassEntity::match(const std::string& line, int* offset) {
    return GMSEntity::match(line, class_mark, offset);
}

const char* WarGrey::CAE::ClassEntity::prompt() {
    return "{ seq:nat, name:str, room_type:str }";
}

const char* WarGrey::CAE::ClassEntity::update_prompt() {
    return "{ name:str, room_type:str }";
}

/*************************************************************************************************/
WarGrey::CAE::ClassEntity::ClassEntity(const std::string& s, int idx) {
    size_t pos = size_t(idx);
    size_t end = s.size();
    const char* src = s.c_str();

    scan_skip_space(src, &pos, end);
    this->seq = scan_natural(src, &pos, end);
    if (this->seq == 0U) throw exn_gms("Invalid Class No.");
    
    scan_skip_delimiter(src, &pos, end, field_delimiter);
    this->nickname = scan_string(src, &pos, end, field_delimiter);

    scan_skip_delimiter(src, &pos, end, field_delimiter);
    this->cr_type = name_to_class_room_type(scan_string(src, &pos, end, field_delimiter).c_str());
}

std::string WarGrey::CAE::ClassEntity::to_string() {
    return make_nstring("%c:%llu,%s,%s", class_mark, this->seq,
        this->get_nickname(), class_room_type_to_name(this->cr_type));
}

#pragma once

#include <plteen/datum/string.hpp>

namespace WarGrey::CAE {
    enum class ClassRoomType { ComputerRoom, STEM, Blank };
    enum class ClassRoomLayoutType { Column, Hexagon, None };

    class ClassRoomConfig {
    public:
        ClassRoomConfig(const char* s, int idx = 0) : ClassRoomConfig(std::string(s), idx) {}
        ClassRoomConfig(const std::string& s, int idx);
        virtual ~ClassRoomConfig() {}

    public:
        ClassRoomType type() { return this->cr_type; }
        ClassRoomLayoutType layout_type() { return this->layout; }

    public:
        virtual const char* prompt() = 0;
        virtual std::string to_string() = 0;

    private:
        ClassRoomType cr_type;
        ClassRoomLayoutType layout;
    };
}

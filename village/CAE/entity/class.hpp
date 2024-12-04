#pragma once

#include "entity.hpp"

namespace WarGrey::CAE {
    enum class ClassRoomType { ComputerColumn, STEM, Blank };

    class ClassEntity : public WarGrey::CAE::CAEEntity {
    public:
        static bool match(const std::string& line, int* offset);
        static const char* prompt();
        static const char* update_prompt();

    public:
        ClassEntity(const char* s, int idx = 0) : ClassEntity(std::string(s), idx) {}
        ClassEntity(const std::string& s, int idx);
        virtual ~ClassEntity() {}

    public:
        const char* get_nickname() { return this->nickname.c_str(); }
        ClassRoomType class_room_type() { return this->cr_type; }

    public:
        uint64_t primary_key() override { return this->seq; }
        std::string to_string() override;

    private:
        uint64_t seq;
        std::string nickname;
        ClassRoomType cr_type;
    };

    typedef std::shared_ptr<ClassEntity> shared_class_t;
}

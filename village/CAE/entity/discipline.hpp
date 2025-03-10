#pragma once

#include "entity.hpp"

namespace WarGrey::CAE {
    enum class DisciplineType {
        Mathematics, Programming,
        Physics, Chemistry, Biology, STEM,
        Printing3D,
        Blank
    };

    class DisciplineEntity : public WarGrey::CAE::CAEEntity {
    public:
        static bool match(const std::string& line, int* offset);
        static const char* prompt();

    public:
        static const char* type_to_name(DisciplineType type);
        static DisciplineType name_to_type(const char* name);

    public:
        DisciplineEntity(const char* s, int idx = 0) : DisciplineEntity(std::string(s), idx) {}
        DisciplineEntity(const std::string& s, int idx);
        virtual ~DisciplineEntity() {}

    public:
        const char* cannonical_name();
        DisciplineType cannonical_type() { return this->type; }

    public:
        uint64_t primary_key() override { return this->code; }
        std::string to_string() override;

    private:
        uint64_t code;
        DisciplineType type;
    };

    typedef std::shared_ptr<DisciplineEntity> shared_discipline_t;
}

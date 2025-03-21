#pragma once

#include <cstdint>
#include <exception>

#include <plteen/datum/string.hpp>

/*************************************************************************************************/
namespace WarGrey::CAE {
    static const char field_delimiter = ',';
    
    static const char class_mark = 'c';
    static const char discipline_mark = 'd';
    static const char student_mark = 's';

    /*********************************************************************************************/
    class CAEEntity {
    protected: // for subclasses to check the type markers of records
        static bool match(const std::string& line, char mark, int* offset);
        static bool match(const std::string& line, char mark1, char mark2, int* offset);
        static bool match(const std::string& line, char mark1, char mark2, char mark3, int* offset);
        
    public:
        virtual ~CAEEntity() {}

    public:
        virtual uint64_t primary_key() = 0;
        virtual std::string to_string() = 0;
    };

    class exn_cae : public std::exception {
    public:
        exn_cae(const char* msg, ...);

    public:
        const char* what() const noexcept override { return this->message.c_str(); }

    private:
        std::string message;
    };
}

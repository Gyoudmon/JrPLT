#pragma once

#include <plteen/game.hpp>

namespace WarGrey::CAE {
    class DoorSprite : public Plteen::Sprite {
    public:
        DoorSprite(uint64_t seq, const char* nickname);
        virtual ~DoorSprite() {}

        const char* name() override { return this->_name.c_str(); }
        int preferred_local_fps() override { return 24; }

    public:
        uint64_t primary_key() { return this->seq; }

    public:
        void open();
        void close();

    private:
        uint64_t seq;
        std::string _name;
    };
}

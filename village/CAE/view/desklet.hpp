#pragma once

#include <plteen/game.hpp>

namespace WarGrey::CAE {
    class IDesk {
    public:
        IDesk(size_t idx) : idx(idx) {}
        virtual ~IDesk() {}

    public:
        virtual size_t seat_count() = 0;
        virtual int get_seat_by(float local_x, float local_y) = 0;
        virtual void sit(Plteen::ISprite* stu, int idx, double duration = 0.0) = 0;

    public:
        size_t get_index() { return this->idx; }

    private:
        size_t idx;
    };
}

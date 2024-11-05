#pragma once

#include <plteen/game.hpp>

#include "../desklet.hpp"

namespace WarGrey::CAE {
    class ComputerDesklet : public Plteen::Rectanglet, public WarGrey::CAE::IDesk {
    public:
        ComputerDesklet(size_t idx, float width, float height, size_t count, size_t idx0
            , const Plteen::RGBA& color, const Plteen::RGBA& border_color = WHITESMOKE)
            : Plteen::Rectanglet(width, height, color, border_color), IDesk(idx)
            , count(count), idx0(idx0) {}
        virtual ~ComputerDesklet() noexcept {}

    public:
		void draw(Plteen::dc_t* dc, float x, float y, float Width, float Height) override;

    public:
        size_t seat_count() override { return this->count; }
        int get_seat_by(float local_x, float local_y) override;
        void sit(Plteen::ISprite* stu, int idx, double duration = 0.0) override;

    private:
        size_t count;
        size_t idx0;
    };
}

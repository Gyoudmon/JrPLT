#pragma once

#include <plteen/game.hpp>

#include "../desklet.hpp"

namespace WarGrey::CAE {
    class HexagonalDesklet : public Plteen::RegularPolygonlet, public WarGrey::CAE::IDesk {
    public:
        HexagonalDesklet(size_t idx, float radius, const Plteen::RGBA& color, const Plteen::RGBA& border_color = Plteen::transparent)
            : Plteen::RegularPolygonlet(6, radius, color, border_color), IDesk(idx) {}
        virtual ~HexagonalDesklet() noexcept {}

    public:
        size_t seat_count() override { return 6; }
        int get_seat_by(float local_x, float local_y) override;
        void sit(Plteen::ISprite* stu, int idx, double duration = 0.0) override;
    };
}

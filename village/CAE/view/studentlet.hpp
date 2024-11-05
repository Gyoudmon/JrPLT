#pragma once

#include <plteen/game.hpp>

#include "../entity/student.hpp"

namespace WarGrey::CAE {
    class StudentSprite : public Plteen::TrailStudent {
    public:
        StudentSprite(uint64_t No, std::string nickname, uint8_t avatar)
            : TrailStudent(avatar, nickname.c_str()), No(No), name_texture(nullptr) {}

        virtual ~StudentSprite() {}

    public:
        void draw(Plteen::dc_t* renderer, float x, float y, float Width, float Height) override;

    public:
        uint64_t primary_key() { return this->No; }

    public:
        void set_score_percentage(double percentage, const Plteen::RGBA& color = 0x00FF00U);

    private:
        uint64_t No;

    private:
        Plteen::RGBA sbar_color = Plteen::transparent;
        double sbar_percentage = 0.0;

    private:
        Plteen::shared_texture_t name_texture;
        SDL_FRect name_region;
    };
}

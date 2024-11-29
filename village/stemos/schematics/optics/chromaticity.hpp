#pragma once // 确保只被 include 一次

#include <plteen/bang.hpp>

#include "../../stem.hpp"

#include <vector>

namespace WarGrey::STEM {
    class __lambda__ ChromaticityDiagramPlane : public WarGrey::STEM::TheSTEMPlane {
    public:
        ChromaticityDiagramPlane() : TheBigBang("色度图", 0xFFFFFFU) {}
        virtual ~ChromaticityDiagramPlane() {}
        
    public:
        void load(float width, float height) override;
        void update(uint64_t interval, uint32_t count, uint64_t uptime) override;
        void reflow(float width, float height) override;

    public:
        bool can_select(Plteen::IMatter* m) override;
        void after_select(Plteen::IMatter* m, bool yes) override;
        bool update_tooltip(Plteen::IMatter* m, float lx, float ly, float gx, float gy) override;

    private:
        void reflow_primaries(float x, float y);

    private:
        std::vector<Plteen::Circlet*> hues;
        std::vector<Plteen::Ellipselet*> primaries;
        Plteen::Chromalet* chroma_dia;

    private:
        size_t selection_seq = 0; 
    };
}

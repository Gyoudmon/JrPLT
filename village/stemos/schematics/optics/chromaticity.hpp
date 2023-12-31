#pragma once // 确保只被 include 一次

#include <gydm/bang.hpp>

#include "../../stem.hpp"

#include <vector>

namespace WarGrey::STEM {
    class __lambda__ ChromaticityDiagramPlane : public WarGrey::STEM::TheSTEMPlane {
    public:
        ChromaticityDiagramPlane() : TheSTEMPlane("色度图", 0xFFFFFFU) {}
        virtual ~ChromaticityDiagramPlane() {}
        
    public:
        void load(float width, float height) override;
        void update(uint64_t interval, uint32_t count, uint64_t uptime) override;
        void reflow(float width, float height) override;

    public:
        bool can_select(GYDM::IMatter* m) override;
        void after_select(GYDM::IMatter* m, bool yes) override;
        bool update_tooltip(GYDM::IMatter* m, float lx, float ly, float gx, float gy) override;

    private:
        void reflow_primaries(float x, float y);

    private:
        std::vector<GYDM::Circlet*> hues;
        std::vector<GYDM::Ellipselet*> primaries;
        GYDM::Chromalet* chroma_dia;

    private:
        size_t selection_seq = 0; 
    };
}

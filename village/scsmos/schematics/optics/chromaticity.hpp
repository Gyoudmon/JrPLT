#pragma once // 确保只被 include 一次

#include <gydm_stem/bang.hpp>

#include "../../scsm.hpp"

#include <vector>

namespace WarGrey::SCSM {
    class __lambda__ ChromaticityDiagramPlane : public WarGrey::SCSM::TheSCSMPlane {
    public:
        ChromaticityDiagramPlane() : TheSCSMPlane("色度图", 0xFFFFFFU) {}
        virtual ~ChromaticityDiagramPlane() {}
        
    public:
        void load(float width, float height) override;
        void update(uint64_t interval, uint32_t count, uint64_t uptime) override;
        void reflow(float width, float height) override;

    public:
        bool can_select(WarGrey::STEM::IMatter* m) override;
        void after_select(WarGrey::STEM::IMatter* m, bool yes) override;
        bool update_tooltip(WarGrey::STEM::IMatter* m, float lx, float ly, float gx, float gy) override;

    private:
        void reflow_primaries(float x, float y);

    private:
        std::vector<WarGrey::STEM::Circlet*> hues;
        std::vector<WarGrey::STEM::Ellipselet*> primaries;
        WarGrey::STEM::Chromalet* chroma_dia;

    private:
        size_t selection_seq = 0; 
    };
}

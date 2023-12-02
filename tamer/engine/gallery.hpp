#pragma once

#include "../../digitama/gydm_stem/bang.hpp"

/*************************************************************************************************/
namespace WarGrey::STEM {
    class GalleryPlane : public WarGrey::STEM::TheBigBang {
        public:
            GalleryPlane() : TheBigBang("Gallery") { this->the_name("Tamer"); }

        public:
            void load(float width, float height) override;
            void reflow(float width, float height) override;

        public:
            bool can_select(WarGrey::STEM::IMatter* m) override {
                return true;
            }

        private:
            void load_for_raft(float width, float height);
            void reflow_for_raft(float width, float height);

        private: // objects for composing a raft
            WarGrey::STEM::Labellet* caption;
            WarGrey::STEM::IShapelet* raft;
            WarGrey::STEM::IShapelet* bow;
            WarGrey::STEM::IShapelet* stern;
            WarGrey::STEM::IShapelet* flag;
            WarGrey::STEM::IShapelet* mast;
            WarGrey::STEM::IShapelet* post;
            WarGrey::STEM::IShapelet* paddle;
            WarGrey::STEM::IShapelet* sea;
    };
}

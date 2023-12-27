#pragma once

#include "../../digitama/gydm/bang.hpp"

/*************************************************************************************************/
namespace GYDM {
    class GalleryPlane : public GYDM::TheBigBang {
        public:
            GalleryPlane() : TheBigBang("Gallery") { this->the_name("Tamer"); }

        public:
            void load(float width, float height) override;
            void reflow(float width, float height) override;

        public:
            bool can_select(GYDM::IMatter* m) override {
                return true;
            }

        private:
            void load_for_house(float width, float height);
            void load_for_raft(float width, float height);

            void reflow_for_house(float width, float height);
            void reflow_for_raft(float width, float height);

        private: // objects for composing a raft
            GYDM::Labellet* caption;
            GYDM::IShapelet* raft;
            GYDM::IShapelet* bow;
            GYDM::IShapelet* stern;
            GYDM::IShapelet* flag;
            GYDM::IShapelet* mast;
            GYDM::IShapelet* post;
            GYDM::IShapelet* paddle;
            GYDM::IShapelet* sea;

        private: // objects for composing a house
            GYDM::IShapelet* roof;
            GYDM::IShapelet* wall;
            GYDM::IShapelet* door;
            GYDM::IShapelet* lock;
            GYDM::IShapelet* window;
            GYDM::IShapelet* garden;
    };
}

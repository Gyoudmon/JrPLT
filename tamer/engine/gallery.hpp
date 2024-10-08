#pragma once

#include "../../digitama/plteen/bang.hpp"

/*************************************************************************************************/
namespace Plteen {
    class GalleryPlane : public Plteen::TheBigBang {
        public:
            GalleryPlane() : TheBigBang("Gallery") { this->the_name("Tamer"); }

        public:
            void load(float width, float height) override;
            void reflow(float width, float height) override;

        public:
            bool can_select(Plteen::IMatter* m) override {
                return true;
            }

        private:
            void load_for_house(float width, float height);
            void load_for_raft(float width, float height);

            void reflow_for_house(float width, float height);
            void reflow_for_raft(float width, float height);

        private: // objects for composing a raft
            Plteen::Labellet* caption;
            Plteen::IShapelet* raft;
            Plteen::IShapelet* bow;
            Plteen::IShapelet* stern;
            Plteen::IShapelet* flag;
            Plteen::IShapelet* mast;
            Plteen::IShapelet* post;
            Plteen::IShapelet* paddle;
            Plteen::IShapelet* sea;

        private: // objects for composing a house
            Plteen::IShapelet* roof;
            Plteen::IShapelet* wall;
            Plteen::IShapelet* door;
            Plteen::IShapelet* lock;
            Plteen::IShapelet* window;
            Plteen::IShapelet* garden;
    };
}

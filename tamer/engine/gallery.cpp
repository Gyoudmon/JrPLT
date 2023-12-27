#include "gallery.hpp"

using namespace GYDM;

/*************************************************************************************************/
static const float raft_height = float(generic_font_size(FontSize::xx_large));
static const float raft_width = raft_height * 4.0F;

/*************************************************************************************************/
void GYDM::GalleryPlane::load(float width, float height) {
    TheBigBang::load(width, height);

    this->load_for_house(width, height);
    this->load_for_raft(width, height);
}

void GYDM::GalleryPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);

    this->reflow_for_house(width, height);
    this->reflow_for_raft(width, height);
}

/*************************************************************************************************/
void GYDM::GalleryPlane::load_for_house(float width, float height) {
    this->garden = this->insert(new Ellipselet(100, 40, PALEGREEN, KHAKI));               // 苍绿色院子
    
    this->roof = this->insert(new Trianglet(128.0F, -90.0F, DEEPSKYBLUE, ROYALBLUE));     // 深空蓝屋顶
    this->wall = this->insert(new Rectanglet(100, 90, WHITESMOKE, SNOW));                 // 白色墙壁
    this->door = this->insert(new Rectanglet(21, 42, KHAKI, DARKKHAKI));                  // 卡其色门
    this->lock = this->insert(new Circlet(3, CHOCOLATE));                                 // 巧克力色门锁
    this->window = this->insert(new RoundedSquarelet(32, -0.15F, LIGHTSKYBLUE, SKYBLUE)); // 天蓝色窗户
}

void GYDM::GalleryPlane::reflow_for_house(float width, float height) {
    this->move_to(this->roof, { width * 0.25F, height * 0.75F }, MatterAnchor::CB);
    this->move_to(this->wall, { this->roof, MatterAnchor::CB }, MatterAnchor::CT);
    this->move_to(this->door, { this->wall, MatterAnchor::LB }, MatterAnchor::LB, 12.0F);
    this->move_to(this->lock, { this->door, MatterAnchor::RC }, MatterAnchor::RC, -3.0F);
    this->move_to(this->window, { this->wall, MatterAnchor::CC }, MatterAnchor::LC);

    this->move_to(this->garden, { this->wall, MatterAnchor::CC }, MatterAnchor::CT);
}

/*************************************************************************************************/
void GYDM::GalleryPlane::load_for_raft(float width, float height) {
    this->sea = this->insert(new Ellipselet(raft_width * 1.618F, raft_height, DEEPSKYBLUE));

    this->mast = this->insert(new Rectanglet(4.0F, raft_width, BURLYWOOD, SADDLEBROWN));
    this->flag = this->insert(new Trianglet(raft_height * 0.618F, 0.0F, ROYALBLUE, DODGERBLUE));

    this->post = this->insert(new RoundedRectanglet(raft_height * 0.618F, raft_height * 2.0F, -0.45F, BURLYWOOD, BURLYWOOD));
    this->paddle = this->insert(new Linelet(raft_width * 0.618F, raft_height * 2.0F, BROWN));
    this->raft = this->insert(new RoundedRectanglet(raft_width, raft_height, -0.1F, BURLYWOOD, BURLYWOOD));
    this->bow = this->insert(new RegularPolygonlet(3, raft_height * 0.5F, 180.0F, KHAKI, BURLYWOOD));
    this->stern = this->insert(new RegularPolygonlet(3, raft_height * 0.5F, 0.0F, KHAKI, BURLYWOOD));
                
    /* load renderer's name as the caption */ {
        IScreen* screen = this->master();

        if (screen != nullptr) {
            IUniverse* master = dynamic_cast<IUniverse*>(screen->display());

            if (master != nullptr) {
                this->caption = this->insert(new Labellet(GameFont::Default(), BLACK, master->get_renderer_name()));
            } else {
                this->caption = this->insert(new Labellet(GameFont::Default(), BLACK, "[Unknown]"));
            }
        }
    }
}

void GYDM::GalleryPlane::reflow_for_raft(float width, float height) {
    this->move_to(this->sea, { width * 0.75F, height * 0.80F }, MatterAnchor::CT);
                
    this->move_to(this->raft, { this->sea, MatterAnchor::CT }, MatterAnchor::CC);
    this->move_to(this->caption, { this->raft, MatterAnchor::CC }, MatterAnchor::CC);
    this->move_to(this->bow, { this->raft, MatterAnchor::LC }, MatterAnchor::RC);
    this->move_to(this->stern, { this->raft, MatterAnchor::RC }, MatterAnchor::LC);
    this->move_to(this->post, { this->raft, MatterAnchor::RB }, MatterAnchor::RB, -raft_height);
    this->move_to(this->paddle, { this->post, MatterAnchor::CC }, MatterAnchor::CC, raft_height);

    this->move_to(this->mast, { this->raft, MatterAnchor::LB }, MatterAnchor::LB, raft_height);
    this->move_to(this->flag, { this->mast, MatterAnchor::RT }, MatterAnchor::LT, 0.0F, raft_height * 0.25F);
}

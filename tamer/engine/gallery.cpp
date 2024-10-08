#include "gallery.hpp"

using namespace Plteen;

/*************************************************************************************************/
static const float raft_height = float(generic_font_size(FontSize::xx_large));
static const float raft_width = raft_height * 4.0F;

/*************************************************************************************************/
void Plteen::GalleryPlane::load(float width, float height) {
    TheBigBang::load(width, height);

    this->load_for_house(width, height);
    this->load_for_raft(width, height);
}

void Plteen::GalleryPlane::reflow(float width, float height) {
    TheBigBang::reflow(width, height);

    this->reflow_for_house(width, height);
    this->reflow_for_raft(width, height);
}

/*************************************************************************************************/
void Plteen::GalleryPlane::load_for_house(float width, float height) {
    this->garden = this->insert(new Ellipselet(100, 40, PALEGREEN, KHAKI));               // 苍绿色院子
    
    this->roof = this->insert(new Trianglet(128.0F, -90.0F, DEEPSKYBLUE, ROYALBLUE));     // 深空蓝屋顶
    this->wall = this->insert(new Rectanglet(100, 90, WHITESMOKE, SNOW));                 // 白色墙壁
    this->door = this->insert(new Rectanglet(21, 42, KHAKI, DARKKHAKI));                  // 卡其色门
    this->lock = this->insert(new Circlet(3, CHOCOLATE));                                 // 巧克力色门锁
    this->window = this->insert(new RoundedSquarelet(32, -0.15F, LIGHTSKYBLUE, SKYBLUE)); // 天蓝色窗户
}

void Plteen::GalleryPlane::reflow_for_house(float width, float height) {
    this->move_to(this->roof, { width * 0.25F, height * 0.75F }, MatterPort::CB);
    this->move_to(this->wall, { this->roof, MatterPort::CB }, MatterPort::CT);
    this->move_to(this->door, { this->wall, MatterPort::LB }, MatterPort::LB, { 12.0F, 0.0F });
    this->move_to(this->lock, { this->door, MatterPort::RC }, MatterPort::RC, { -3.0F, 0.0F });
    this->move_to(this->window, { this->wall, MatterPort::CC }, MatterPort::LC);

    this->move_to(this->garden, { this->wall, MatterPort::CC }, MatterPort::CT);
}

/*************************************************************************************************/
void Plteen::GalleryPlane::load_for_raft(float width, float height) {
    this->sea = this->insert(new Ellipselet(raft_width * 1.618F, raft_height, DEEPSKYBLUE));

    this->mast = this->insert(new Rectanglet(4.0F, raft_width, BURLYWOOD, SADDLEBROWN));
    this->flag = this->insert(new Trianglet(raft_height * 0.618F, 0.0F, ROYALBLUE, DODGERBLUE));

    this->post = this->insert(new RoundedRectanglet(raft_height * 0.618F, raft_height * 2.0F, -0.45F, BURLYWOOD, BURLYWOOD));
    this->paddle = this->insert(new Linelet(raft_width * 0.618F, raft_height * 2.0F, BROWN));
    this->raft = this->insert(new RoundedRectanglet(raft_width, raft_height, -0.1F, BURLYWOOD, BURLYWOOD));
    this->bow = this->insert(new RegularPolygonlet(3, raft_height * 0.5F, 180.0F, KHAKI, BURLYWOOD));
    this->stern = this->insert(new RegularPolygonlet(3, raft_height * 0.5F, 0.0F, KHAKI, BURLYWOOD));
                
    /* load renderer's name as the caption */ {
        dc_t* device = this->drawing_context();

        if (device != nullptr) {
            this->caption = this->insert(new Labellet(GameFont::Default(), BLACK, device->name()));
        } else {
            this->caption = this->insert(new Labellet(GameFont::Default(), BLACK, "[Unknown]"));
        }
    }
}

void Plteen::GalleryPlane::reflow_for_raft(float width, float height) {
    this->move_to(this->sea, { width * 0.75F, height * 0.80F }, MatterPort::CT);
                
    this->move_to(this->raft, { this->sea, MatterPort::CT }, MatterPort::CC);
    this->move_to(this->caption, { this->raft, MatterPort::CC }, MatterPort::CC);
    this->move_to(this->bow, { this->raft, MatterPort::LC }, MatterPort::RC);
    this->move_to(this->stern, { this->raft, MatterPort::RC }, MatterPort::LC);
    this->move_to(this->post, { this->raft, MatterPort::RB }, MatterPort::RB, { -raft_height, 0.0F });
    this->move_to(this->paddle, { this->post, MatterPort::CC }, MatterPort::CC, { raft_height, 0.0F });

    this->move_to(this->mast, { this->raft, MatterPort::LB }, MatterPort::LB, { raft_height, 0.0F });
    this->move_to(this->flag, { this->mast, MatterPort::RT }, MatterPort::LT, { 0.0F, raft_height * 0.25F });
}

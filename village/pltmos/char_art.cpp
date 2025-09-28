#include "char_art.hpp"

#include <plteen/bang.hpp>

#include "console/ascii/rectangle.hpp"
#include "console/ascii/triangle.hpp"
#include "console/ascii/hollow_rhumbus.hpp"

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
static const double CMDLET_DURATION = 1.0;
static const double CURSOR_DURATION = 0.2;
static const double MODULE_DURATION = 2.0;
static const double CHAR_DURATION = 0.2;
static const double LASER_DURATION = 0.5;
static const float GRID_CELL_WIDTH = 32.0F;
static const float GRID_CELL_HEIGHT = 64.0F;
static const float LASER_THICKNESS = 4.0F;
static const int LASER_COL0 = -2;
static const RGBA true_color = RGBA(ROYALBLUE, 0.81);

static const char VIS_KEY = 'v';
static const char SQR_KEY = 'q';
static const char RIT_KEY = 'w';
static const char TRI_KEY = 'e';
static const char RHM_KEY = 'r';

static const std::string cout_fmt("std::cout << \"%s\";");
static const std::string ccnt_fmt("%s在%s%s。");
static const std::string prnt_fmt("printf(\"(%%d, %%d)\", %s, %s);");
static const std::string pcnt_fmt("(%d, %d)");

static const cmdlet_item_t text_config[] = {
    { VIS_KEY, "算法可视化" },
    { SQR_KEY, "实心矩形" },
    { RIT_KEY, "实心直角三角形" },
    { TRI_KEY, "实心等腰三角形" },
    { RHM_KEY, "空心菱形" }
};


/*************************************************************************************************/
WarGrey::PLT::ASCIIArtPlane::ASCIIArtPlane(int r, int c)
        : TheBigBang("字符画中的数学模型", GHOSTWHITE), CmdletPlane(GameFont::monospace(FontSize::large), text_config)
        , term_row(r), term_col(c), term_ridx(0), term_cidx(0) {
    this->set_background(BLACK);
    this->set_grid_color(DIMGRAY);
    this->set_enabled_cmdlet_color(LIGHTSKYBLUE);
    this->set_processing_cmdlet_color(PALEGREEN);

    if ((r > 0) || (c > 0)) {
        this->chars = new Labellet**[this->term_row + 1];
        
        for (int r = 0; r <= this->term_row; r ++) {
            this->chars[r] = new Labellet*[this->term_col + 1];
        }
    }
}

WarGrey::PLT::ASCIIArtPlane::~ASCIIArtPlane() noexcept {
    if (this->chars != nullptr) {
        for (int r = 0; r <= this->term_row; r ++) {
            delete [] this->chars[r];
        }

        delete [] this->chars;
    }
}

void WarGrey::PLT::ASCIIArtPlane::load(float width, float height) {
    auto label_font = GameFont::monospace(FontSize::xx_large);
    auto seq_font = GameFont::Default(FontSize::medium);
    auto model_font = GameFont::math(FontSize::x_large);
    auto cell_seq_font = GameFont::Default(FontSize::x_small);

    CmdletPlane::load(width, height);
    
    for (int r = 0; r < this->term_row; r ++) {
        for (int c = 0; c < this->term_col; c ++) {
            this->chars[r][c] = this->insert(new Labellet(label_font, DIMGRAY));
        }
    }

    this->cursor = this->insert(new Labellet(label_font, ROYALBLUE));

    for (int r = 0; r < this->term_row; r ++) {
        this->chars[r][this->term_col] = this->insert(new Labellet(cell_seq_font, SILVER, "%d", r + 1));
    }

    for (int c = 0; c < this->term_col; c ++) {
        this->chars[this->term_row][c] = this->insert(new Labellet(cell_seq_font, SILVER, "%d", c + 1));
    }

    this->scanline = this->insert(new Rectanglet(LASER_THICKNESS, LASER_THICKNESS, LIGHTSKYBLUE));
    this->assistant = this->insert(new Tita());
    
    this->modinfo = this->insert(new Labellet(model_font, GHOSTWHITE));
    this->posinfo = this->insert(new Labellet(seq_font, GHOSTWHITE));
    this->spaninfo = this->insert(new Labellet(seq_font, GHOSTWHITE));
}

void WarGrey::PLT::ASCIIArtPlane::reflow(float width, float height) {
    CmdletPlane::reflow(width, height);

    this->create_centered_grid(this->term_row, this->term_col, GRID_CELL_WIDTH, GRID_CELL_HEIGHT);
    this->move_to_grid(this->assistant, 0, LASER_COL0 - 2, MatterPort::RC);
    this->move_to(this->modinfo, { width * 0.02F, height * 0.20F }, MatterPort::CC);
    this->move_to_grid(this->posinfo,  this->term_row + 0, 0, MatterPort::LC);
    this->move_to_grid(this->spaninfo, this->term_row + 1, 0, MatterPort::LT);

    for (int r = 0; r < this->term_row; r ++) {
        for (int c = 0; c < this->term_col; c ++) {
            this->move_to_grid(this->chars[r][c], r, c, MatterPort::CC);
        }
    }

    for (int r = 0; r < this->term_row; r ++) {
        this->move_to_grid(this->chars[r][this->term_col], r, -1, MatterPort::RB,
                            { GRID_CELL_WIDTH * -0.2F, GRID_CELL_HEIGHT * -0.1F });
    }

    for (int c = 0; c < this->term_col; c ++) {
        this->move_to_grid(this->chars[this->term_row][c], -1, c, MatterPort::CB,
                            { 0.0F, GRID_CELL_HEIGHT * -0.1F });
    }

    this->sync_cursor();
}

void WarGrey::PLT::ASCIIArtPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
    if (this->scanline->visible()) {
        Box box = this->scanline->get_bounding_box();
        int R, C;

        this->move_to(this->scanline, { this->assistant, MatterPort::RC }, MatterPort::LC);
        this->grid_cell_index(this->scanline, &R, &C, MatterPort::RC);

        for (int r = 0; r < this->term_row; r ++) {
            if (!this->chars[r][0]->visible()) break;

            for (int c = 0; c < this->term_col; c ++) {
                if (!this->chars[r][c]->visible()) break;

                if (this->is_colliding(this->chars[r][c], this->scanline)) {
                    this->chars[r][c]->set_text_color(YELLOW);
                } else if ((r < R) || ((r == R) && (c <= C))) {
                    this->chars[r][c]->set_text_color(DARKGOLDENROD);
                }
            }
        }

        if (this->scanline_extending) {
            if (this->chars[R][0]->visible()) {
                if ((C < 0) || (this->chars[R][C]->visible())) {
                    this->scanline->resize(box.width() + GRID_CELL_WIDTH * 0.25F, LASER_THICKNESS, MatterPort::LC);
                } else if (R < this->term_row) {
                    this->scanline_extending = false;
                    this->scanline->resize(LASER_THICKNESS, LASER_THICKNESS, MatterPort::LC);
                    
                    if (!this->assistant->in_speech()) {
                        this->glide_to_grid(LASER_DURATION, this->assistant, R + 1, LASER_COL0, MatterPort::RC);
                    }
                }
            } else {
                this->scan_done();
            }
        }
    }
}

/*************************************************************************************************/
void WarGrey::PLT::ASCIIArtPlane::on_mission_start(float width, float height) {
    this->clear_screen();
    this->shape_dots.size();
    this->true_shape.reset();
    this->scanline->show(false);
    this->scanline_extending = false;
    this->modinfo->clear_text();
    this->posinfo->clear_text();
    this->spaninfo->clear_text();
}

void WarGrey::PLT::ASCIIArtPlane::scan_done() {
    this->scanline->show(false);
    this->scanline_extending = false;
    this->assistant->shh();
    this->cmdlet_job_done();

    this->move_to_grid(this->assistant, 0, LASER_COL0 - 2, MatterPort::RC);
    this->assistant->set_heading(90.0);

    if (this->algorithm != nullptr) {
        delete this->algorithm;
        this->algorithm = nullptr;
    }
}

void WarGrey::PLT::ASCIIArtPlane::on_motion_complete(Plteen::IMatter* m, float x, float y, double xspd, double yspd) {
    if (this->assistant == m) {
        if (this->scanline->visible()) {
            int N = this->algorithm->height();
            int L = 0;
            int R, C;

            this->grid_cell_index(this->assistant, &R, &C, MatterPort::CC);

            this->assistant->set_heading(0.0);
            this->scanline_extending = true;

            if (R < N) {
                L = R + 1;
                this->assistant->say(MODULE_DURATION, this->algorithm->line_desc(L));
            } else { // mirror
                L = 2 * N - R - 1;
                this->assistant->say(MODULE_DURATION, this->algorithm->line_desc(L));
            }

            if (R == 0) {
                this->posinfo->append_text(MatterPort::LC, "%d", this->algorithm->pos(L));
                this->spaninfo->append_text(MatterPort::LC, "%d", this->algorithm->span(L));
            } else if (this->chars[R][0]->visible()) {
                this->posinfo->append_text(MatterPort::LC, ", %d", this->algorithm->pos(L));
                this->spaninfo->append_text(MatterPort::LC, ", %d", this->algorithm->span(L));
            }
        }
    }
}

void WarGrey::PLT::ASCIIArtPlane::on_bubble_expired(IMatter* who, SpeechBubble type) {
    if (this->current_cmdlet() == VIS_KEY) {
        if (!this->scanline_extending) {
            int R, C;

            this->grid_cell_index(this->assistant, &R, &C, MatterPort::CC);
            this->glide_to_grid(LASER_DURATION, this->assistant, R + 1, LASER_COL0, MatterPort::RC);
        }
    } else {
        this->cmdlet_job_done();
    }
}

void WarGrey::PLT::ASCIIArtPlane::on_cmdlet(size_t idx, char key, const std::string& name, float width, float height) {
    switch (key) {
    case SQR_KEY: this->display_shape(new CharRectangle(this->term_row, this->term_col), name, false); break;
    case RIT_KEY: this->display_shape(new CharRightTriangle(this->term_row, this->term_col), name, false); break;
    case TRI_KEY: this->display_shape(new CharRegularTriangle(this->term_row, this->term_col), name, false); break;
    case RHM_KEY: this->display_shape(new CharHollowRhumbus(this->term_row, this->term_col), name, true); break;
    case VIS_KEY: {
        if (this->algorithm != nullptr) {
            this->scanline->show(true);
            this->scanline->resize(LASER_THICKNESS, LASER_THICKNESS);
            this->glide_to_grid(CMDLET_DURATION, this->assistant, 0, LASER_COL0, MatterPort::RC);
        } else {
            this->cmdlet_job_done();
        }
    }; break;
    }

    this->notify_updated();
}

void WarGrey::PLT::ASCIIArtPlane::display_shape(ASCIIArt* algo, const std::string& name, bool mirror) {
    std::vector<int> lft_cols;
    std::vector<int> rgt_cols;
    Dot dot;
    
    this->algorithm = algo;

    this->assistant->say(CMDLET_DURATION, name);
    this->modinfo->set_text("Model:\n" + this->algorithm->model_desc(), MatterPort::LT);
    this->posinfo->set_text(MatterPort::LC, "%s: ", "起始位置");
    this->spaninfo->set_text(MatterPort::LC, "%s: ", "填充长度");

    this->clear_screen();
    this->shape_dots.clear();
    this->true_shape.reset();
    
    for (int l = 1; l <= algo->height(); l ++) {
        int p = algo->pos(l);
        int s = algo->span(l);

        lft_cols.push_back(p);
        rgt_cols.push_back(p + s - 1);

        this->display(algo->shape_line(l), false);
        this->linefeed(false);
    }

    if (mirror) {
        for (int l = algo->height() - 1; l >= 1; l --) {
            int p = algo->pos(l);
            int s = algo->span(l);

            lft_cols.push_back(p);
            rgt_cols.push_back(p + s - 1);

            this->display(algo->shape_line(l), false);
            this->linefeed(false);
        }
    }

    this->sync_cursor();

    for (int r = 0; r < lft_cols.size(); r ++) {
        dot = this->get_grid_cell_location(r, lft_cols[r] - 1);
        this->shape_dots.push_back({ dot.x, dot.y });
    }

    for (int r = rgt_cols.size() - 1; r >= 0; r --) {
        dot = this->get_grid_cell_location(r, rgt_cols[r] - 1);
        this->shape_dots.push_back({ dot.x, dot.y });
    }

    this->shape_dots.push_back(this->shape_dots[0]);
}

void WarGrey::PLT::ASCIIArtPlane::draw_background(dc_t* dc, float X, float Y, float Width, float Height) {
    if ((this->shape_dots.size() > 0) && (this->scanline->visible())) {
        Dot pos = this->get_matter_location(this->scanline, MatterPort::LC);

        if (this->true_shape.use_count() == 0) {
            this->last_scanline_pos = pos;
            this->true_shape = std::make_shared<Texture>(dc->create_blank_image(Width, Height));
        } else if ((this->assistant->get_heading() != 0.0) && (this->last_scanline_pos != pos) && (this->true_shape->okay())) {
            SDL_Texture* origin = dc->get_target();
            float px, py, t;

            dc->set_target(this->true_shape->self());

            for (float y = flfloor(this->last_scanline_pos.y); y < flfloor(pos.y); y += 1.0F) {
                Dot intersects[2];
            
                for (int i = 1; i < this->shape_dots.size(); i ++) {
                    float x1 = this->shape_dots[i - 1].x;
                    float y1 = this->shape_dots[i - 1].y;
                    float x2 = this->shape_dots[i].x;
                    float y2 = this->shape_dots[i].y;
                        
                    if (lines_intersect(x1, y1, x2, y2, pos.x, y, pos.x + Width, y, &px, &py, &t)) {
                        if ((t >= 0.0F) && (t <= 1.0F)) {
                            if (intersects[0].is_zero()) {
                                intersects[0] = { px, py };
                            } else {
                                intersects[1] = { px, py };
                            }
                        }
                    }
                }

                dc->draw_line(intersects[0].x, intersects[0].y, intersects[1].x, intersects[1].y, true_color);
            }

            dc->set_target(origin);
            this->last_scanline_pos = pos;
        }

        dc->stamp(this->true_shape->self(), X, Y, Width, Height);
    }
}

/*************************************************************************************************/
void WarGrey::PLT::ASCIIArtPlane::display(const std::string& message, bool sync) {
    if (this->term_ridx < this->term_row) {
        size_t i = 0;

        while (i < message.size()) {
            size_t len = string_character_size(message, i);
            std::string character = message.substr(i, len);
            
            i += len;

            if (character != "\n") {
                if (len == 1) {
                    this->chars[this->term_ridx][this->term_cidx]->show(true);
                    this->chars[this->term_ridx][this->term_cidx]->set_text(character, MatterPort::CC);
                    this->move_to_grid(this->chars[this->term_ridx][this->term_cidx], this->term_ridx, this->term_cidx, MatterPort::CC);
                    this->term_cidx ++;
                } else {
                    this->clear_term_cell(this->term_ridx, this->term_cidx, 2);

                    if (this->term_cidx + 2 > this->term_col) {
                        this->linefeed(false);
                    }

                    this->chars[this->term_ridx][this->term_cidx]->show(true);
                    this->chars[this->term_ridx][this->term_cidx]->set_text(character, MatterPort::CC);
                    this->glide_to_grid(CHAR_DURATION, this->chars[this->term_ridx][this->term_cidx],
                                            this->term_ridx, this->term_cidx, MatterPort::CC,
                                            { GRID_CELL_WIDTH * 0.5F, 0.0F });
                    this->term_cidx += 2;
                }
            }
        
            if ((character == "\n") || (this->term_cidx >= this->term_col)) {
                this->linefeed(false);
            }
        }

        if (sync) {
            this->sync_cursor();
        }
    }
}

void WarGrey::PLT::ASCIIArtPlane::linefeed(bool sync) {
    this->term_cidx = 0;
    this->term_ridx ++;
    
    while (this->term_ridx >= this->term_row) {
        this->try_scroll_screen();
        this->term_ridx --;
    }

    if (sync) {
        this->sync_cursor();
    }
}

void WarGrey::PLT::ASCIIArtPlane::sync_cursor() {
    this->glide_to_grid(CURSOR_DURATION, this->cursor, this->term_ridx, this->term_cidx, MatterPort::CC);
}

void WarGrey::PLT::ASCIIArtPlane::clear_term_cell(int row, int col, int count) {
    int end_idx = col + count;

    if (end_idx > this->term_col) {
        end_idx = this->term_col;
    }

    for (int c = col; c < end_idx; c ++) {
        this->chars[row][c]->show(false);
        this->chars[row][c]->set_foreground_color(DIMGRAY);
    }
}

void WarGrey::PLT::ASCIIArtPlane::clear_screen() {
    this->term_ridx = 0;
    this->term_cidx = 0;
    
    if (this->chars != nullptr) {
        for (int r = 0; r < this->term_row; r ++) {
            this->clear_term_cell(r, 0, this->term_col);
        }
    }

    this->sync_cursor();
}

void WarGrey::PLT::ASCIIArtPlane::try_scroll_screen() {
    if (this->chars != nullptr) {
        for (int r = 0; r < this->term_row - 1; r ++) {
            for (int c = 0; c < this->term_col; c ++) {
                this->clear_motion_actions(this->chars[r][c]);
                this->chars[r][c]->set_text(MatterPort::CC, "%s", this->chars[r + 1][c]->c_str());

                if (this->chars[r][c]->content_size() == 1) {
                    this->move_to_grid(this->chars[r][c], r + 1, c, MatterPort::CC);
                } else {
                    this->move_to_grid(this->chars[r][c], r + 1, c, MatterPort::CC, { GRID_CELL_WIDTH * 0.5F, 0.0F });
                }

                this->glide(CHAR_DURATION, this->chars[r][c], { 0.0F, -GRID_CELL_HEIGHT });
            }
        }

        this->clear_term_cell(this->term_row - 1, 0, this->term_col);
    }
}

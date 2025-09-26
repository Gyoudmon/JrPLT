#include "terminal.hpp"

#include <plteen/bang.hpp>

using namespace Plteen;
using namespace WarGrey::PLT;

/*************************************************************************************************/
static const double CMDLET_DURATION = 1.0;
static const double CURSOR_DURATION = 0.3;
static const double CHAR_DURATION = 0.2;
static const float GRID_CELL_WIDTH = 32.0F;
static const float GRID_CELL_HEIGHT = 64.0F;

static const char CLS_KEY = 'c';
static const char MKS_KEY = 'g';
static const char POS_KEY = 'p';
static const char NWL_KEY = 'n';

static const std::string cout_fmt(" std::cout << \"%s\"; ");
static const std::string ccnt_fmt("%s在%s%s。");
static const std::string prnt_fmt(" printf(\"(%%d, %%d)\", %s, %s); ");
static const std::string pcnt_fmt("(%d, %d)");

static const char* subjects [] = {
    "一只修勾", "某e人", "小学生", "挖呀挖老师",
    "班主任", "校长", "孙悟空", "霸王龙",
    "宇宙人", "机器人", "女娲", "奥特曼"
};

static const char* places [] = {
    "长安城", "火星基地", "另一个时空", "月球",
    "梦里", "厕所", "操场", "办公室",
    "教室", "直播间", "婚礼现场", "车祸现场"
};

static const char* actions [] = {
    "跳魔性舞", "偷吃螺蛳粉", "吟诵大悲咒", "模仿校长讲话",
    "写情书", "怀疑人生", "求V我50", "表演无实物驱魔",
    "教李白写代码", "emo发呆", "打怪兽", "突然唱起《孤勇者》"
};

static const TextFacilityConfig<4> text_config = {
    {
        { CLS_KEY, "清空控制台" },
        { MKS_KEY, "荒诞造句" },
        { POS_KEY, "打印位置" },
        { NWL_KEY, "手动换行" }
    },

    {
        { false, false, false, false },
        { false, false, false, false },
        { false, false, false, false },
        { false, false, false, false },
    }
};

/*************************************************************************************************/
WarGrey::PLT::TerminalPlane::TerminalPlane(int r, int c)
        : TheBigBang("终端控制台", GHOSTWHITE), TextFacilityPlane(GameFont::monospace(FontSize::large), text_config)
        , term_row(r), term_col(c), term_ridx(0), term_cidx(0) {
    this->set_background(BLACK);
    this->set_grid_color(DIMGRAY);

    if ((r > 0) || (c > 0)) {
        this->chars = new Labellet**[this->term_row + 1];
        
        for (int r = 0; r <= this->term_row; r ++) {
            this->chars[r] = new Labellet*[this->term_col + 1];
        }
    }
}

WarGrey::PLT::TerminalPlane::~TerminalPlane() noexcept {
    if (this->chars != nullptr) {
        for (int r = 0; r <= this->term_row; r ++) {
            delete [] this->chars[r];
        }

        delete [] this->chars;
    }
}

void WarGrey::PLT::TerminalPlane::load(float width, float height) {
    auto label_font = GameFont::monospace(FontSize::xx_large);
    auto digit_font = GameFont::fantasy(FontSize::x_large);
    auto base_font = GameFont::Default(FontSize::medium);
    auto seq_font = GameFont::Default(FontSize::x_small);

    TextFacilityPlane::load(width, height);
    this->stage = this->insert(new Rectanglet(this->term_col * GRID_CELL_WIDTH, this->term_row * GRID_CELL_HEIGHT, transparent));
    this->assistant = this->insert(new Klose());

    for (int r = 0; r < this->term_row; r ++) {
        for (int c = 0; c < this->term_col; c ++) {
            this->chars[r][c] = this->insert(new Labellet(label_font, GHOSTWHITE, " "));
        }
    }

    this->cursor = this->insert(new Labellet(label_font, ROYALBLUE, "_"));

    for (int r = 0; r < this->term_row; r ++) {
        this->chars[r][this->term_col] = this->insert(new Labellet(seq_font, SILVER, "%d", r + 1));
    }

    for (int c = 0; c < this->term_col; c ++) {
        this->chars[this->term_row][c] = this->insert(new Labellet(seq_font, SILVER, "%d", c + 1));
    }
}

void WarGrey::PLT::TerminalPlane::reflow(float width, float height) {
    TextFacilityPlane::reflow(width, height);

    this->move_to(this->stage, { width * 0.5F, height * 0.5F }, MatterPort::CC);
    this->move_to(this->assistant, { this->stage, MatterPort::RT}, MatterPort::LT, {GRID_CELL_WIDTH, 0.0F });
    this->create_grid(this->term_row, this->term_col, this->stage);

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
}

void WarGrey::PLT::TerminalPlane::update(uint64_t count, uint32_t interval, uint64_t uptime) {
}

bool WarGrey::PLT::TerminalPlane::can_select(IMatter* m) {
    return ThePLTPlane::can_select(m);
}

/*************************************************************************************************/
void WarGrey::PLT::TerminalPlane::on_mission_start(float width, float height) {
    this->clear_screen();
}

void WarGrey::PLT::TerminalPlane::on_motion_complete(Plteen::IMatter* m, float x, float y, double xspd, double yspd) {
}

void WarGrey::PLT::TerminalPlane::on_bubble_expired(IMatter* who, SpeechBubble type) {
    this->clear_working_facility();
}

void WarGrey::PLT::TerminalPlane::on_facility_command(size_t idx, char key, float width, float height) {
    switch (key) {
    case MKS_KEY: {
        const char* who = subjects[random_uniform(1, sizeof(subjects) / sizeof(char*)) - 1];
        const char* where = places[random_uniform(1, sizeof(places) / sizeof(char*)) - 1];
        const char* action = actions[random_uniform(1, sizeof(actions) / sizeof(char*)) - 1];
        std::string sentence = make_nstring(ccnt_fmt.c_str(), who, where, action);
        std::string fmt_str = make_nstring(cout_fmt.c_str(), sentence.c_str());
        
        this->assistant->say(CMDLET_DURATION, fmt_str);
        this->display(sentence);
    }; break;
    case POS_KEY: {
        std::string fmt_str = make_nstring(prnt_fmt.c_str(), "row", "col");
        std::string coordinate = make_nstring(pcnt_fmt.c_str(), this->term_ridx + 1, this->term_cidx + 1);

        this->assistant->say(CMDLET_DURATION, fmt_str);
        this->display(coordinate);
    }; break;
    case NWL_KEY: {
        this->assistant->say(CMDLET_DURATION, " <Enter> ");
        this->display("\n");
    }; break;
    case CLS_KEY: {
        this->assistant->say(CMDLET_DURATION, " cls ");
        this->clear_screen();
    }; break;
    }

    this->notify_updated();
}

/*************************************************************************************************/
void WarGrey::PLT::TerminalPlane::display(const std::string& message) {
    if (this->term_ridx < this->term_row) {
        size_t i = 0;

        while (i < message.size()) {
            size_t len = string_character_size(message, i);
            std::string character = message.substr(i, len);
            
            i += len;

            if (character != "\n") {
                if (len == 1) {
                    this->chars[this->term_ridx][this->term_cidx]->set_text(character, MatterPort::CC);
                    this->move_to_grid(this->chars[this->term_ridx][this->term_cidx], this->term_ridx, this->term_cidx, MatterPort::CC);
                    this->term_cidx ++;
                } else {
                    this->clear_term_cell(this->term_ridx, this->term_cidx, 2);

                    if (this->term_cidx + 2 > this->term_col) {
                        this->linefeed();
                    }

                    this->chars[this->term_ridx][this->term_cidx]->set_text(character, MatterPort::CC);
                    this->glide_to_grid(CHAR_DURATION, this->chars[this->term_ridx][this->term_cidx],
                                            this->term_ridx, this->term_cidx, MatterPort::CC,
                                            { GRID_CELL_WIDTH * 0.5F, 0.0F });
                    this->term_cidx += 2;
                }
            }
        
            if ((character == "\n") || (this->term_cidx >= this->term_col)) {
                this->linefeed();
            }
        }

        this->glide_to_grid(CURSOR_DURATION, this->cursor, this->term_ridx, this->term_cidx, MatterPort::CC);
    }
}

void WarGrey::PLT::TerminalPlane::linefeed() {
    this->term_cidx = 0;
    this->term_ridx ++;
    
    while (this->term_ridx >= this->term_row) {
        this->try_scroll_screen();
        this->term_ridx --;
    }
}

void WarGrey::PLT::TerminalPlane::clear_term_cell(int row, int col, int count) {
    int end_idx = col + count;

    if (end_idx > this->term_col) {
        end_idx = this->term_col;
    }

    for (int c = col; c < end_idx; c ++) {
        this->chars[row][c]->set_text(MatterPort::CC, " ");
    }
}

void WarGrey::PLT::TerminalPlane::clear_screen() {
    this->term_ridx = 0;
    this->term_cidx = 0;
    
    if (this->chars != nullptr) {
        for (int r = 0; r < this->term_row; r ++) {
            this->clear_term_cell(r, 0, this->term_col);
        }
    }

    this->glide_to_grid(CURSOR_DURATION, this->cursor, 0, 0, MatterPort::CC);
}

void WarGrey::PLT::TerminalPlane::try_scroll_screen() {
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

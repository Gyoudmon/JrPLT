#include "JrPLTCR.hpp"

#include "menu.hpp"
#include "avatar.hpp"
#include "model.hpp"

#include "view/doorlet.hpp"
#include "view/disciplinelet.hpp"
#include "view/studentlet.hpp"
#include "view/desk/computer_desklet.hpp"

#include <plteen/datum/string.hpp>
#include <plteen/datum/vector.hpp>

#include <sstream>
#include <vector>
#include <map>

using namespace Plteen;
using namespace WarGrey::CAE;

/*************************************************************************************************/
namespace {
    static const size_t DESK_COUNT = 4;
    static const float platform_width = 512.0F;
    static const float platform_height = 64.0F;
    static const double gliding_duration = 0.4;
    static const double radar_alpha = 0.42;
    static const double radar_levels [] = { 0.4, 0.6, 0.8 };
    static const char* radar_variables [] = { "理论", "表达", "逻辑", "批判", "工程", "专注" };

    static const std::vector<DisciplineType> report_disciplines = {
        DisciplineType::Mathematics, DisciplineType::Programming,
        DisciplineType::STEM, DisciplineType::Printing3D
    };
    
    enum class GradeTask { CheckCondition, DoTask, _ };

    /*********************************************************************************************/
    class JrPLTPlane : public Plane, public IMenuEventListener, public IModelListener {
    public:
        JrPLTPlane(const std::string& caein, const std::string& caeout, const std::string& grdout) : Plane("JrPLT CAE") {
            this->model = new CAEModel(this);
            this->caein = caein;
            this->caeout = (caeout.empty()) ? this->caein : caeout;
            this->grdout = grdout;
        }

        virtual ~JrPLTPlane() {
            delete this->model;
        }

    public:
        void load(float width, float height) override {
            this->load_gui_elements(width, height);
            this->load_menus(width, height);
            this->load_classroom(width, height);
            this->load_avatars(width, height);

            try {
                this->model->import_from_file(this->caein);
                this->reflow_model_sprites(gliding_duration);
            } catch (const std::exception& e) {}

            this->load_literacy_radars(width, height);
            this->set_background(SNOW);
        }

        void reflow(float width, float height) override {
            this->reflow_gui_elements(width, height);
            this->reflow_classroom(width, height);
            this->reflow_class_logos();
            this->reflow_discipline_logos();
        }

    public:
        bool can_select(IMatter* m) override {
            return true;
        }
        
        bool can_select_multiple() override {
            return true;
        }

        void after_select(IMatter* m, bool yes) override {
            auto cls = dynamic_cast<DoorSprite*>(m);
            auto dis = dynamic_cast<DisciplineSprite*>(m);
            auto stu = dynamic_cast<StudentSprite*>(m);
            
            if (yes) {
                if (m == this->platform) {
                    if (this->the_disCode > 0U) {
                        this->remove_selected(this->disciplines[this->the_disCode]);
                        this->the_disCode = 0U;
                    }

                    this->reflow_discipline_logos();
                    this->update_students_score_bars(this->the_disCode);
                } else if (stu != nullptr) {
                    this->on_student_changed(stu->primary_key());
                } else if (cls != nullptr) {
                    if ((this->the_task == MenuTask::BindClass) && (this->the_sNo > 0U)) {
                        this->glide_to(gliding_duration, this->students[this->the_sNo], { cls, MatterPort::RC }, MatterPort::LC);
                        this->model->bind_student_to_class(this->the_sNo, cls->primary_key());
                    } else {
                        this->on_class_changed(cls->primary_key(), false);
                    }
                } else if (dis != nullptr) {
                    this->on_discipline_changed(this->model->get_discipline_code(dis->get_type()), false);
                }
            } else {
                if (stu != nullptr) {
                    this->on_student_changed(0U);
                } else if (dis != nullptr) {
                    this->on_discipline_changed(0U, false);
                } else if (cls != nullptr) {
                    // do nothing
                }
            }

            this->switch_menu();
        }

        void on_tap(IMatter* m, float local_x, float local_y) override {
            auto dsk = dynamic_cast<IDesk*>(m);

            if (dsk != nullptr) {
                this->try_exchange_seat(dsk, local_x, local_y);
            } else {
                Plane::on_tap(m, local_x, local_y);
            }
        }

        bool update_tooltip(IMatter* m, float lx, float ly, float gx, float gy) override {
            bool updated = false;
            
            if (m != this->agent) {
                auto student = dynamic_cast<StudentSprite*>(m);
                auto desk = dynamic_cast<IDesk*>(m);
                auto door = dynamic_cast<DoorSprite*>(m);

                if (student != nullptr) {
                    this->tooltip->set_text(" %s[%u] ", student->nickname(), student->primary_key());
                    updated = true;
                } else if (desk != nullptr) {
                    this->tooltip->set_text(" %d ", desk->get_seat_by(lx, ly));
                    updated = true;
                } else if (door != nullptr) {
                    this->tooltip->set_text(" %s ", door->name());
                    updated = true;
                }
            }

            return updated;
        }

    public:
        void on_char(char key, uint16_t modifiers, uint8_t repeats, bool pressed) override {
            if (!pressed) {
                if (isdigit(key)) {
                    int idx = key - '0';
                    IMenu* self = this->menus[this->the_menu_type]->unsafe_plane<IMenu>();

                    if ((idx >= 0) && (idx < self->count() + self->menu_key_base())) {
                        self->on_menu_char(this, this->the_menu_type, key);
                    }
                } else if (key == 'w') {
                    if (!this->caeout.empty()) {
                        try {
                            this->model->export_to_file(this->caeout);
                            this->log_message(Log::Info, make_nstring("done exporting to %s.", this->caeout.c_str()));
                        } catch (const std::exception& e) {
                            this->log_message(Log::Fatal, e.what());
                        }
                    }
                } else if (int(key) == 27) { // ESC
                    this->the_task = MenuTask::_;
                    this->menus[this->the_menu_type]->unsafe_plane<IMenu>()->select_menu('\0');
                }
            }
        }

        void on_text(const char* text, size_t size, bool entire) override {
            if (entire) {
                if (size > 0) {
                    try {
                        switch (this->the_task) {
                        case MenuTask::CreateClass: this->model->create_class_from_user_input(text, size); break;
                        case MenuTask::DeleteClass: this->on_model_deletion(this->the_task, text, size); break;
                        case MenuTask::CreateDiscipline: this->model->create_discipline_from_user_input(text, size); break;
                        case MenuTask::DeleteDiscipline: this->on_model_deletion(this->the_task, text, size); break;
                        case MenuTask::CreateStudent: this->model->create_student_from_user_input(text, size); break;
                        case MenuTask::UpdateStudent: this->model->update_student_from_user_input(this->the_sNo, text, size); break;
                        case MenuTask::UpdateAvatar: this->model->update_student_avatar_from_user_input(this->the_sNo, text, size); break;
                        case MenuTask::DeleteStudent: this->on_model_deletion(this->the_task, text, size); break;
                        case MenuTask::CreateGrade: this->on_grade_text(text, size); break;
                        case MenuTask::UpdateGrade: this->on_grade_text(text, size); break;
                        case MenuTask::DeleteGrade: this->on_grade_text(text, size); break;
                        case MenuTask::ClearStudent: this->on_model_deletion(this->the_task, text, size); break;
                        case MenuTask::ClearGrade: this->on_model_deletion(this->the_task, text, size); break;
                        default: /* do nothing */;
                        }
                    } catch (const std::exception& e) {
                        this->log_message(Log::Fatal, e.what());
                    }
                } else if (this->the_grade_subtask != GradeTask::_) {
                    this->on_grade_text(text, size);
                }
            }
        }

        void on_grade_text(const char* text, size_t size) {
            switch (this->the_grade_subtask) {
            case GradeTask::CheckCondition: {
                size_t pos = 0U;
                uint64_t ts = scan_natural(text, &pos, size);
                    
                if (ts > 0U) {
                    this->the_timestamp = ts;
                }

                this->on_grade_task(this->the_timestamp > 0U);
            }; break;
            case GradeTask::DoTask: {
                if (size > 0U) {
                    switch (this->the_task) {
                    case MenuTask::CreateGrade: {
                        this->model->register_student_scores_from_user_input(this->the_sNo,
                            this->the_disCode, this->the_timestamp,
                            text, size);
                    }; break;
                    case MenuTask::UpdateGrade: {
                        this->model->update_student_scores_from_user_input(this->the_sNo,
                            this->the_disCode, this->the_timestamp,
                            text, size);
                    }; break;
                    default: /* do nothing */;
                    }

                    this->update_student_report(this->the_sNo);
                    this->update_students_score_bars(this->the_disCode, this->the_sNo);
                    this->log_message(Log::Info, "Done.");
                }

                this->the_grade_subtask = GradeTask::_;
            }; break;
            default: /* do nothing */;
            }
        }

        void on_model_deletion(MenuTask task, const char* text, size_t size) {
            std::string answer;
            size_t pos = 0;
            
            scan_skip_space(text, &pos, size);
            answer = scan_string(text, &pos, size);

            if (string_ci_equal(answer, "y") || string_ci_equal(answer, "yes")) {
                try {
                    switch (task) {
                    case MenuTask::DeleteClass: this->model->delete_class_as_user_request(this->the_clsId); break;
                    case MenuTask::DeleteDiscipline: this->model->delete_discipline_as_user_request(this->the_disCode); break;
                    case MenuTask::DeleteStudent: this->model->delete_student_as_user_request(this->the_sNo); break;
                    case MenuTask::ClearStudent: this->model->clear_detached_students(); break;
                    case MenuTask::ClearGrade: this->model->clear_detached_grades(); break;
                    default: /* do nothing */;
                    }

                    this->log_message(Log::Info, "Done.");
                    this->switch_menu();
                } catch (const std::exception& e) {
                    this->log_message(Log::Fatal, e.what());
                }
            }
        }

    public:
        void on_menu_switch(MenuType self, MenuType to) override {
            if (self != to) {
                if (this->menus.find(to) != this->menus.end()) {
                    this->menus[self]->unsafe_plane<IMenu>()->select_menu('\0');
                    this->menus[self]->show(false);
                    this->menus[to]->show(true);
                    this->the_menu_type = to;
                }

                this->the_task = MenuTask::_;
                this->the_grade_subtask = GradeTask::_;

                // this->avatar->show(this->the_menu_type == MenuType::Student);
            }
        }
        
        void on_menu_task(MenuType self, MenuTask task) override {
            this->the_task = task;

            try {
                switch (task) {
                case MenuTask::Exit: this->mission_complete(); break;
                case MenuTask::CreateClass: this->start_input_text("Class Info(%s): ", ClassEntity::prompt()); break;
                case MenuTask::DeleteClass: this->start_input_text("Delete Class[%llu](Y/N)?: ", this->the_clsId); break;
                case MenuTask::CreateDiscipline: this->start_input_text("Discipline Info(%s): ", DisciplineEntity::prompt()); break;
                case MenuTask::DeleteDiscipline: this->start_input_text("Delete Discipline[%s](Y/N)?: ", this->disciplines[this->the_disCode]->name()); break;
                case MenuTask::CreateStudent: this->start_input_text("Studuent Info(%s): ", StudentEntity::prompt()); break;
                case MenuTask::UpdateStudent: this->start_input_text("Student[%s] Info(%s): ", this->students[this->the_sNo]->nickname(), StudentEntity::update_prompt()); break;
                case MenuTask::UpdateAvatar: this->start_input_text("Student[%s] Gender Info(%s): ", this->students[this->the_sNo]->nickname(), StudentEntity::update_gender_prompt()); break;
                case MenuTask::DeleteStudent: this->start_input_text("Delete Student[%s](Y/N)?: ", this->students[this->the_sNo]->nickname()); break;
                case MenuTask::CreateGrade: this->on_grade_task(false); break;
                case MenuTask::UpdateGrade: this->on_grade_task(false); break;
                case MenuTask::DeleteGrade: this->on_grade_task(false); break;
                case MenuTask::ClearStudent: this->start_input_text("Clear Detached Students(Y/N)?: "); break;
                case MenuTask::ClearGrade: this->start_input_text("Clear Detached Points(Y/N)?: "); break;
                case MenuTask::ExportGrade: {
                    try {
                        if (!this->grdout.empty()) {
                            this->model->export_grade_to_file(this->grdout);
                            this->log_message(Log::Info, make_nstring("done exporting grades to %s.", this->grdout.c_str()));
                        } else {
                            this->model->export_grade_to_file(std::cout);
                            this->log_message(Log::Info, make_nstring("done exporting grades to stdout."));
                        }
                    } catch (const std::exception& e) {
                        this->log_message(Log::Fatal, e.what());
                    }
                }; break;        
                default: /* do nothing */;
                }
            } catch (const std::exception& e) {
                this->log_message(Log::Fatal, e.what());
            }
        }

        void on_grade_task(bool timestamp_okay) {
            this->the_grade_subtask = GradeTask::CheckCondition;

            if (timestamp_okay || this->check_timestamp_for_grade_task()) {
                this->the_grade_subtask = GradeTask::DoTask;

                switch (this->the_task) {
                case MenuTask::CreateGrade: {
                    this->start_input_text("Create Discipline Points(%s@%llu): ",
                        this->disciplines[this->the_disCode]->name(),
                        this->the_timestamp);
                }; break;
                case MenuTask::UpdateGrade: {
                    this->start_input_text("Update Discipline Points(%s@%llu): ",
                        this->disciplines[this->the_disCode]->name(),
                        this->the_timestamp);
                }; break;
                case MenuTask::DeleteGrade: {
                    this->model->delete_student_scores_as_user_request(this->the_sNo,
                        this->the_disCode, this->the_timestamp);
                    this->update_student_report(this->the_sNo);
                    this->update_students_score_bars(this->the_sNo, this->the_disCode);
                    this->the_grade_subtask = GradeTask::_;
                 }; break;
                default: /* do nothing */; break;
                }
            }
        }

        bool check_timestamp_for_grade_task() {
            this->the_grade_subtask = GradeTask::CheckCondition;

            if (this->the_timestamp == 0U) {
                this->start_input_text("Timestamp(YYYYMMDD): ");
            } else {
                this->start_input_text("We are editing points of %llu: ", this->the_timestamp);
            }

            return false;
        }

    public:
        void on_class_created(uint64_t pk, shared_class_t entity, bool in_batching) override {
            this->doors[pk] = this->insert(new DoorSprite(pk, entity->get_nickname()));
            this->doors[pk]->resize_by_height(platform_height * 0.72F);

            if (!in_batching) {
                this->reflow_class_logos(gliding_duration);
            }
        }

        void on_class_deleted(uint64_t pk, shared_class_t entity, bool in_batching) override {
            this->remove(this->doors[pk]);
            this->doors.erase(pk);

            if (this->the_clsId == pk) {
                this->on_class_changed(0U, in_batching);
            }

            if (!in_batching) {
                this->reflow_class_logos(gliding_duration);
            }
        }

        void on_class_changed(uint64_t clsId, bool in_batching) {
            if (this->doors.find(this->the_clsId) != this->doors.end()) {
                this->remove_selected(this->doors[this->the_clsId]);
                this->doors[this->the_clsId]->close();
            }

            this->the_clsId = (this->the_clsId != clsId) ? clsId : 0U;
            this->stuLabel->set_text(MatterPort::RB, " ");

            if (this->the_clsId > 0U) {
                this->doors[this->the_clsId]->open();
                this->clsLabel->set_text(MatterPort::RT, "%zu in %s",
                    this->model->get_class_population(this->the_clsId),
                    this->doors[this->the_clsId]->name());
            } else {
                this->clsLabel->set_text(MatterPort::RB, " ");    
            }
            
            if (!in_batching) {
                this->reflow_students(gliding_duration);
            }
        }

        void on_discipline_created(uint64_t pk, shared_discipline_t entity, bool in_batching) override {
            this->disciplines[pk] = this->insert(new DisciplineSprite(entity->cannonical_type()));
            this->disciplines[pk]->resize_by_height(platform_height * 0.90F);

            if (!in_batching) {
                this->reflow_discipline_logos(gliding_duration);
            }
        }

        void on_discipline_deleted(uint64_t pk, shared_discipline_t entity, bool in_batching) override {
            this->remove(this->disciplines[pk]);
            this->disciplines.erase(pk);

            if (this->the_disCode == pk) {
                this->on_discipline_changed(0U, in_batching);
            }

            if (!in_batching) {
                this->reflow_discipline_logos(gliding_duration);
            }
        }

        void on_discipline_changed(uint64_t pk, bool in_batching) {
            if (this->disciplines.find(this->the_disCode) != this->disciplines.end()) {
                this->remove_selected(this->disciplines[this->the_disCode]);
            }
            
            this->the_disCode = pk;

            if (!in_batching) {
                this->update_students_score_bars(this->the_disCode);
                this->reflow_discipline_logos(gliding_duration);
            }
        }

        void on_student_created(uint64_t pk, shared_student_t entity, bool in_batching) override {
            this->students[pk] = this->insert(new StudentSprite(pk, entity->get_nickname(), entity->get_avatar()));

            if (!in_batching) {
                this->reflow_students(gliding_duration);
            }
        }

        void on_student_updated(uint64_t pk, shared_student_t entity) override {
            this->students[pk]->give_nickname(entity->get_nickname());
            this->on_student_changed(pk);
            this->log_message(Log::Info, make_nstring("Student(%s) has been updated.", this->students[pk]->nickname()));
        }

        void on_student_avatar_updated(uint64_t pk, shared_student_t entity) override {
            this->on_student_deleted(pk, entity, true);
            this->on_student_created(pk, entity, false);
        }

        void on_student_deleted(uint64_t pk, shared_student_t entity, bool in_batching) override {
            this->remove(this->students[pk]);
            this->students.erase(pk);

            if (this->the_sNo == pk) {
                this->on_student_changed(0U);
            }

            if (!in_batching) {
                this->reflow_students(gliding_duration);
            }
        }

        void on_student_changed(uint64_t sNo) {
            if (this->students.find(this->the_sNo) != this->students.end()) {
                this->remove_selected(this->students[this->the_sNo]);
            }
            
            this->the_sNo = sNo;

            if (this->the_sNo > 0U) {
                this->stuLabel->set_text(MatterPort::RB, "%s", this->students[this->the_sNo]->nickname());
            } else {
                this->stuLabel->set_text(MatterPort::RB, "-");
            }

            this->update_student_report(this->the_sNo);
        }

    protected:
        void on_motion_complete(IMatter* m, float x, float y, double xspd, double yspd) override {
            auto dis = dynamic_cast<DisciplineSprite*>(m);
            auto stu = dynamic_cast<StudentSprite*>(m);

            if (dis != nullptr) {
                if (this->the_disCode > 0U) {
                    if (this->model->get_discipline_code(dis->get_type()) != this->the_disCode) {
                        dis->show(false);
                    }
                }
            } else if (stu != nullptr) {
                if (this->the_task == MenuTask::BindClass) {
                    stu->show(this->the_clsId == this->model->get_student_class(stu->primary_key()));

                    if (!stu->visible()) {
                        this->on_student_changed(0U);
                    }
                }
            }
        }

    private:
        void load_gui_elements(float width, float height) {
            this->agent = this->insert(new Linkmon());
            this->title = this->insert(new Labellet(GameFont::Title(), BLACK, "%s", this->name()));
            this->side_border = this->insert(new VLinelet(height, GRAY));
            this->tooltip = this->insert(make_label_for_tooltip(GameFont::fangsong()));
            
            this->set_sentry_sprite(this->agent);
            this->set_tooltip_matter(this->tooltip);
            this->agent->scale(-1.0F, 1.0F);
            this->side_border->camouflage(true);
            this->tooltip->set_background_color(GHOSTWHITE);
        }

        void reflow_gui_elements(float width, float height) {
            float sidebar_width = this->calculate_sidebar_width();
            float gap = 3.0F;

            this->move_to(this->title, { this->agent, MatterPort::RB }, MatterPort::LB);
            this->move_to(this->side_border, { sidebar_width, height }, MatterPort::CB);
            this->move_to(this->literacy_radar, { sidebar_width * 0.5F, height }, MatterPort::CB );
            
            for (auto menu : this->menus) {
                this->move_to(menu.second, { this->agent, MatterPort::LB }, MatterPort::LT, { 4.0F, 4.0F });
            }

            this->move_to(this->avatar, { 0.0F, height }, MatterPort::LB, { gap, -gap });
        }

    private:
        void load_classroom(float width, float height) {
            float sidebar_pos = this->calculate_sidebar_width();
            float croom_width = width - sidebar_pos;
            float desk_width = croom_width / float(DESK_COUNT + 4);
            float desk_height = height - platform_height * 1.618;

            this->platform = this->insert(new Rectanglet(platform_width, platform_height, SKYBLUE));
            this->clsLabel = this->insert(new Labellet(GameFont::monospace(FontSize::x_large), GHOSTWHITE, "[Class]"));
            this->stuLabel = this->insert(new Labellet(GameFont::serif(), GHOSTWHITE, "%s", "[Student]"));
            
            this->desks.push_back(this->insert(new ComputerDesklet(1, desk_width, desk_height, 12,  1, DARKSEAGREEN)));
            this->desks.push_back(this->insert(new ComputerDesklet(2, desk_width, desk_height, 12, 13, DARKSEAGREEN)));
            this->desks.push_back(this->insert(new ComputerDesklet(3, desk_width, desk_height, 12, 25, DARKSEAGREEN)));
            this->desks.push_back(this->insert(new ComputerDesklet(4, desk_width, desk_height, 12, 37, DARKSEAGREEN)));
        }

        void reflow_classroom(float width, float height) {
            float sidebar_pos = this->calculate_sidebar_width();
            float croom_width = width - sidebar_pos;
            float desk_cy = (height - platform_height) * 0.5F;

            this->move_to(this->platform, { croom_width * 0.50F + sidebar_pos, height * 1.0F }, MatterPort::CB);
            this->move_to(this->clsLabel, { this->platform, MatterPort::RT }, MatterPort::RT, { -4.0F, +4.0F });
            this->move_to(this->stuLabel, { this->platform, MatterPort::RB }, MatterPort::RB, { -4.0F, -4.0F });

            this->move_to(this->desks[0], { croom_width * 0.125F + sidebar_pos, desk_cy }, MatterPort::LC);
            this->move_to(this->desks[1], { croom_width * 0.250F + sidebar_pos, desk_cy }, MatterPort::LC);
            this->move_to(this->desks[2], { croom_width * 0.750F + sidebar_pos, desk_cy }, MatterPort::RC);
            this->move_to(this->desks[3], { croom_width * 0.875F + sidebar_pos, desk_cy }, MatterPort::RC);

            this->reflow_model_sprites(0.0);
        }

        void reflow_model_sprites(double duration = gliding_duration) {
            this->reflow_class_logos(duration);
            this->reflow_discipline_logos(duration);
            this->reflow_students(duration);
        }

        void reflow_class_logos(double duration = gliding_duration) {
            if (!this->doors.empty()) {
                cPoint spot = this->get_matter_location(this->side_border, MatterPort::CB);
                Box grid = this->doors.rbegin()->second->get_bounding_box() * 1.2F;

                for (auto cls = this->doors.rbegin(); cls != this->doors.rend(); cls ++) {
                    this->glide_to(duration, cls->second, spot, MatterPort::CB, { 0.0F, -1.0F });
                    spot -= cPoint(0.0F, grid.height());
                }
            }
        }

        void reflow_discipline_logos(double duration = gliding_duration) {
            if (!this->disciplines.empty()) {
                cPoint gap(4.0F, 0.0F);
                cPoint spot = this->get_matter_location(this->platform, MatterPort::LC) + gap;
                float grid_width = this->disciplines.begin()->second->get_bounding_box().width() + _X(gap);
                float dis_x0 = _X(spot);

                for (auto dis : this->disciplines) {
                    uint64_t disCode = this->model->get_discipline_code(dis.second->get_type());

                    if ((disCode == this->the_disCode) || (this->the_disCode == 0U)) {
                        dis.second->show(true);
                        this->glide_to(duration, dis.second, spot, MatterPort::LC);
                        spot += cPoint(grid_width, 0.0F);
                    } else {
                        this->glide_to(duration, dis.second, { dis_x0, _Y(spot) }, MatterPort::LC);
                    }
                }
            }
        }

        void reflow_students(double duration = gliding_duration) {
            if (!this->students.empty()) {
                cPoint nocls_stu = this->get_matter_location(this->side_border, MatterPort::LB);
                Box grid = this->students.begin()->second->get_bounding_box();
                uint64_t desk_idx, seat_idx;
                float gap = 4.0F;
         
                grid.rbdot += cVector(gap, gap);
                set_X(nocls_stu, _X(nocls_stu) * 0.90F);
                set_Y(grid.ltdot, grid.height() * 3.0F);

                for (auto stu : this->students) {
                    uint64_t stuClsId = this->model->get_student_class(stu.second->primary_key());

                    stu.second->show((stuClsId == this->the_clsId) || (stuClsId == 0U));

                    if (stuClsId == 0U) {
                        this->glide_to(duration, stu.second, nocls_stu, MatterPort::RB);

                        if (_Y(nocls_stu) > grid_y) {
                            nocls_stu -= cVector(0.0F, grid.height());
                        } else {
                            nocls_stu -= cVector(grid.width(), 0.0F);
                            set_Y(nocls_stu, _Y(this->get_matter_location(this->side_border, MatterPort::LB)));
                        }
                    } else {
                        if (stu.second->visible()) {
                            this->model->feed_student_seat(stu.first, &desk_idx, &seat_idx);

                            if ((desk_idx > 0U) && (seat_idx > 0U) && (desk_idx <= this->desks.size())) {
                                this->desks[desk_idx - 1]->sit(stu.second, seat_idx, duration);
                            } else {
                                this->glide_to(duration, stu.second, { this->doors[stuClsId], MatterPort::RC }, MatterPort::LC);
                            }
                        } else {
                            this->move_to(stu.second, { this->doors[stuClsId], MatterPort::RC }, MatterPort::LC);
                        }
                    }
                }
            }
        }

    private:
        void try_exchange_seat(IDesk* dsk, float local_x, float local_y) {
            if (this->the_sNo > 0U) {
                if (this->the_clsId > 0U) {
                    int idx = dsk->get_seat_by(local_x, local_y);

                    if (idx > 0) {
                        uint64_t stuTarget = this->model->get_student_at_seat(this->the_clsId, dsk->get_index(), idx);
                            
                        dsk->sit(this->students[this->the_sNo], idx, gliding_duration);

                        if (stuTarget > 0U) { // 交换座位
                            uint64_t the_dsk, the_st;

                            this->model->feed_student_seat(this->the_sNo, &the_dsk, &the_st);

                            if (the_dsk > 0U) {
                                this->desks[the_dsk - 1]->sit(this->students[stuTarget], the_st, gliding_duration);
                                this->model->bind_student_to_seat(stuTarget, the_dsk, the_st);
                            } else {
                                this->model->bind_student_to_seat(stuTarget, 0, the_st);
                                this->glide_to(gliding_duration, this->students[stuTarget],
                                    { this->doors[this->the_clsId], MatterPort::RC },
                                    MatterPort::LC);
                            }
                        }

                        if (this->model->get_student_class(this->the_sNo) == 0U) {
                            this->model->bind_student_to_class(this->the_sNo, this->the_clsId);
                        }

                        this->model->bind_student_to_seat(this->the_sNo, dsk->get_index(), idx);
                    }
                } else {
                    this->log_message(Log::Error, "Hasn't Entered a Class");
                }
            }
        }

    private:
        void switch_menu() {
            if ((this->the_disCode > 0U) || (this->the_sNo > 0U)) {
                if ((this->the_disCode > 0U) && (this->the_sNo > 0U)) {
                    this->on_menu_switch(the_menu_type, MenuType::Grade);
                } else if (this->the_sNo > 0U) {
                    this->on_menu_switch(the_menu_type, MenuType::Student);
                } else {
                    this->on_menu_switch(the_menu_type, MenuType::Discipline);
                }
            } else if ((this->the_clsId > 0U) && (this->is_selected(this->doors[this->the_clsId]))) {
                this->on_menu_switch(the_menu_type, MenuType::Class);
            } else {
                this->on_menu_switch(the_menu_type, MenuType::TopLevel);
            }
        }
        
        void load_menus(float width, float height) {
            this->menus[MenuType::TopLevel] = this->insert(new Continent(new TopLevelMenu()));
            this->menus[MenuType::Class] = this->insert(new Continent(new ClassMenu()));
            this->menus[MenuType::Discipline] = this->insert(new Continent(new DisciplineMenu()));
            this->menus[MenuType::Student] = this->insert(new Continent(new StudentMenu()));
            this->menus[MenuType::Grade] = this->insert(new Continent(new GradeMenu()));
            this->menus[MenuType::Clear] = this->insert(new Continent(new ClearMenu()));

            for (auto menu : this->menus) {
                if (this->the_menu_type != menu.first) {
                    menu.second->show(false);
                }

                menu.second->camouflage(true);
            }
        }

        void load_avatars(float width, float height) {
            this->avatar = this->insert(new Continent(new AvatarPlane("Avatars")));

            this->avatar->set_border_color(ROYALBLUE);
            this->avatar->set_background_color(RGBA(DIMGRAY, 0.64));

            this->avatar->show(false);
            this->avatar->camouflage(true);
        }

        void load_literacy_radars(float width, float height) {
            this->literacy_radar = this->load_literacy_radar(width, height, "Student's Assessment");
        }

        Radarlet* load_literacy_radar(float width, float height, const char* title) {
            float side_width = this->calculate_sidebar_width();
            float radar_radius = side_width * 0.382F;

            auto radar = this->insert(new Radarlet(sizeof(radar_variables)/sizeof(char*), radar_radius, WHITESMOKE, 0.90));

            radar->set_title(title);
            radar->set_start_angle(0.0, false);
            radar->set_variable_names(radar_variables);
            radar->camouflage(true);

            radar->set_levels(radar_levels);
            radar->push_observation( std::vector<double>(radar->variable_length(), 0.0).data(), SEAGREEN,   radar_alpha);
            radar->push_observation( std::vector<double>(radar->variable_length(), 0.0).data(), DODGERBLUE, radar_alpha);

            this->update_report_discipline(radar);
            
            return radar;
        }

        void update_report_discipline(Radarlet* radar) {
            std::vector<double> vars(radar->variable_length());
            std::vector<DisciplineType> actual_types;

            for (auto disType : report_disciplines) {
                uint64_t disCode = this->model->get_discipline_code(disType);

                if (disCode > 0U) {
                    actual_types.push_back(disType);
                }
            }

            for (size_t idx = 0; idx < radar->variable_length(); idx ++) {
                vars[idx] = random_uniform(0.0, 1.0);
            }

            radar->set_observation(1, vars.data());

            // report->set_disciplines(actual_types, MatterPort::LB);
        }

        void update_student_report(uint64_t sNo) {
            if (sNo == 0U) {
            } else {
                uint64_t lts = this->model->get_student_latest_timestamp(this->the_sNo);
                uint64_t pts = this->model->get_student_latest_timestamp(this->the_sNo, 1);

                if (lts > 0U) {
                    std::vector<std::vector<double>> s_pts;
                    std::vector<double> diffs;
    
                    for (auto disType : report_disciplines) {
                        uint64_t disCode = this->model->get_discipline_code(disType);
                        std::vector<double> lpts, ppts;

                        if (disCode > 0U) {
                            this->model->feed_student_score_points(sNo, disCode, lts, lpts);
                            this->model->feed_student_score_points(sNo, disCode, pts, ppts);
                        }

                        if (ppts.size() > 0) {
                            diffs.push_back(vector_sum(lpts) - vector_sum(ppts));
                        } else {
                            diffs.push_back(flnan);
                        }
                        
                        s_pts.push_back(lpts);
                    }
                }

                this->update_report_discipline(this->literacy_radar);
            }
        }

        void update_students_score_bars(uint64_t disCode, uint64_t sNo = 0U) {
            if (sNo == 0U) {
                for (auto& stu : this->students) {
                    this->update_students_score_bars(disCode, stu.first);
                }
            } else if (this->model->get_student_class(sNo) == this->the_clsId) {
                if (disCode == 0U) {
                    this->students[sNo]->set_score_percentage(0.0);
                } else {
                    uint64_t ts = this->model->get_student_latest_timestamp(sNo);
                    uint32_t color = CRIMSON;
                    double score = 0.0;
                    
                    if (ts > 0U) {
                        score = this->model->get_student_score(sNo, disCode, ts);
                    }

                    if (score >= 80.0) {
                        color = GREEN;
                    } else if (score >= 60.0) {
                        color = ORANGE;
                    }

                    this->students[sNo]->set_score_percentage(score / 100.0, color);
                }
            }
        }

    private:
        float calculate_sidebar_width() {
            Box abox = this->agent->get_bounding_box();
            Box tbox = this->title->get_bounding_box();

            return tbox.width() + abox.width() * 1.618F;
        }

    private:
        Rectanglet* platform;
        std::vector<ComputerDesklet*> desks;
    
    private:
        Labellet* title;
        Labellet* tooltip;
        Labellet* clsLabel;
        Labellet* stuLabel;
        Linelet* side_border;
        Radarlet* literacy_radar;
        std::map<uint64_t, DoorSprite*> doors;
        std::map<uint64_t, DisciplineSprite*> disciplines;
        std::map<uint64_t, StudentSprite*> students;
        std::map<MenuType, Continent*> menus;
        Continent* avatar;
        Linkmon* agent;

    private:
        MenuType the_menu_type = MenuType::TopLevel;
        MenuTask the_task = MenuTask::_;
        uint64_t the_clsId = 0U;
        uint64_t the_disCode = 0U;
        uint64_t the_sNo = 0U;
        CAEModel* model;

    private:
        GradeTask the_grade_subtask = GradeTask::_;
        uint64_t the_timestamp = 0U;

    private:
        std::string caein;
        std::string caeout;
        std::string grdout;
    };
}

/*************************************************************************************************/
namespace { enum CAECmdOpt { CAEIn, CAEOut, GRDOut, _ }; }

void WarGrey::CAE::JrPLTCRCosmos::construct(int argc, char* argv[]) {
    enter_digimon_zone(argv[0]);
    imgdb_setup(digimon_subdir("stone"));

#ifdef __windows__
    digimon_appdata_setup("C:\\opt\\JrPLT\\");
    digimon_mascot_setup("C:\\opt\\JrPLT\\stone\\mascot");
#else
    digimon_appdata_setup("/opt/JrPLT/");
    digimon_mascot_setup("/opt/JrPLT/stone/mascot");
#endif

    this->parse_commandline_argument(argc, argv);
    this->set_snapshot_folder("/Users/wargrey/Desktop");
    this->set_cmdwin_height(24);

    GameFont::fontsize(20);

    this->push_plane(new JrPLTPlane(this->caein, this->caeout, this->grdout));
}

void WarGrey::CAE::JrPLTCRCosmos::parse_commandline_argument(int argc, char* argv[]) {
    CAECmdOpt opt = CAECmdOpt::_;

    for (int idx = 1; idx < argc; idx ++) {
        switch (opt) {
        case CAECmdOpt::CAEIn: {
            this->caein = argv[idx];
            opt = CAECmdOpt::_;
        }; break;
        case CAECmdOpt::CAEOut: {
            this->caeout = argv[idx];
            opt = CAECmdOpt::_;
        }; break;
        case CAECmdOpt::GRDOut: {
            this->grdout = argv[idx];
            opt = CAECmdOpt::_;
        }; break;
        default: {
            if ((strcmp(argv[idx], "-i") == 0) || (strcmp(argv[idx], "--in") == 0)) {
                opt = CAECmdOpt::CAEIn;
            } else if ((strcmp(argv[idx], "-o") == 0) || (strcmp(argv[idx], "--out") == 0)) {
                opt = CAECmdOpt::CAEOut;
            } else if ((strcmp(argv[idx], "-g") == 0) || (strcmp(argv[idx], "--grdout") == 0)) {
                opt = CAECmdOpt::GRDOut;
            }
        }; break;
        }
    }
}

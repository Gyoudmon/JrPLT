#include "model.hpp"

#include <plteen/datum/box.hpp>
#include <plteen/datum/fixnum.hpp>
#include <plteen/datum/flonum.hpp>
#include <plteen/datum/vector.hpp>

#include <vector>
#include <fstream>
#include <filesystem>

using namespace Plteen;
using namespace WarGrey::CAE;

using namespace std::filesystem;

/*************************************************************************************************/
void WarGrey::CAE::CAEModel::import_from_file(const std::string& path_db) {
    if (exists(path_db) && (!is_directory(path_db))) {
        std::ifstream caein;
        std::string line;
        int offset;

        caein.exceptions(std::ios_base::badbit | std::ios_base::failbit);
        caein.open(path_db);

        this->clear();

        while (std::getline(caein, line)) {
            try {
                if (GradeEntity::match(line, &offset)) {
                    this->register_student_scores(std::make_shared<GradeEntity>(line, offset));
                } else if (StudentEntity::match(line, &offset)) {
                    this->register_student(std::make_shared<StudentEntity>(line, offset), true);
                } else if (ClassEntity::match(line, &offset)) {
                    this->register_class(std::make_shared<ClassEntity>(line, offset), true);
                } else if (DisciplineEntity::match(line, &offset)) {
                    this->register_discipline(std::make_shared<DisciplineEntity>(line, offset), true);
                } else if (SeatEntity::match(line, &offset)) {
                    shared_seat_t seat = std::make_shared<SeatEntity>(line, offset);
                    uint64_t sNo = seat->get_student();

                    if (this->seats.find(sNo) == this->seats.end()) {
                        this->seats[sNo] = seat;
                    }
                }
            } catch (const std::exception& e) {
                printf("%s in line \"%s\"\n", e.what(), line.c_str());
            }
        }

        caein.close();
    }
}

void WarGrey::CAE::CAEModel::export_to_file(const std::string& path_db, bool override_if_exists) {
    if (!exists(path_db) || override_if_exists) {
        std::ofstream caeout;
        
        caeout.exceptions(std::ios_base::badbit | std::ios_base::failbit);
        caeout.open(path_db);

        for (auto& cls : this->classes) {
            caeout << cls.second->to_string() << std::endl;
        }

        for (auto& dis : this->disciplines) {
            caeout << dis.second->to_string() << std::endl;
        }

        for (auto& stu : this->students) {
            caeout << stu.second->to_string() << std::endl;
        }

        for (auto& seat : this->seats) {
            caeout << seat.second->to_string() << std::endl;
        }

        for (auto& ts_score : this->scores) {
            for (auto& dis_score : ts_score.second) {
                for (auto& score : dis_score.second) {
                    caeout << score.second->to_string() << std::endl;
                }
            }
        }
        
        caeout.close();
    }
}

void WarGrey::CAE::CAEModel::export_grade_to_file(const std::string& path_csv, bool override_if_exists) {
    if (!exists(path_csv) || override_if_exists) {
        std::ofstream grdout;
        
        grdout.exceptions(std::ios_base::badbit | std::ios_base::failbit);
        grdout.open(path_csv);
        this->export_grade_to_file(grdout);       
        grdout.close();
    }
}

void WarGrey::CAE::CAEModel::export_grade_to_file(std::ostream& caeout) {
    for (auto& dis : this->disciplines) {
        std::map<uint64_t, std::vector<uint64_t>> timestamps;

        for (auto& stu_score : this->scores) {
            if (this->students.find(stu_score.first) != this->students.end()) {
                uint64_t clsId = this->get_student_class(stu_score.first);

                if (clsId > 0) {
                    if (timestamps.find(clsId) == timestamps.end()) {
                        std::vector<uint64_t> times;

                        this->feed_class_timestamps(clsId, dis.first, times);
                        timestamps[clsId] = times;
                    }

                    caeout << stu_score.first << "," << this->students[stu_score.first]->get_nickname();
                    caeout << "," << this->students[stu_score.first]->get_gender();
                    caeout << "," << this->disciplines[dis.first]->cannonical_name();

                    for (auto& ts : timestamps[clsId]) {
                        caeout << ",";

                        if (stu_score.second.find(ts) != stu_score.second.end()) {
                            auto& dis_score = stu_score.second[ts];

                            if (dis_score.find(dis.first) != dis_score.end()) {
                                caeout << dis_score[dis.first]->get_score();
                            }
                        }
                    }
                    
                    caeout << std::endl;
                }
            }
        }
    }
}

/*************************************************************************************************/
void WarGrey::CAE::CAEModel::clear(bool broadcast) {
    if (broadcast) {
        for (auto& cls : this->classes) {
            this->listener->on_class_deleted(cls.first, cls.second, true);
        }
    
        for (auto& dis : this->disciplines) {
            this->listener->on_discipline_deleted(dis.first, dis.second, true);
        }
    
        for (auto& stu : this->students) {
            this->listener->on_student_deleted(stu.first, stu.second, true);
        }
    }

    this->classes.clear();
    this->disciplines.clear();
    this->students.clear();
    this->seats.clear();
    this->scores.clear();
    this->dis_codes.clear();
}

/**
 * Delete student records that binding classes had been deleted
 * but leaving those binding no class as-is.
 */
void WarGrey::CAE::CAEModel::clear_detached_students() {
    auto it = this->students.begin();

    while (it != this->students.end()) {
        if (this->seats.find(it->first) != this->seats.end()) {
            auto& seat = this->seats[it->first];
            uint64_t clsId = seat->get_class();

            // clsId will never be 0 if there is a seat record.
            if (this->classes.find(clsId) == this->classes.end()) {
                this->listener->on_student_deleted(it->first, it->second, true);
                this->seats.erase(it->first);
                it = this->students.erase(it);
            } else {
                ++ it;
            }
        } else { // no seat ==> no binding class
            ++ it;
        }
    }
}

/**
 * Delete grade records that related students or diciplines had been deleted
 */
void WarGrey::CAE::CAEModel::clear_detached_grades() {
    auto it = this->scores.begin();

    while (it != this->scores.end()) {
        if (this->students.find(it->first) == this->students.end()) {
            it = this->scores.erase(it);
        } else {
            auto& ts_scores = it->second;
            auto ts_it = ts_scores.begin();

            while (ts_it != ts_scores.end()) {
                auto& dis_scores = ts_it->second;
                auto dis_it = dis_scores.begin();

                while (dis_it != dis_scores.end()) {
                    if (this->disciplines.find(dis_it->first) == this->disciplines.end()) {
                        dis_it = dis_scores.erase(dis_it);
                    } else {
                        ++ dis_it;
                    }
                }

                if (dis_scores.empty()) {
                    ts_it = ts_scores.erase(ts_it);
                } else {
                    ++ ts_it;
                }
            }

            if (ts_scores.empty()) {
                it = this->scores.erase(it);
            } else {
                ++ it;
            }
        }
    }
}

void WarGrey::CAE::CAEModel::register_student_scores(shared_grade_t grade) {
    uint64_t sNo = grade->get_student();
    uint64_t ts = grade->get_timestamp();
    uint64_t dis = grade->get_discipline();

    if (this->scores.find(sNo) != this->scores.end()) {
        auto& ts_scores = this->scores[sNo];

        if (ts_scores.find(ts) != ts_scores.end()) {
            auto& dis_scores = ts_scores[ts];

            if (dis_scores.find(dis) == dis_scores.end()) {
                dis_scores[dis] = grade;
            } else {
                if (this->disciplines.find(dis) != this->disciplines.end()) {
                    throw exn_cae("成绩已(%s@%llu)登记", this->disciplines[dis]->cannonical_name(), ts);
                } else {
                    throw exn_cae("成绩已(%llu@%llu)登记", dis, ts);
                }
            }
        } else {
            ts_scores[ts] = { { dis, grade } };
        }
    } else {
        this->scores[sNo] = { { ts, { { dis, grade } }} };
    }
}

void WarGrey::CAE::CAEModel::register_class(shared_class_t cls, bool in_batching) {
    uint64_t pk = cls->primary_key();

    if (this->classes.find(pk) == this->classes.end()) {
        this->classes[pk] = cls;
        this->listener->on_class_created(pk, cls, in_batching);
    } else {
        throw exn_cae("班级(%llu)已存在", cls->primary_key());
    }
}

void WarGrey::CAE::CAEModel::register_discipline(shared_discipline_t dis, bool in_batching) {
    uint64_t pk = dis->primary_key();

    if (this->disciplines.find(pk) == this->disciplines.end()) {
        DisciplineType disType = dis->cannonical_type();

        if (this->dis_codes.find(disType) == this->dis_codes.end()) {
            this->disciplines[pk] = dis;
            this->dis_codes[disType] = pk;
            this->listener->on_discipline_created(pk, dis, in_batching);
        } else {
            throw exn_cae("课程类型(%s)已存在", dis->cannonical_name());
        }
    } else {
        throw exn_cae("课程(%llu)已存在", dis->primary_key());
    }
}

void WarGrey::CAE::CAEModel::register_student(shared_student_t stu, bool in_batching) {
    uint64_t pk = stu->primary_key();

    if (this->students.find(pk) == this->students.end()) {
        this->students[pk] = stu;
        this->listener->on_student_created(pk, stu, in_batching);
    } else {
        throw exn_cae("学号(%llu)已存在", stu->primary_key());
    }
}

/*************************************************************************************************/
void WarGrey::CAE::CAEModel::create_class_from_user_input(const char* text, size_t size) {
    this->register_class(std::make_shared<ClassEntity>(text, 0), false);
}

void WarGrey::CAE::CAEModel::delete_class_as_user_request(uint64_t clsId) {
    if (this->classes.find(clsId) != this->classes.end()) {
        shared_class_t entity = this->classes[clsId];

        this->classes.erase(clsId);
        this->listener->on_class_deleted(clsId, entity, false);
    } else {
        throw exn_cae("查无此班(%llu)", clsId);
    }
}

void WarGrey::CAE::CAEModel::create_discipline_from_user_input(const char* text, size_t size) {
    this->register_discipline(std::make_shared<DisciplineEntity>(text, 0), false);
}

void WarGrey::CAE::CAEModel::delete_discipline_as_user_request(uint64_t disCode) {
    if (this->disciplines.find(disCode) != this->disciplines.end()) {
        shared_discipline_t entity = this->disciplines[disCode];

        this->disciplines.erase(disCode);
        this->dis_codes.erase(entity->cannonical_type());
        this->listener->on_discipline_deleted(disCode, entity, false);
    } else {
        throw exn_cae("查无此课(%llu)", disCode);
    }
}

void WarGrey::CAE::CAEModel::create_student_from_user_input(const char* text, size_t size) {
    this->register_student(std::make_shared<StudentEntity>(text, 0), false);
}

void WarGrey::CAE::CAEModel::update_student_from_user_input(uint64_t sNo, const char* text, size_t size) {
    if (this->students.find(sNo) != this->students.end()) {
        if (this->students[sNo]->update(text, size)) {
            this->listener->on_student_updated(sNo, this->students[sNo]);
        }
    } else {
        throw exn_cae("查无此人(%llu)", sNo);
    }
}

void WarGrey::CAE::CAEModel::update_student_avatar_from_user_input(uint64_t sNo, const char* text, size_t size) {
    if (this->students.find(sNo) != this->students.end()) {
        if (this->students[sNo]->update_avatar_gender(text, size)) {
            this->listener->on_student_avatar_updated(sNo, this->students[sNo]);
        }
    } else {
        throw exn_cae("查无此人(%llu)", sNo);
    }
}

void WarGrey::CAE::CAEModel::delete_student_as_user_request(uint64_t sNo) {
    if (this->students.find(sNo) != this->students.end()) {
        shared_student_t entity = this->students[sNo];

        this->students.erase(sNo);
        this->seats.erase(sNo);
        this->listener->on_student_deleted(sNo, entity, false);
    } else {
        throw exn_cae("查无此人(%llu)", sNo);
    }
}

void WarGrey::CAE::CAEModel::register_student_scores_from_user_input(uint64_t sNo, uint64_t disCode, uint64_t ts, const char* text, size_t size) {
    shared_grade_t grade = std::make_shared<GradeEntity>(sNo, disCode, ts);

    grade->extract_scores(text, size, 0U);
    this->register_student_scores(grade);
}

void WarGrey::CAE::CAEModel::update_student_scores_from_user_input(uint64_t sNo, uint64_t disCode, uint64_t ts, const char* text, size_t size) {
    if (this->scores.find(sNo) != this->scores.end()) {
        auto& ts_scores = this->scores[sNo];

        if (ts_scores.find(ts) != ts_scores.end()) {
            auto& dis_scores = ts_scores[ts];

            if (dis_scores.find(disCode) != dis_scores.end()) {
                dis_scores[disCode]->extract_scores(text, size, 0U);
            } else {
                throw exn_cae("成绩(%s)未登记(%s@%llu)",
                    this->disciplines[disCode]->cannonical_name(),
                    this->students[sNo]->get_nickname().c_str(), ts);
            }
        } else {
            throw exn_cae("成绩未登记(%s@%llu)", this->students[sNo]->get_nickname().c_str(), ts);
        }
    } else {
        throw exn_cae("成绩未登记(%llu)", sNo);
    }
}

void WarGrey::CAE::CAEModel::delete_student_scores_as_user_request(uint64_t sNo, uint64_t disCode, uint64_t ts) {
    if (this->scores.find(sNo) != this->scores.end()) {
        auto& ts_scores = this->scores[sNo];

        if (ts_scores.find(ts) != ts_scores.end()) {
            auto& dis_scores = ts_scores[ts];

            if (dis_scores.find(disCode) != dis_scores.end()) {
                dis_scores.erase(disCode);

                if (dis_scores.empty()) {
                    ts_scores.erase(ts);
                }
            } else {
                throw exn_cae("成绩(%s)未登记(%s@%llu)",
                    this->disciplines[disCode]->cannonical_name(),
                    this->students[sNo]->get_nickname().c_str(), ts);
            }
        } else {
            throw exn_cae("成绩未登记(%s@%llu)", this->students[sNo]->get_nickname().c_str(), ts);
        }
    } else {
        throw exn_cae("成绩未登记(%llu)", sNo);
    }
}

/*************************************************************************************************/
void WarGrey::CAE::CAEModel::bind_student_to_class(uint64_t sNo, uint64_t clsId) {
    if (this->seats.find(sNo) == this->seats.end()) {
        this->seats[sNo] = std::make_shared<SeatEntity>(sNo, clsId);
    } else {
        this->seats[sNo]->set_class(clsId);
    }
}

void WarGrey::CAE::CAEModel::bind_student_to_seat(uint64_t sNo, uint64_t dsk_idx, uint64_t seat_idx) {
    if (this->seats.find(sNo) != this->seats.end()) {
        this->seats[sNo]->set_seat(dsk_idx, seat_idx);
    }
}

uint64_t WarGrey::CAE::CAEModel::get_student_class(uint64_t sNo) {
    uint64_t clsId = 0U;

    if (this->seats.find(sNo) != this->seats.end()) {
        clsId = this->seats[sNo]->get_class();        
    }

    return clsId;
}

uint64_t WarGrey::CAE::CAEModel::get_student_at_seat(uint64_t clsId, uint64_t desk_idx, uint64_t seat_idx) {
    uint64_t sNo = 0U;

    for (auto& seat : this->seats) {
        auto& st = seat.second;

        if ((st->get_class() == clsId)
                && (st->get_desk() == desk_idx)
                && (st->get_seat() == seat_idx)) {
            sNo = seat.first;
            break;
        }
    }

    return sNo;
}

void WarGrey::CAE::CAEModel::feed_student_seat(uint64_t sNo, uint64_t* dsk_idx, uint64_t* st_idx) {
    if (this->seats.find(sNo) != this->seats.end()) {
        SET_BOX(dsk_idx, this->seats[sNo]->get_desk());
        SET_BOX(st_idx, this->seats[sNo]->get_seat());
    } else {
        SET_BOXES(dsk_idx, st_idx, 0.0F);
    }
}

/*************************************************************************************************/
uint64_t WarGrey::CAE::CAEModel::get_discipline_code(DisciplineType type) {
    uint64_t discode = 0U;

    if (this->dis_codes.find(type) != this->dis_codes.end()) {
        discode = this->dis_codes[type];
    }

    return discode;
}

size_t WarGrey::CAE::CAEModel::get_class_population(uint64_t clsId) {
    size_t n = 0U;

    for (auto& st : this->seats) {
        if (st.second->get_class() == clsId) {
            n += 1U;
        }
    }

    return n;
}

uint64_t WarGrey::CAE::CAEModel::get_class_latest_timestamp(uint64_t clsId, size_t offset) {
    uint64_t timestamp = 0U;

    for (auto& st : this->seats) {
        if (st.second->get_class() == clsId) {
            auto& ts_score = this->scores[st.first];

            if (ts_score.size() > offset) {
                auto r_it = ts_score.rbegin();

                while ((offset > 0) && (r_it != ts_score.rend())) {
                    r_it ++;
                    offset --;
                }

                if (offset == 0) { 
                    timestamp = fxmax(timestamp, r_it->first);
                }
            }
        }
    }

    return timestamp;
}

uint64_t WarGrey::CAE::CAEModel::get_student_latest_timestamp(uint64_t sNo, size_t offset) {
    uint64_t timestamp = 0U;

    if (this->scores.find(sNo) != this->scores.end()) {
        auto& ts_score = this->scores[sNo];

        if (ts_score.size() > offset) {
            auto r_it = ts_score.rbegin();

            while ((offset > 0) && (r_it != ts_score.rend())) {
                r_it ++;
                offset --;
            }

            if (offset == 0U) {
                timestamp = r_it->first;
            }
        }
    }

    return timestamp;
}

double WarGrey::CAE::CAEModel::get_class_average_score(uint64_t clsId, uint64_t disCode, uint64_t timestamp) {
    double total = 0.0;
    size_t n = 0U;

    for (auto& stu : this->students) {
        if (this->seats.find(stu.first) != this->seats.end()) {
            if (this->seats[stu.first]->get_class() == clsId) {
                if (this->scores.find(stu.first) != this->scores.end()) {
                    auto& ts_scores = this->scores[stu.first];

                    if (ts_scores.find(timestamp) != ts_scores.end()) {
                        auto& dis_scores = ts_scores[timestamp];

                        if (dis_scores.find(disCode) != dis_scores.end()) {
                            auto score = dis_scores[disCode];

                            n ++;
                            total += score->get_score();
                        }
                    }
                }
            }
        }
    }

    return (n == 0U) ? flnan : total / double(n);
}

double WarGrey::CAE::CAEModel::get_student_score(uint64_t sNo, uint64_t disCode, uint64_t timestamp) {
    std::vector<double> pts;

    this->feed_student_score_points(sNo, disCode, timestamp, pts);

    return vector_sum(pts);
}

void WarGrey::CAE::CAEModel::feed_student_score_points(uint64_t sNo, uint64_t disCode, uint64_t timestamp, std::vector<double>& pts) {
    pts.clear();

    if (this->scores.find(sNo) != this->scores.end()) {
        auto& ts_scores = this->scores[sNo];

        if (ts_scores.find(timestamp) != ts_scores.end()) {
            auto& dis_scores = ts_scores[timestamp];

            if (dis_scores.find(disCode) != dis_scores.end()) {
                auto& s = dis_scores[disCode];

                s->feed_score_points(pts);
            }
        }
    }
}

void WarGrey::CAE::CAEModel::feed_class_timestamps(uint64_t clsId, uint64_t disCode, std::vector<uint64_t>& tss) {
    std::map<uint64_t, bool> timestamps;

    for (auto& stu_score : this->scores) {
        if (this->get_student_class(stu_score.first) == clsId) {
            for (auto& ts_score : stu_score.second) {
                if (timestamps.find(ts_score.first) == timestamps.end()) {
                    if (ts_score.second.find(disCode) != ts_score.second.end()) {
                        timestamps[ts_score.first] = true;
                    }
                }
            }
        }
    }

    tss.clear();
    for (auto& ts : timestamps) {
        tss.push_back(ts.first);
    }
}

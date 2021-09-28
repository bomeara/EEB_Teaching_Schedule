library(targets)

source("functions.R")
source("_packages.R")
options(timeout=1200) # let things download for at least 20 minutes
options(download.file.method = "libcurl")

list(
 tar_target(raw_instructor_prefs, (GetInstructorPreferences(sheet_code="1bCTWCYFRp2g3ftYpAvN0K5qoyibeNXg7llqeQ6nJ2yk"))),
 #tar_target(raw_instructor_prefs, (GetInstructorPreferencesFromDF(as.data.frame(readxl::read_xlsx("~/Downloads/Instructor preferred class times (Responses).xlsx", .name_repair="minimal", col_types="text"))))),
 tar_target(instructor_prefs, FilterUnavailableInstructors(ConvertPrefsToScores(raw_instructor_prefs))),
 tar_target(prefs_by_course, ExpandByCourse(instructor_prefs)),
 tar_target(student_time_prefs, GetOverallStudentTimePrefs(prefs_by_course)),
 tar_target(utk_processed, ProcessUTKRaw()),
 tar_target(dept_classes_2021, FilterForSubject(utk_processed, "Ecology/Evolutionary Biology")),
 tar_target(course_time_similarity, FindSimilarityToPreviousCourses(prefs_by_course, dept_classes_2021)),
 tar_target(many_schedules, ComputeManySchedules(prefs_by_course, student_time_prefs, course_time_similarity)),
 tar_target(export_runs, ExportManySchedules(many_schedules, raw_instructor_prefs, filename="SpringSchedule.xlsx"), format="file")
)

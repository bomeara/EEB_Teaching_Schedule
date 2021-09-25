library(targets)

source("R/functions.R")
source("_packages.R")
options(timeout=1200) # let things download for at least 20 minutes
options(download.file.method = "libcurl")

list(
 tar_target(raw_instructor_prefs, (GetInstructorPreferences())),
 tar_target(instructor_prefs, ConvertPrefsToScores(raw_instructor_prefs)),
 tar_target(prefs_by_course, ExpandByCourse(instructor_prefs)),
 tar_target(many_schedules, ComputeManySchedules(prefs_by_course)),
 tar_target(export_runs, ExportManySchedules(many_schedules, raw_instructor_prefs, filename="data/SpringSchedule.xlsx"), format="file")
)

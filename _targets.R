library(targets)

source("R/functions.R")
source("_packages.R")
options(timeout=1200) # let things download for at least 20 minutes
options(download.file.method = "libcurl")

list(
 tar_target(instructor_prefs, ConvertPrefsToScores(GetInstructorPreferences())),
 tar_target(prefs_by_course, ExpandByCourse(instructor_prefs)),
 
)

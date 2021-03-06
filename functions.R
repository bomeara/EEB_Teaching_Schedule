GetInstructorPreferences <- function(sheet_code="1bCTWCYFRp2g3ftYpAvN0K5qoyibeNXg7llqeQ6nJ2yk") {
 prefs <- as.data.frame(googlesheets4::read_sheet(sheet_code, col_types="c"), stringsAsFactors=FALSE)
 prefs <- prefs[, colSums(is.na(prefs)) != nrow(prefs)] # get rid of NA only cols
 colnames(prefs) <- gsub("I would want to teach a class offered at this time: \\[", "TIME ", colnames(prefs))
 colnames(prefs) <- gsub("\\].*", "", colnames(prefs))
 return(prefs)
}

GetInstructorPreferencesFromDF <- function(prefs) {
 prefs <- prefs[, colSums(is.na(prefs)) != nrow(prefs)] # get rid of NA only cols
 colnames(prefs) <- gsub("I would want to teach a class offered at this time: \\[", "TIME ", colnames(prefs))
 colnames(prefs) <- gsub("\\].*", "", colnames(prefs))
 return(prefs)
}

FilterUnavailableInstructors <- function(instructor_prefs) {
	prefs_times <- instructor_prefs[,grepl("TIME", colnames(instructor_prefs))]
	for (i in sequence(ncol(prefs_times))) {
		prefs_times[,i] <- as.numeric(prefs_times[,i])
	}
	sums <- rowSums(prefs_times)
	if(any(sums == 0)) { 
		print(paste("Warning: instructor(s) ", paste(instructor_prefs$'Your name'[sums==0], collapse=", "), " have entered no available times.", sep=""))	
	}
	instructor_prefs <- instructor_prefs[sums > 0,]
	return(instructor_prefs)
}

GetStudentPreferences <- function(sheet_code="1CvqokuJlTB97VvgkUm8IAhEOTXiFcP8r-UHK2DkYPeA") {
	studentprefs <- as.data.frame(googlesheets4::read_sheet(sheet_code, col_types="c"), stringsAsFactors=FALSE)
	studentprefs[studentprefs == "Strongly disagree"] <- -2
	studentprefs[studentprefs == "Disagree"] <- -1
	studentprefs[studentprefs == "Neutral"] <- 0
	studentprefs[studentprefs == "Agree"] <- 1
	studentprefs[studentprefs == "Strongly Agree"] <- 2
	colnames(studentprefs) <- gsub("I would want to enroll in a class offered at this time: \\[", "TIME ", colnames(studentprefs))
	 colnames(studentprefs) <- gsub("\\].*", "", colnames(studentprefs))


	#apply(studentprefs[grepl("enroll", colnames(studentprefs))], 2, mean, na.rm=TRUE)
	return(studentprefs)
}

ConvertStudentPrefsToTimePrefs <- function(studentprefs) {
	students_times <- studentprefs[, grepl("TIME", colnames(studentprefs))]	
	students_times <- apply(students_times, 2, as.numeric)
	means <- apply(students_times, 2, mean, na.rm=TRUE)
	return(means)
}	

# 4 is best, 0 is worst.
# Assumes missing values are 0.
# min_pref lets you choose to only offer classes that match instructor min_pref or higher
ConvertPrefsToScores <- function(prefs, min_pref=1) {
	prefs[prefs=="Ideal"] <- 4
	prefs[prefs=="Preferred"] <- 3
	prefs[prefs=="Feasible"] <- 2
	prefs[prefs=="Unwelcome but feasible"] <- 1
	prefs[prefs=="Not possible"] <- 0
	prefs[is.na(prefs)] <- 0
	prefs[prefs<min_pref] <- 0
	return(prefs)
}

ExtractCourseNumbers <- function(prefs, prefixes=c("EEB", "BIO")) {
	courses <- toupper(prefs$'What class(es) are you teaching in the Spring? Include course number and name')	
	#courses <- gsub("BIOL", "BIO", gsub("BIO ", "BIO", gsub("EEB ", "EEB", toupper(courses))))
	for (i in sequence(length(prefixes))) {
		courses <- gsub(paste0(prefixes[i], " "), prefixes[i], courses)	
	}
	courses_extracted <- gsub(",*$", "", apply(str_extract_all(courses, "\\w?\\w\\w\\w\\d\\d\\d", simplify=TRUE), 1, paste0, collapse=","))
	return(courses_extracted)
}

ExpandByCourse <- function(scored_prefs, prefixes=c("EEB", "BIO")) {
	scored_prefs$CourseNumber <- ExtractCourseNumbers(scored_prefs, prefixes)
	scored_prefs <- subset(scored_prefs, nchar(scored_prefs$CourseNumber) > 0)
	by_course <- subset(scored_prefs, !grepl(',', scored_prefs$CourseNumber))
	multi_courses <- subset(scored_prefs, grepl(',', scored_prefs$CourseNumber))
	for (i in sequence(nrow(multi_courses))) { 
		course_numbers <- strsplit(multi_courses[i, "CourseNumber"], ",")[[1]]
		for (j in sequence(length(course_numbers))) {
			by_course <- rbind(by_course, multi_courses[i,])
			by_course$CourseNumber[nrow(by_course)] <- course_numbers[j]
		}	
	}
	by_course$CourseNumbers <- NULL
	by_course$'What class(es) are you teaching in the Spring? Include course number and name' <- NULL
	return(by_course)
}

# A which for multidimensional arrays.
# Mark van der Loo 16.09.2011
#
# A Array of booleans
# returns a sum(A) x length(dim(A)) array of multi-indices where A == TRUE
#
multi.which <- function(A){
	if(!any(A)) {return(NULL)} # so it works if no match; added by BCO
    if ( is.vector(A) ) return(which(A))
    d <- dim(A)
    T <- which(A) - 1
    nd <- length(d)
    t( sapply(T, function(t){
        I <- integer(nd)
        I[1] <- t %% d[1]
        sapply(2:nd, function(j){
            I[j] <<- (t %/% prod(d[1:(j-1)])) %% d[j]
        })
        I
    }) + 1 )
}


GetOverallStudentTimePrefs <- function(prefs_by_course) {
	student_prefs <- c(`TIME MWF 8:00-8:50 am` = -0.24, `TIME MWF 2:15-3:05 pm` = 1.28, 
`TIME MWF 3:30-4:20 pm` = 1.08, `TIME MWF 4:45-5:35 pm` = -0.04, 
`TIME MWF 6:00-6:50 pm` = -0.84, `TIME MWF 7:15-8:05 pm` = -1.12, 
`TIME TR 8:10-9:25 am` = 0.52, `TIME TR 2:50-4:05 pm` = 1.2, 
`TIME TR 4:30-5:45 pm` = 0.36, `TIME TR 6:10-7:25 pm` = -0.96, 
`TIME TR 7:50-9:05 pm` = -1.4) # from the student preferences spreadsheet, as of Sept. 25, 2021
	time_names_from_prefs <- colnames(prefs_by_course)[grepl("TIME", colnames(prefs_by_course))]
	time_prefs <- rep(2, length(time_names_from_prefs))
	names(time_prefs) <- time_names_from_prefs
	for (i in seq_along(student_prefs)) {
		time_prefs[names(student_prefs)[i]] <- student_prefs[i]	
	}
	names(time_prefs) <- gsub("TIME ", "", names(time_prefs))

	return(time_prefs)
}


CreateSampleSchedule <- function(prefs_by_course, student_time_prefs, course_time_similarity, maximum_allowed_time_shift=23.9) {
	previous_schedule_emphasis <- runif(1, 1, 4)
	schedule <- data.frame()
	while(nrow(prefs_by_course) > 0) {
		pref_weights <- c()
		all_available <- data.frame()
		for(preference in seq(4, 1, -1)) {
			local_available <- multi.which(prefs_by_course == preference)
			if(!is.null(local_available)) {
				pref_weights <- c(pref_weights, rep(preference, nrow(local_available)))
				all_available <- rbind(all_available, local_available)
			}
		}
		time_differences <- rep(NA, nrow(all_available))
		for (i in sequence(nrow(all_available))) {
			time_differences[i] <- as.numeric(course_time_similarity[all_available[i,1], all_available[i,2]])
		}
		sample_probabilities <- (pref_weights/(time_differences+1))^previous_schedule_emphasis
		sample_probabilities <- sample_probabilities/max(sample_probabilities)
		if(nrow(all_available) == 0) {
			stop("Schedule not feasible with current parameters")	
		}
		chosen_best <- as.numeric(all_available[sample.int(nrow(all_available), size=1, prob=sample_probabilities),]) #idea here is to bias the samples towards courses with the same time
		ntries <- 1
		while(course_time_similarity[chosen_best[1], chosen_best[2]]>maximum_allowed_time_shift && ntries < 10) {
			chosen_best <- as.numeric(all_available[sample.int(nrow(all_available), size=1, prob=sample_probabilities),])
			ntries <- ntries+1	
		}
		local_schedule <- data.frame(Instructor=prefs_by_course$'Your name'[chosen_best[1]], CourseNumber=prefs_by_course$CourseNumber[chosen_best[1]], Time=colnames(prefs_by_course)[chosen_best[2]], LastYearSchedule=course_time_similarity$PreviousDateTime[chosen_best[1]], TimeDifference=round(course_time_similarity[chosen_best[1], chosen_best[2]],1), Preference=prefs_by_course[chosen_best[1], chosen_best[2]])
		try(local_schedule$Email <- prefs_by_course$'Your email address'[chosen_best[1]])
		if(nrow(schedule) == 0) { schedule <- local_schedule } else { schedule <- rbind(schedule, local_schedule) } 
		other_instructor_courses <- which(prefs_by_course$'Your name' == prefs_by_course$'Your name'[chosen_best[1]])
		prefs_by_course[other_instructor_courses, chosen_best[2]] <- 0
		prefs_by_course <- prefs_by_course[-chosen_best[1],]
		course_time_similarity <- course_time_similarity[-chosen_best[1],]
		#print(schedule)
	}
	schedule$PreferenceVerbal <- ""
	schedule$Time <- gsub("TIME ", "", schedule$Time)
	schedule$PreferenceVerbal[which(schedule$Preference == 4)] <- "Ideal"
	schedule$PreferenceVerbal[which(schedule$Preference == 3)] <- "Preferred"
	schedule$PreferenceVerbal[which(schedule$Preference == 2)] <- "Feasible"
	schedule$PreferenceVerbal[which(schedule$Preference == 1)] <- "Unwelcome but feasible"
	schedule$StudentLikertPreference <- student_time_prefs[schedule$Time]
	schedule <- schedule[order(schedule$Time),]
	return(schedule)
}

ComputeScheduleScores <- function(schedule, student_time_prefs) {
	schedule_scores <- data.frame(
		InstructorPref = mean(as.numeric(schedule$Preference)), 
		StudentPref=mean(2+as.numeric(schedule$StudentLikertPreference)), 
		WorstInstructorPref=min(as.numeric(schedule$Preference)), 
		BestInstructorPref=max(as.numeric(schedule$Preference)), 
		WorstStudentPref=min(2+as.numeric(schedule$StudentLikertPreference)), 
		BestStudentPref=max(2+as.numeric(schedule$StudentLikertPreference)), 
		TotalPrime = sum(grepl("PRIME", schedule$Time)), 
		TotalNonPrime = sum(!grepl("PRIME", schedule$Time)),
		BiggestTimeShift = round(max(as.numeric(schedule$TimeDifference)),1),
		MedianTimeShift = round(median(as.numeric(schedule$TimeDifference)),1),
		MeanTimeShift = round(mean(as.numeric(schedule$TimeDifference)),1)
	)
	schedule_scores$ProportionPrime <- schedule_scores$TotalPrime / (schedule_scores$TotalPrime + schedule_scores$TotalNonPrime)
	schedule_scores$MaxAnyOverlap <- max(table(schedule$Time))
	schedule_scores$StudentAveragePref <- mean(student_time_prefs[schedule$Time])
	times <- gsub("TIME ", "", sort(table(schedule$Time), decreasing=TRUE))
	schedule_scores$MostCommonTime <- names(times)[1]
	schedule_scores$StudentPlusInstructorPref <- schedule_scores$InstructorPref + schedule_scores$StudentPref
	schedule_scores$InstructorPercentIdeal <- 100*schedule_scores$InstructorPref / 4
	schedule_scores$StudentPercentIdeal <- 100*schedule_scores$StudentPref / 4
	schedule_scores$StudentPlusInstructorPercentIdeal <- 100*schedule_scores$StudentPlusInstructorPref / 8
	schedule_scores$InstructorPercentPreferredOrBetter <- 100*sum(as.numeric(schedule$Preference) >= 3)/nrow(schedule)
	schedule_scores$StudentPercentWantToEnroll <- 100*sum(as.numeric(schedule$StudentLikertPreference) > 0)/nrow(schedule)
	schedule_scores$StudentAndInstructorPercentPositive <- (schedule_scores$InstructorPercentPreferredOrBetter + schedule_scores$StudentPercentWantToEnroll)/2
	schedule_scores$InstructorsWithUnwelcomeSchedule <- paste(schedule$Instructor[which(schedule$Preference==1)], collapse=", ")
	schedule_scores$NumberOfInstructorsWithUnwelcomeSchedule <- length(unique(schedule$Instructor[which(schedule$Preference==1)]))
	return(schedule_scores)
}

ComputeManySchedules <- function(prefs_by_course, student_time_prefs, course_time_similarity, nrep=50, maximum_proportion_prime=0.7, maximum_allowed_time_shift=23.9) {
	schedules <- list()
	scores <- data.frame()
	while(length(schedules) < nrep) {
		schedule <- CreateSampleSchedule(prefs_by_course, student_time_prefs, course_time_similarity, maximum_allowed_time_shift=maximum_allowed_time_shift)
		scores_local <- ComputeScheduleScores(schedule, student_time_prefs)
		if(scores_local$ProportionPrime <= maximum_proportion_prime & scores_local$BiggestTimeShift < maximum_allowed_time_shift) {
			schedule$Preference <- NULL
			colnames(schedule) <- gsub("PreferenceVerbal", "Instructor_Preference", colnames(schedule))
			schedules[[length(schedules)+1]] <- schedule
			scores <- rbind(scores, scores_local)
		}
	}
	preferred_order <- order(scores$MeanTimeShift, scores$StudentAndInstructorPercentPositive, decreasing=c(FALSE, TRUE))
	schedules <- schedules[preferred_order]
	scores <- scores[preferred_order,]
	scores$scheduleName <- paste("S_", 1:nrow(scores), sep="")
	names(schedules) <- scores$scheduleName
	return(list(schedules=schedules, scores=scores, simple_scores=SimplifyScores(scores)))
}

SimplifyScores <- function(scores) {
	scores_simplified <- data.frame(
		Schedule_Name = scores$scheduleName,
		Average_Percent_Happy_Students_and_Instructors = scores$StudentAndInstructorPercentPositive,
		Percent_Happy_Instructors = scores$InstructorPercentPreferredOrBetter,
		Percent_Happy_Students = scores$StudentPercentWantToEnroll,
		Percent_Prime_Courses = 100*scores$ProportionPrime,
		Worst_Number_Overlapping_Courses = scores$MaxAnyOverlap,
		Number_Of_Instructors_With_Unwelcome_Schedule = scores$NumberOfInstructorsWithUnwelcomeSchedule,
		Biggest_Shift_In_Times = round(scores$BiggestTimeShift,1)
	)
	return(scores_simplified)
}

ExportManySchedules <- function(many_schedules, instructor_prefs, filename, number_to_export=20) {
	all_df <- c(list(instructor_prefs, many_schedules$simple_scores[sequence(number_to_export),], many_schedules$scores[sequence(number_to_export),]), many_schedules$schedules[sequence(number_to_export)])
	names(all_df) <- c("original", "simplified_scores", "raw_scores", names(many_schedules$schedules)[sequence(number_to_export)])
	wb <- openxlsx::write.xlsx(all_df, file=filename, overwrite=TRUE)
	for(i in sequence(length(all_df))) {
		setColWidths(wb, sheet = i, cols = sequence(ncol(all_df[[i]])), widths = "auto")
	}
	saveWorkbook(wb, filename, overwrite = TRUE)
	return(filename)
}

ProcessUTKRaw <- function() {
	utk <- read.csv("data/UTK_Classes_Spring_2021_raw.csv", header=TRUE, stringsAsFactors=FALSE)	
	utk <- subset(utk, nchar(utk$Title)>1)
	utk$Day <- gsub(":.*", "", gsub("\\d", "", gsub("SMTWTFS","", stringr::str_extract(utk$Meeting.Times, ".*SMTWTFS\\d"))))
	utk$Time <- gsub("SMTWTFS", "", gsub(" Type", "", stringr::str_extract(utk$Meeting.Times, "SMTWTFS.*? Type")))
	splittimes <- strsplit(utk$Time, " - ")
	utk$StartTime <- tolower(gsub("  *", " ", unlist(lapply(splittimes, "[", 1))))
	utk$EndTime <- tolower(gsub("  *", " ", unlist(lapply(splittimes, "[", 2))))
	utk$Instructor <- gsub("\\(.*\\)", "", utk$Instructor)
	utk$Instructor <- stringr::str_trim(utk$Instructor)
	utk$Subject.Description <- stringr::str_trim(utk$Subject.Description)
	utk$Course.Number <- stringr::str_trim(utk$Course.Number)
	utk$Instructor[nchar(utk$Instructor)==0] <- " "
	utk$InstructorLastName <- humaniformat::last_name(utk$Instructor)
	utk$InstructorFirstName <- humaniformat::first_name(utk$Instructor)
	utk$DayFormat <- "None"
	utk$DayFormat[grepl("Tuesday", utk$Day)] <- "TR" # doesn't handle weird cases like every day classes, but those are rare
	utk$DayFormat[grepl("Thursday", utk$Day)] <- "TR"
	utk$DayFormat[grepl("Wednesday", utk$Day)] <- "MWF"
	utk$DayFormat[grepl("Monday", utk$Day)] <- "MWF"
	utk$DayFormat[grepl("Friday", utk$Day)] <- "MWF"
	return(utk)
}

FilterForSubject <- function(utk, subject="Ecology/Evolutionary Biology") {
	return(subset(utk, utk$Subject.Description==subject))
}

#returns how many hours different from Spring 2021 (numbers go from zero to positive). A shift between days starts with 24 and gets worse from there.
FindSimilarityToPreviousCourses <- function(prefs_by_course, dept_classes_2021) {
	time_conversion_table <- GetConversionTo24HourTimeTable()
	similarity_to_last_year <- prefs_by_course
	similarity_to_last_year$PreviousDateTime <- "Unknown"
	TIME_columns <- which(grepl("TIME", colnames(similarity_to_last_year)))
	MWF_columns <- which(grepl("TIME MWF", colnames(similarity_to_last_year)))
	TR_columns <- which(grepl("TIME TR", colnames(similarity_to_last_year)))
	for (i in sequence(ncol(similarity_to_last_year))) {
		if(grepl("TIME", colnames(similarity_to_last_year)[i])) {
			similarity_to_last_year[,i] <- 0	
		}
	}
	
	for(row_index in sequence(nrow(similarity_to_last_year))) {
		matching_courses <- subset(dept_classes_2021, grepl(stringr::str_extract( similarity_to_last_year$CourseNumber[row_index], "\\d\\d\\d+"), dept_classes_2021$Course.Number))
		if(nrow(matching_courses)>0) {
			matching_course <- matching_courses[which.min(adist(matching_courses$Instructor, similarity_to_last_year$'Your name'[row_index], ignore.case=TRUE)),]
			dayformat <- matching_course$DayFormat
			if(dayformat=="MWF") {
				similarity_to_last_year[row_index,TR_columns] <- 24
			}
			if(dayformat=="TR") {
				similarity_to_last_year[row_index,MWF_columns] <- 24
			}
			twentyfourtime_raw <- as.numeric(strsplit(gsub(" [ap]m", "", matching_course$StartTime), ":")[[1]])
			twentyfourtime <- twentyfourtime_raw[1] + twentyfourtime_raw[2]/60
			if(grepl("pm", matching_course$StartTime)) {
				twentyfourtime <- twentyfourtime + 12
			}
			for (time_col_index in seq_along(TIME_columns)) {
				chosen_col <- TIME_columns[time_col_index]
				timediff <- abs(twentyfourtime - time_conversion_table$TwentyFourHourTime[which(time_conversion_table$column_name==colnames(similarity_to_last_year)[chosen_col])])
				similarity_to_last_year[row_index, chosen_col] <- similarity_to_last_year[row_index, chosen_col] + timediff
				if(twentyfourtime_raw[1]==0) { #it's one of the 12 am  classes
					similarity_to_last_year[row_index, chosen_col] <- 0
				}
			}
			similarity_to_last_year$PreviousDateTime[row_index] <- gsub("Monday", "M", gsub("Tuesday", "T", gsub("Wednesday", "W", (gsub("Thursday", "R", gsub("Friday", "F", gsub(",", "", gsub("  *", " ", paste0(matching_course$Day, " ", matching_course$Time)))))))))
		}
	}
	return(similarity_to_last_year)
}

GetConversionTo24HourTimeTable <- function() {
	
	return(data.frame(column_name = c("TIME MWF 8:00-8:50 am", "TIME MWF 2:15-3:05 pm", 
	"TIME MWF 3:30-4:20 pm", "TIME MWF 4:45-5:35 pm", "TIME MWF 6:00-6:50 pm", 
	"TIME MWF 7:15-8:05 pm", "TIME TR 8:10-9:25 am", "TIME TR 2:50-4:05 pm", 
	"TIME TR 4:30-5:45 pm", "TIME TR 6:10-7:25 pm", "TIME TR 7:50-9:05 pm", 
	"TIME MWF 9:15-10:05 am PRIME", "TIME MWF 10:30-11:20 am PRIME", 
	"TIME MWF 11:45 am-12:35 pm PRIME", "TIME MWF 1:00-1:50 pm PRIME", 
	"TIME TR 9:50-11:05 am PRIME", "TIME TR 11:30 am-12:45 pm PRIME", 
	"TIME TR 1:10-2:25 pm PRIME"), TwentyFourHourTime = c(8, 14.25, 
	15.5, 16.75, 18,19.25, 8.17, 14.83, 
	16.5, 18.17, 19.83, 
	9.25, 10.5, 
	11.75, 13, 
	9.83, 11.5, 
	13.16)))
}
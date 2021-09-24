GetInstructorPreferences <- function() {
 prefs <- as.data.frame(googlesheets4::read_sheet("1bCTWCYFRp2g3ftYpAvN0K5qoyibeNXg7llqeQ6nJ2yk", col_types="c"), stringsAsFactors=FALSE)
 prefs <- prefs[, colSums(is.na(prefs)) != nrow(prefs)] # get rid of NA only cols
 colnames(prefs) <- gsub("I would want to teach a class offered at this time: \\[", "TIME ", colnames(prefs))
 colnames(prefs) <- gsub("\\].*", "", colnames(prefs))
 return(prefs)
}

# 4 is best, 0 is worst.
# Assumes missing values are 0.
ConvertPrefsToScores <- function(prefs) {
	prefs[prefs=="Ideal"] <- 4
	prefs[prefs=="Preferred"] <- 3
	prefs[prefs=="Feasible"] <- 2
	prefs[prefs=="Unwelcome but feasible"] <- 1
	prefs[prefs=="Not possible"] <- 0
	prefs[is.na(prefs)] <- 0
	return(prefs)
}

ExtractCourseNumbers <- function(prefs) {
	courses <- prefs$'What class(es) are you teaching in the Spring? Include course number and name'	
	courses <- gsub("BIOL", "BIO", gsub("BIO ", "BIO", gsub("EEB ", "EEB", toupper(courses))))
	courses_extracted <- gsub(",*$", "", apply(str_extract_all(courses, "\\w\\w\\w\\d\\d\\d", simplify=TRUE), 1, paste0, collapse=","))
	return(courses_extracted)
}

ExpandByCourse <- function(scored_prefs) {
	scored_prefs$CourseNumber <- ExtractCourseNumbers(scored_prefs)
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
	if(!any(A)) {return(NULL)} # so it works if no match
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

CreateSampleSchedule <- function(prefs_by_course) {
	ExpandByCourse(instructor_prefs) -> prefs_by_course
	schedule <- data.frame()
	while(nrow(prefs_by_course) > 0) {
		weights <- c()
		all_available <- data.frame()
		for(preference in seq(4, 1, -1)) {
			local_available <- multi.which(prefs_by_course == preference)
			if(!is.null(local_available)) {
				weights <- c(weights, rep(preference, nrow(local_available)))
				all_available <- rbind(all_available, local_available)
			}
		}
		chosen_best <- as.numeric(all_available[sample.int(nrow(all_available), size=1, prob=weights^2),])
		local_schedule <- data.frame(Instructor=prefs_by_course$'Your name'[chosen_best[1]], CourseNumber=prefs_by_course$CourseNumber[chosen_best[1]], Time=colnames(prefs_by_course)[chosen_best[2]], Email=prefs_by_course$'Your email address'[chosen_best[1]], Preference=prefs_by_course[chosen_best[1], chosen_best[2]])
		if(nrow(schedule) == 0) { schedule <- local_schedule } else { schedule <- rbind(schedule, local_schedule) } 
		other_instructor_courses <- which(prefs_by_course$'Your name' == prefs_by_course$'Your name'[chosen_best[1]])
		prefs_by_course[other_instructor_courses, chosen_best[2]] <- 0
		prefs_by_course <- prefs_by_course[-chosen_best[1],]
	}
	return(schedule)
}

ComputeScheduleScores <- function(schedule) {
	schedule_scores <- data.frame(TotalPref = sum(as.numeric(schedule$Preference)), TotalPrefSquared = sum((as.numeric(schedule$Preference))^2), WorstPref=min(as.numeric(schedule$Preference)), BestPref=max(as.numeric(schedule$Preference)), TotalPrime = sum(grepl("PRIME", schedule$Time)), TotalNonPrime = sum(!grepl("PRIME", schedule$Time)))
	schedule_scores$ProportionPrime <- schedule_scores$TotalPrime / (schedule_scores$TotalPrime + schedule_scores$TotalNonPrime)
	schedule400s <- schedule[grepl("EEB4", schedule$CourseNumber),]
	schedule_scores$Max400Overlap <- max(table(schedule400s$Time))
	schedule_scores$MaxAnyOverlap <- max(table(schedule$Time))
	times <- sort(table(schedule$Time), decreasing=TRUE)
	schedule_scores$MostCommonTime <- names(times)[1]
	return(schedule_scores)
}
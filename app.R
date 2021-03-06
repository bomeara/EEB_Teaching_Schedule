library(shiny)
library(stringr)
library(openxlsx)
library(readxl)
library(shinybusy)

source("functions.R")

utk_processed<- ProcessUTKRaw()
departments <- sort(unique(utk_processed$Subject.Description))


ui <- fluidPage(
  use_busy_spinner(spin = "fading-circle"),
  sidebarLayout(
    sidebarPanel(
		textInput('prefixes', 'Enter prefixes for your courses (comma delimited, no spaces)', "EEB,BIO"),
		selectInput('department', 'Select a department', departments, "Ecology/Evolutionary Biology"),
      fileInput("file1", "Choose XLSX File",
        accept = c(
          ".xlsx")
        ),
		p("It may take up to a couple of minutes to run the analysis after first uploading."),
	   downloadButton("downloadData", "Download sample schedules"),
		br(),
		hr(),
	  p("This will generate many potential schedules, some much better than others. The more you try, the better they may be, but this app may run more slowly. You can choose to save only the best ones. The initial number is low for speed."),
	  textInput('num_to_try', 'How many schedules to try making', 40),
	  textInput('num_to_export', 'How many of the best schedules to export', 20),
	  p("The app automatically avoids unavailable times for instructors, but allows unwelcome times. There may be schedules that only have times instructors actually like. You can try running this with the 'Feasible' or 'Preferred' settings. If there are no such possible schedules, you will see an error -- just choose a different setting and it will run again."),
radioButtons("min_pref", "Instructors will only be assigned courses that meet at least this level of preference:",
		c("Unwelcome but feasible" = 1,
			"Feasible" = 2,
			"Preferred" = 3,
			"Ideal" = 4), selected = 1),
		p('The current goal is to have no more than 70% of classes during "prime time." By default the app tries even fewer during prime time, but you can adjust this up to the max with the slider below.'),
	   sliderInput(inputId = "threshold",
                  label = "Maximum percentage of courses offered during 'prime time':",
                  min = 40,
                  max = 70,
                  value = 60)
	
),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
 output$downloadData <- downloadHandler(
	filename = function() {
      "SampleSchedule.xlsx"
    },
    content = function(file) {
 		ExportManySchedules(many_schedules, raw_instructor_prefs, filename=file, number_to_export=as.numeric(input$num_to_export))
    },
	contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
		inFile <- input$file1
		prefs_df <- as.data.frame(readxl::read_xlsx(inFile$datapath, .name_repair="minimal", col_types="text"))
		raw_instructor_prefs <<- GetInstructorPreferencesFromDF(prefs_df)
		instructor_prefs <- FilterUnavailableInstructors(ConvertPrefsToScores(raw_instructor_prefs, min_pref=input$min_pref))
		prefs_by_course <- ExpandByCourse(instructor_prefs, prefixes=strsplit(toupper(gsub(" ", "",input$prefixes)), ",")[[1]])
		student_time_prefs <- GetOverallStudentTimePrefs(prefs_by_course)
		dept_classes_2021 <- FilterForSubject(utk_processed, input$department)
		course_time_similarity <- FindSimilarityToPreviousCourses(prefs_by_course, dept_classes_2021)

		many_schedules <<- ComputeManySchedules(prefs_by_course, student_time_prefs,course_time_similarity, maximum_proportion_prime=input$threshold, nrep=as.numeric(input$num_to_try))
		simple <- many_schedules$simple_score 
		colnames(simple) <- gsub("_", " ", colnames(simple))
		simple
  })
}

shinyApp(ui, server)

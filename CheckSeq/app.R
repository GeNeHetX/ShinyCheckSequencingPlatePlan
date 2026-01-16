#====================
# Initialization
#====================

# Required packages
pkgs <- c("shiny","shinydashboard", "DT", "DBI", "RPostgres", "readxl", "shinyjs", "ssh")

# Import required packages
for (p in pkgs) {
  library(p, character.only = TRUE)
}

# Connection to the Serge database to allow checking existing files
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "serge",
  host     = "10.93.244.33",
  port     = 5432,
  user     = "user_read_only",
  password = "inserm"
)

# Constants used to define the styles of the messages rendered
ERROR_STYLE <- "color:#B20000; font-size:18px;"
SUCCESS_STYLE <- "color:#00B200; font-size:18px;"

# Used to avoid error on file upload size
options(shiny.maxRequestSize = -1)

#===================
# Helper Functions
#===================

# checks if the chosen excel file is the right one by checking the sheets
checkIfRightExcel <- function(pathToExcel){

  # Gets only the sheets from the excel file
  sheets <- excel_sheets(pathToExcel)

  # Creates vector of booleans for the results of the checks performed
  errors <- c(
    hasWrongNumberOfSheets = (length(sheets) != 3),
    isSheetNucleicAcidMissing  = !("nucleic_acid" %in% sheets),
    isSheetPlateplanMissing    = !("plateplan" %in% sheets),
    isSheetRefModalitiesMissing = !("RefModalities" %in% sheets)
  )

  errors
}

# Create error message to be rendered in case of wrong excel file chosen
getErrorMessageWrongExcelSheet <- function(errors){

  errorMessageLines <- NULL

  # Error line for an excel file with the wrong number of sheets if needed
  if (errors["hasWrongNumberOfSheets"])
    errorMessageLines <- tagList(errorMessageLines, span("Error: File must have exactly 3 sheets.", style= ERROR_STYLE), br())

  # Error line specifying the missing sheets if needed
  if (any(errors[c("isSheetNucleicAcidMissing", "isSheetPlateplanMissing", "isSheetRefModalitiesMissing")])) {

    missingSheets <- c()
    if (errors["isSheetNucleicAcidMissing"]){ missingSheets <- c(missingSheets, "nucleic_acid") }
    if (errors["isSheetPlateplanMissing"]){ missingSheets <- c(missingSheets,  "plateplan") }
    if (errors["isSheetRefModalitiesMissing"]){ missingSheets <- c(missingSheets, "RefModalities") }

    errorMessageLines <- tagList(
      errorMessageLines,
      span(
        paste0(
          "Error: Missing sheets (",
          paste(
            missingSheets,
            collapse = ", "
          ),
          ")"
        ),
        style= ERROR_STYLE
      ),
      br()
    )
  }

  # Error lines combined if multiple
  errorMessageLines
}

# Activates the submit button if a correct excel file has been selected and deactivates it if not
toggleSubmitButton <- function(errors, submitButtonTag){
  if (any(errors)) {
    disable(submitButtonTag)
  } else {
    enable(submitButtonTag)
  }
}

# Returns a slightly formated version of the data in the nucleic-acid sheet of the excel file as a dataframe
readNucleicAcidSheet <- function(pathToExcel){
  df <- read_excel(pathToExcel, sheet = "nucleic_acid")
  names(df)[1] <- "Line_Number"
  df
}

# For each ID in the column, checks if it has the right nomenclature and returns a boolean
checkNomenclatureID <- function(nucleicAcidIDColumn){
  grepl("^[^a-zA-Z]",nucleicAcidIDColumn) | grepl("[^a-zA-Z0-9_]",nucleicAcidIDColumn)
}

# For each ID in the column, checks if there is another occurence of it. (Second check with from last needed to get all occurences not all-1)
checkDuplicatedID <- function(nucleicAcidIDColumn){
  duplicated(nucleicAcidIDColumn) | duplicated(nucleicAcidIDColumn, fromLast = TRUE)
}

# Looks for corresponding files in the database for each sample in the dataframe and increments the current progress bar if incrementSize is provided
# /!\ Do not provide incrementSize when calling the function outside of a withProgress function
getFilesInSerge <- function(df, con, incrementSize = NULL) {

  rows <- lapply(seq_len(nrow(df)), function(i) {

    queryResults <- dbGetQuery(
      con,
      paste0(
        "SELECT filename FROM rawfile WHERE filename ILIKE '",
        df$ID_NucleicAcid[i],
        "_R_.fastq.gz' AND acquisition = '",
        df$Project[i],
        "' AND is_current = 'TRUE';"
      )
    )

    # Progress update
    if (!is.null(incrementSize)) incProgress(incrementSize)

    # If no files → return NULL
    if (nrow(queryResults) == 0) {
      return(NULL)
    }

    # Otherwise return ONE data frame row
    data.frame(
      Line_Number   = df$Line_Number[i],
      ID_NucleicAcid = df$ID_NucleicAcid[i],
      Project       = df$Project[i],
      Filenames     = paste(queryResults$filename, collapse = " | "),
      stringsAsFactors = FALSE
    )
  })

  # Remove NULLs and bind once
  dfFilesInSerge <- do.call(rbind, rows)

  if (is.null(dfFilesInSerge)) {
    dfFilesInSerge <- data.frame(
      Line_Number    = integer(),
      ID_NucleicAcid = character(),
      Project        = character(),
      Filenames      = character(),
      stringsAsFactors = FALSE
    )
  }

  dfFilesInSerge
}

getUniqueProjectsInSerge <- function(df, con){

  dbGetQuery(
    con,
    "SELECT DISTINCT acquisition FROM rawfile WHERE is_current = 'TRUE';",
  )

}

checkIfProjectMissingFromSerge <- function(df, projectsInSerge, incrementSize = NULL){

  rows <- lapply(seq_len(nrow(df)), function(i) {

    # Progress update
    if (!is.null(incrementSize)) incProgress(incrementSize)

    # If no files → return NULL
    if (df$Project[i] %in% projectsInSerge) {
      data.frame(
        Line_Number   = df$Line_Number[i],
        ID_NucleicAcid = df$ID_NucleicAcid[i],
        Project       = df$Project[i],
        stringsAsFactors = FALSE
      )
    }else{
      return(NULL)
    }

    # Otherwise return ONE data frame row
    
  })

  # Remove NULLs and bind once
  dfFilesWithProjectMissingFromSerge <- do.call(rbind, rows)

  if (is.null(dfFilesWithProjectMissingFromSerge)) {
    dfFilesWithProjectMissingFromSerge <- data.frame(
      Line_Number    = integer(),
      ID_NucleicAcid = character(),
      Project        = character(),
      stringsAsFactors = FALSE
    )
  }

  dfFilesWithProjectMissingFromSerge
}

# Factory function for data tables rendered in the results
createFormattedDataTable <- function(df, colnames, coloredColumn){
  datatable(
    df,
    options = list(
      scrollX = TRUE, 
      dom = "Brtip", 
      buttons = list(
        list(
          extend = "copy",
          title = NULL   # prevents copying the table title
        ),
        list(
          extend = "csv",
          filename = "my_custom_csv_name",
          title = NULL
        ),
        list(
          extend = "excel",
          filename = "my_custom_excel_name",
          title = NULL
        )
      )
    ),
    extensions = "Buttons",
    rownames = FALSE,
    colnames = colnames
  ) |> formatStyle(columns = coloredColumn, backgroundColor = "#B20000", color = "white")
}

# Factory function for the UI sections rendered in the results
createCheckResultSection <- function(df, colnames ,coloredColumn, errorMessage, successMessage){

  # Defines the table to render containing the lines of the excel with a duplicated NucleicAcid_ID if there are
  if (nrow(df) > 0) {
    dtToBeRendered <- createFormattedDataTable(
      df,
      colnames,
      coloredColumn
    )

    uiSection <- tagList(
      p(errorMessage, style = ERROR_STYLE),
      dtToBeRendered
    )
  } else {
    uiSection <- p(successMessage, style = SUCCESS_STYLE)
  }
  
  # Defines UI elements to render for the nomenclature check section of the results
  tagList(
    uiSection,
    br()
  )

}

#==================
# Shiny App Setup
#==================

# Define UI for app
ui <- dashboardPage(
  skin = "purple",
  # App title
  title = "Check Sequencing",
  dashboardHeader(
    title = "Check Sequencing",
    titleWidth=300
  ),
  # Sidebar panel for menu
  dashboardSidebar(
    title = h3("Steps", style = "padding-left : 5% ;"),
    width = 300,
    sidebarMenu(
      menuItem(span("Pre-sequencing", style = "padding-left : 5% ;"),
        tabName = "Pre-sequencing",
        icon = icon("arrow-right-to-bracket")
      ),
      menuItem(span("Post-sequencing", style = "padding-left : 5% ;"),
        tabName = "Post-sequencing",
        icon = icon("arrow-right-from-bracket")
      )
    )
  ),
  dashboardBody(
    tags$style(HTML("
      div.dt-buttons {
        float: right;   /* Moves buttons to the right */
      }
    ")),
    # Allows enabling and disabling of the action button
    useShinyjs(),
    # Tabs chosen via the sidebar menu
    tabItems(
      # UI for section allowing to check excel file before sending samples for sequencing
      tabItem(
        tabName = "Pre-sequencing",
        h1("Pre-sequencing"),
        box(
          width = "auto",
          fileInput(
            "samplesInfoTablePreSeq",
            "Select Excel files containing info about samples to be sequenced",
            width = "auto",
            accept = c(".xls",".xlsx")
          ),
          uiOutput("SamplesPreSeq")
        ),
        actionButton(
          "submitPreSeq",
          "Submit",
          style = "background-color : #605ca8 ; color : white "
        ),
        uiOutput("resultsBoxPre")
      ),

      # UI section allowing to check files coming back from sequencing using the previously sent excel file
      tabItem(
        tabName = "Post-sequencing",
        h1("Post-sequencing"),
        box(
          width = "auto",
          textInput(
            "rawfilesDirPathPostSeq",
            "Enter path to directory containing the raw files to check on IFB cluster",
            width = "auto",
          ),
          fileInput(
            "samplesInfoTablePostSeq",
            "Select Excel file containing info about checked sequenced samples",
            width = "auto",
            accept = c(".xls",".xlsx")
          ),
          uiOutput("SamplesPostSeq")
        ),
        actionButton(
          "submitPostSeq",
          "Submit",
          style = "background-color : #605ca8 ; color : white "
        ),
        uiOutput("resultsBoxPost")
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Disables submit butoons for both tabs
  disable("submitPreSeq")
  disable("submitPostSeq")

  # Checks if the provided excel file is the right one for the pre-sequencing step
  validateExcelPre <- reactive({
    req(input$samplesInfoTablePreSeq)
    checkIfRightExcel(input$samplesInfoTablePreSeq$datapath)
  })

  # TODO: Check columns instead
  # Checks if the provided excel file is the right one for the post-sequencing step
  validateExcelPost <- reactive({
    req(input$samplesInfoTablePostSeq)
    checkIfRightExcel(input$samplesInfoTablePostSeq$datapath)
  })


  # Checks if the excel file is provided and is the right one before enabling the submit button
  observe({
    req(input$samplesInfoTablePreSeq)
    toggleSubmitButton(validateExcelPre(), "submitPreSeq")
  })

  observe({
    req(input$samplesInfoTablePostSeq)
    toggleSubmitButton(validateExcelPost(), "submitPostSeq")
  })


  # Renders either the error messages if the excel file provided is not the right one or the UI element containing the summary table if it is
  output$SamplesPreSeq <- renderUI({
    req(input$samplesInfoTablePreSeq)

    errors <- validateExcelPre()

    if (any(errors)) {
      getErrorMessageWrongExcelSheet(errors)
    } else {
      DTOutput("summaryTablePreSeq")
    }
  })

  output$SamplesPostSeq <- renderUI({
    req(input$samplesInfoTablePostSeq)

    errors <- validateExcelPost()

    if (any(errors)) {
      getErrorMessageWrongExcelSheet(errors)
    } else {
      DTOutput("summaryTablePostSeq")
    }
  })

  # Data stored in a reactive so it will be rendered in the summary data table only if the data exists
  dataSummaryTablePreSeq <- reactive({
    req(input$samplesInfoTablePreSeq, !(any(validateExcelPre())))
    readNucleicAcidSheet(input$samplesInfoTablePreSeq$datapath)
  })

  # Renders the summary table itself
  output$summaryTablePreSeq <- renderDataTable({
    datatable(dataSummaryTablePreSeq(), options = list(scrollX = TRUE, dom = "rtip"), rownames = FALSE) 
  })

  # Data stored in a reactive so it will be rendered in the summary data table only if the data exists
  dataSummaryTablePostSeq <- reactive({
    req(input$samplesInfoTablePostSeq, !(any(validateExcelPost())))
    readNucleicAcidSheet(input$samplesInfoTablePostSeq$datapath)
  })

  # Renders the summary table itself
  output$summaryTablePostSeq <- renderDataTable({
    datatable(dataSummaryTablePostSeq(), options = list(scrollX = TRUE, dom = "rtip"), rownames = FALSE) 
  })

  # Setting up reactive values that will contain the data for the UI elements to render after pressing the submit buttons
  resultsPre <- reactiveVal(NULL)
  resultsPost <- reactiveVal(NULL)

  # Actions to perform when pressing the submit button for the pre-sequencing step
  observeEvent(input$submitPreSeq, {
    # Defines a progress bar for the wait after the submit button press
    withProgress(message = "Checking excel file ...", value = 0, {

      # Imports the nucleic_acid sheet of the excel file
      dataNucleicAcidSheetTablePreSeq <- readNucleicAcidSheet(input$samplesInfoTablePreSeq$datapath)

      # Defines the real number of samples in the excel file used for the incrementation of the progress bar
      nbSamples <- length(dataNucleicAcidSheetTablePreSeq$ID_NucleicAcid)

      incrementSize <- 1/(nbSamples+4)

      incProgress(incrementSize)

      # Dataframe subset generated by checking the nomenclature of the nucleic acid IDs. (Only letters, numbers or underscore allowed and only letters as the first character)
      wrongNomenclatureRows <- dataNucleicAcidSheetTablePreSeq[checkNomenclatureID(dataNucleicAcidSheetTablePreSeq$ID_NucleicAcid), ]

      checkNomenclatureSection <- createCheckResultSection(
        wrongNomenclatureRows,
        names(wrongNomenclatureRows),
        "ID_NucleicAcid",
        "Nucleic Acid IDs do not follow the right nomenclature for the following sample(s).",
        "All Nucleic Acid IDs follow the right nomenclature."
      )

      incProgress(incrementSize)

      # Dataframe subset generated by checking if the nucleic acid ID is duplicated in the table. (from the beginning and from the end to get the first occurence too)
      duplicatedNucleicAcidIDs <- dataNucleicAcidSheetTablePreSeq[checkDuplicatedID(dataNucleicAcidSheetTablePreSeq$ID_NucleicAcid), ]

      checkDuplicatesSection <- createCheckResultSection(
        duplicatedNucleicAcidIDs,
        names(duplicatedNucleicAcidIDs),
        "ID_NucleicAcid",
        "Duplicate Nucleic Acid IDs found for the following samples.",
        "All samples have unique Nucleic Acid IDs."
      )

      incProgress(incrementSize)

      # Defines a dataframe that will be used to store the names of the rawfiles registered in the Serge database for each NucleicAcid_ID if there are any
      dfFilesInSergeForSample <- getFilesInSerge(dataNucleicAcidSheetTablePreSeq, con, incrementSize)

      checkAlreadyInSergeSection <- createCheckResultSection(
        dfFilesInSergeForSample, 
        c("Line_Number", "ID_NucleicAcid", "Project", "Filenames"),
        "Filenames",
        tagList(
          "Following files found already existing in Serge database for provided samples.",
          br(), 
          "(Please add \"_redo\" if the sample has to be resequenced or choose a different ID.)"
        ),
        "All samples are new."
      )

      projectsInSerge <- getUniqueProjectsInSerge(dataNucleicAcidSheetTablePreSeq, con)

      # Defines a dataframe that will be used to store the names of the rawfiles registered in the Serge database for each NucleicAcid_ID if there are any
      dfSamplesWithMissingProjectInSerge <- checkIfProjectMissingFromSerge(dataNucleicAcidSheetTablePreSeq, projectsInSerge, incrementSize)

      checkProjectNotInSergeSection <- createCheckResultSection(
        dfSamplesWithMissingProjectInSerge, 
        names(dfSamplesWithMissingProjectInSerge),
        "Project",
        "Projects of following files are missing from Serge database for provided samples (check if new projects).",
        "All projects exist."
      )


      # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
      checkSamplesSection <- tagList(checkNomenclatureSection, checkDuplicatesSection, checkAlreadyInSergeSection, checkProjectNotInSergeSection)

      #Returns all the sections to render if the excel sheets are right if not only the excel sheet check section
      resultsPre(
        tagList(
            checkSamplesSection
        )
      )

      # Increments the progress bar
      incProgress(incrementSize)
    })
  })


  observeEvent(input$submitPostSeq, {

    showModal(modalDialog(
      h3("Sign-in to IFB account", style = "color : #605ca8 ; "),
      textInput("sshUsernameInput", "Username"),
      passwordInput("sshPasswordInput", "Password"),
      actionButton("signInPostSeq", "Sign-in", style = "background-color : #605ca8 ; color : white ")
    ))

  })

  sshUsername <- reactiveVal(NULL)
  sshPassword <- reactiveVal(NULL)

  # Actions to perform when pressing the submit button for the post-sequencing step
  observeEvent(input$signInPostSeq, {

    removeModal()

    # Defines a progress bar for the wait after the submit button press
    withProgress(message = "Checking Excel file ...", value = 0, {

      incrementSize <- 1

      sshUsername(input$sshUsernameInput)
      sshPassword(input$sshPasswordInput)

      sshHost = paste(sshUsername(),"core.cluster.france-bioinformatique.fr", sep = "@")

      sshSession <- tryCatch({
        ssh_connect(sshHost, passwd = sshPassword())
        },
        error = function(e) {
          message("SSH authentication failed: ", e$message)
          return(NULL)
        }
      )

      if(!(is.null(sshSession))){

        resultCheckRemoteDir <- rawToChar(ssh_exec_internal(sshSession, sprintf("[ -d '%s' ] && echo yes || echo no", input$rawfilesDirPathPostSeq))$stdout)

        isRemoteDirPathRight <- grepl("yes", resultCheckRemoteDir)

        if(isRemoteDirPathRight){

          # Imports the nucleic_acid sheet of the excel file
          dataNucleicAcidSheetTablePostSeq <- readNucleicAcidSheet(input$samplesInfoTablePostSeq$datapath)

          # Defines the real number of samples in the excel file used for the incrementation of the progress bar
          nbSamples <- length(dataNucleicAcidSheetTablePostSeq$ID_NucleicAcid)

          incrementSize <- 1/(nbSamples*2+4)

          incProgress(incrementSize)

          # Dataframe subset generated by checking the nomenclature of the nucleic acid IDs. (Only letters, numbers or underscore allowed and only letters as the first character)
          wrongNomenclatureRows <- dataNucleicAcidSheetTablePostSeq[checkNomenclatureID(dataNucleicAcidSheetTablePostSeq$ID_NucleicAcid), ]

          checkNomenclatureSection <- createCheckResultSection(
            wrongNomenclatureRows,
            names(wrongNomenclatureRows),
            "ID_NucleicAcid",
            "Nucleic Acid IDs do not follow the right nomenclature for the following sample(s).",
            "All Nucleic Acid IDs follow the right nomenclature."
          )

          incProgress(incrementSize)

          # Dataframe subset generated by checking if the nucleic acid ID is duplicated in the table. (from the beginning and from the end to get the first occurence too)
          duplicatedNucleicAcidIDs <- dataNucleicAcidSheetTablePostSeq[checkDuplicatedID(dataNucleicAcidSheetTablePostSeq$ID_NucleicAcid), ]

          checkDuplicatesSection <- createCheckResultSection(
            duplicatedNucleicAcidIDs,
            names(duplicatedNucleicAcidIDs),
            "ID_NucleicAcid",
            "Duplicate Nucleic Acid IDs found for the following samples.",
            "All samples have unique Nucleic Acid IDs."
          )

          incProgress(incrementSize)

          # Defines a dataframe that will be used to store the names of the rawfiles registered in the Serge database for each NucleicAcid_ID if there are any
          dfFilesInSergeForSample <- getFilesInSerge(dataNucleicAcidSheetTablePostSeq, con, incrementSize)

          checkAlreadyInSergeSection <- createCheckResultSection(
            dfFilesInSergeForSample, 
            c("Line_Number", "ID_NucleicAcid", "Project", "Filenames"),
            "Filenames",
            tagList(
              "Following files found already existing in Serge database for provided samples.",
              br(), 
              "(Please add \"_redo\" if the sample has to be resequenced or choose a different ID.)"
            ),
            "All samples are new."
          )

          projectsInSerge <- getUniqueProjectsInSerge(dataNucleicAcidSheetTablePostSeq, con)

          # Defines a dataframe that will be used to store the names of the rawfiles registered in the Serge database for each NucleicAcid_ID if there are any
          dfSamplesWithMissingProjectInSerge <- checkIfProjectMissingFromSerge(dataNucleicAcidSheetTablePostSeq, projectsInSerge, incrementSize)

          checkProjectNotInSergeSection <- createCheckResultSection(
            dfSamplesWithMissingProjectInSerge, 
            names(dfSamplesWithMissingProjectInSerge),
            "Project",
            "Projects of following files are missing from Serge database for provided samples (check if new projects).",
            "All projects exist."
          )

          hasNoFiles <- logical()

          # Sends a query to the Serge database and adds the needed data to the dataframe mentionned above for each NucleicAcid_ID if there is any data returned 
          for (i in seq_len(nrow(dataNucleicAcidSheetTablePostSeq))){

            filesCheckResults <- tryCatch({
              rawToChar(ssh_exec_internal(sshSession, sprintf( "ls %s/%s_*.fastq*",input$rawfilesDirPathPostSeq, dataNucleicAcidSheetTablePostSeq[i,"ID_NucleicAcid"]))$stdout)
              },
              error = function(e) {
                message("File not found: ", e$message)
                return(NULL)
              }
            )
            
            hasNoFiles <- c(hasNoFiles, is.null(filesCheckResults))

            # Increments the progress bar
            incProgress(incrementSize)
          }

          dfFilesNotInIFB <- dataNucleicAcidSheetTablePostSeq[hasNoFiles, ]

          checkFilesInIFBSection <- createCheckResultSection(
            dfFilesNotInIFB, 
            names(dfFilesNotInIFB),
            "ID_NucleicAcid",
            "Files not found in provided IFB directory for the following sample(s).",
            "Files found for all samples."
          )

          # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
          checkSamplesSection <- tagList(checkNomenclatureSection, checkDuplicatesSection, checkAlreadyInSergeSection, checkProjectNotInSergeSection, checkFilesInIFBSection)

        }

        ssh_disconnect(sshSession)

      }

      #Returns all the sections to render if the excel sheets are right if not only the excel sheet check section
      resultsPost(
        if (is.null(sshSession)){
          HTML("<b style='color:red; font-size:18px'>Error: Authentification failed.</b>")
        } else if (!(isRemoteDirPathRight)) {
          HTML("<b style='color:red; font-size:18px'>Error: Directory not found.</b>")
        } else {
          tagList(
            checkSamplesSection
          )
        }
      )

      # Increments the progress bar
      incProgress(incrementSize)
    })

  })



  # Render the results section after pressing the submit button for the each tab

  output$resultsBoxPre <- renderUI({
    req(resultsPre())
    div(style = "margin-top : 20px;",
      box(width = "auto", h2("Validation Report"), resultsPre())
    )
  })

  output$resultsBoxPost <- renderUI({
    req(resultsPost())
    div(style = "margin-top : 20px;",
      box(width = "auto", h2("Validation Report"), resultsPost())
    )
  })


}

shinyApp(ui = ui, server = server)
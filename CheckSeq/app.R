# TODO :
# Check if right excel by comparing all the content of the sheets to the excel template online (link to the template in the procedure)
# Check if added values in RefModalities have the right nomenclature
#(First character = letter, only letters, numbers, _and -, no spaces)
# Files names in "ID_scanSVS" and "ID_annotationXML" for the Beaujon version of the template must be identical and have the right extension


#====================
# Initialization
#====================

# Required packages
pkgs <- c("shiny","shinydashboard", "DT", "DBI", "RPostgres", "readxl", "shinyjs", "ssh", "httr")

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
ERROR_COLOR <- "#B20000"
ERROR_STYLE <- paste0("color:", ERROR_COLOR, "; font-size:18px;")

SUCCESS_COLOR <- "#00B200"
SUCCESS_STYLE <- paste0("color:", SUCCESS_COLOR, "; font-size:18px;")

WARNING_COLOR <- "#FF8800"
WARNING_STYLE <- paste0("color:", WARNING_COLOR, "; font-size:18px;")

# Links of procedure document and template excels online
# /!\ Change links if need be when procedure or templates are updated and keep the "&download=1" for the download to function correctly (& if already an argument passed after ? and ? if first argument in the link)
LINK_PROCEDURE <- "https://insermfrance-my.sharepoint.com/:w:/g/personal/diana_mendes_inserm_fr/IQCp1XhWSPg9RbJAnDkLJ1sZAaRnWzzg4NutQvwCjbgw32Y?rtime=Wb4USN1Y3kg"
LINK_TEMPLATE_BEAUJON <- paste0("https://insermfrance-my.sharepoint.com/:x:/g/personal/diana_mendes_inserm_fr/EY7pQMaGMnZCgokPQDgbbDwBSoT_OJEHfx473bbap1oPtA?e=zwXRao","&download=1")
LINK_TEMPLATE_EXT <- paste0("https://insermfrance-my.sharepoint.com/:x:/g/personal/diana_mendes_inserm_fr/EbTlTtY0vwNFvWhwVduU_q0BDNav3LyoAbX9-8f90GH9Bw?e=hh6pbT","&download=1")

# Constants used to define the right column names of dataframes
# TODO : Update when templates are changed or get names directly from online templates
CORE_TEMPLATE_NUCLEIC_ACID_COLNAMES <- c("ID_NucleicAcid",	"Platewell",	"ng",	"microL",	"ID_Sample",	"Project",	"Species",	"StorageBeforeExtraction",	"SampleFrom",	"RnaExtractedFrom",	"ExtractionMethod")
BEAUJON_TEMPLATE_EXTRA_NUCLEIC_ACID_COLNAMES <- c("ID_layer", "ID_scanSVS", "ID_annotationXML")
CORE_TEMPLATE_PLATEPLAN_COLNAMES <- c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10", "11", "12")
CORE_TEMPLATE_PLATEPLAN_ROWNAMES <- c("A", "B", "C", "D", "E", "F", "G", "H")
CORE_TEMPLATE_REFMODALITIES_COLNAMES <- c("Species",	"Project",	"StorageBeforeExtraction",	"RnaExtractedFrom",	"SampleFrom",	"ExtractionMethod")

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

  errors <- c(
    errors,
    isSheetNucleicAcidWrong = if (errors["isSheetNucleicAcidMissing"]) FALSE else checkIfWrongSheet(readNucleicAcidSheet(pathToExcel), templateColnames = CORE_TEMPLATE_NUCLEIC_ACID_COLNAMES),
    isSheetPlateplanWrong = if (errors["isSheetPlateplanMissing"]) FALSE else checkIfWrongSheet(readPlateplanSheet(pathToExcel), templateColnames = CORE_TEMPLATE_PLATEPLAN_COLNAMES, templateRownames = CORE_TEMPLATE_PLATEPLAN_ROWNAMES),
    isSheetRefModalitiesWrong = if (errors["isSheetRefModalitiesMissing"]) FALSE else checkIfWrongSheet(readRefModalitiesSheet(pathToExcel), templateColnames = CORE_TEMPLATE_REFMODALITIES_COLNAMES)
  )

  errors <- c(
    errors,
    isColumnPlatewellWrong = FALSE
  )

  if (!(errors["isSheetNucleicAcidMissing"]) && !(errors["isSheetNucleicAcidWrong"])) {
    templateTempfile <- tempfile(fileext = ".xlsx")

    GET(
      LINK_TEMPLATE_EXT,
      write_disk(templateTempfile, overwrite = TRUE)
    )

    dfTemplateNucleicAcidSheet <- readNucleicAcidSheet(templateTempfile)
    dfCheckedNucleicAcidSheet <- readNucleicAcidSheet(pathToExcel)
  
    errors["isColumnPlatewellWrong"] = !(identical(dfTemplateNucleicAcidSheet$Platewell, dfCheckedNucleicAcidSheet$Platewell))

  }

  errors
}

# Checks if the names of the columns and/or the rows in the sheet matches its corresponding names to check if the sheet is the correct one.
checkIfWrongSheet <- function(df, templateColnames = NULL, templateRownames = NULL){
  
  areCoreColnamesMissing <- NULL
  areCoreRownamesMissing <- NULL

  if (!(is.null(templateColnames))){
    sheetColnames <- colnames(df)
    areCoreColnamesMissing <- vapply(templateColnames, function(i) (!(i %in% sheetColnames)), logical(1))
  }

  if (!(is.null(templateRownames))){
    sheetRownames <- rownames(df)
    areCoreRownamesMissing <- vapply(templateRownames, function(i) (!(i %in% sheetRownames)), logical(1))
  }

  any(c(areCoreColnamesMissing, areCoreRownamesMissing))
}

extraChecksExcel <- function(pathToExcel, isBeaujonTemplate){

  templateTempfile <- tempfile(fileext = ".xlsx")

  GET(
    if (isBeaujonTemplate) LINK_TEMPLATE_BEAUJON else LINK_TEMPLATE_EXT,
    write_disk(templateTempfile, overwrite = TRUE)
  )

  dfTemplateRefModalities <- readRefModalitiesSheet(templateTempfile)
  dfCheckedRefModalities <- readRefModalitiesSheet(pathToExcel)

  missingRefModalitiesMask <- createMismatchMask(dfTemplateRefModalities, dfCheckedRefModalities)

  warnings <- list(areRefModalitiesMissing = do.call(cbind, missingRefModalitiesMask))

  warnings

}

createMismatchMask <- function(dfTemplate, df){
  as.data.frame(
    mapply(
      function(colTemplate, colChecked) !(colChecked %in% c(colTemplate, NA)),
      dfTemplate,
      df,
      SIMPLIFY = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

# Create error message to be rendered in case of wrong excel file chosen
getErrorMessageWrongExcelSheet <- function(errors){

  errorMessageLines <- tagList(h3("Errors :", style= paste0("color: ", ERROR_COLOR, ";")))

  # Error line for an excel file with the wrong number of sheets if needed
  if (errors["hasWrongNumberOfSheets"])
    errorMessageLines <- tagList(errorMessageLines, span("- File must have exactly 3 sheets.", style= ERROR_STYLE), br())

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
          "- Missing sheets (",
          paste(
            missingSheets,
            collapse = ", "
          ),
          ")."
        ),
        style= ERROR_STYLE
      ),
      br()
    )
  }

  if (any(errors[c("isSheetNucleicAcidWrong", "isSheetPlateplanWrong", "isSheetRefModalitiesWrong")])) {
    wrongSheets <- c()
    if (errors["isSheetNucleicAcidWrong"]){ wrongSheets <- c(wrongSheets, "nucleic_acid") }
    if (errors["isSheetPlateplanWrong"]){ wrongSheets <- c(wrongSheets,  "plateplan") }
    if (errors["isSheetRefModalitiesWrong"]){ wrongSheets <- c(wrongSheets, "RefModalities") }

    errorMessageLines <- tagList(
      errorMessageLines,
      span(
        paste0(
          "- Sheets containing wrong tables (",
          paste(
            wrongSheets,
            collapse = ", "
          ),
          ")."
        ),
        style= ERROR_STYLE
      ),
      br()
    )
  }

  # Error line for an excel file with the wrong platewell column if needed
  if (errors["isColumnPlatewellWrong"]){
    errorMessageLines <- tagList(errorMessageLines, span("- Platewell column in nucleic_acid sheet does not match the one from the template.", style= ERROR_STYLE), br())
  }

  errorMessageLines <- tagList(errorMessageLines, br(), span("(Please check the procedure at the following location : ", a("RNASeqProcedureICM", href=LINK_PROCEDURE), ")", style= ERROR_STYLE), br())

  # Error lines combined if multiple
  errorMessageLines
}

getWarningMessageExcelSheet <- function(warnings, pathToExcel){

  warningMessageSection <- tagList(
    h3("Warning :", style = paste0("color: ", WARNING_COLOR, ";"))
  )

  # Check if any RefModalities are missing
  if (any(warnings$areRefModalitiesMissing)){
    # Read Excel sheet
    dfCheckedRefModalities <- readRefModalitiesSheet(pathToExcel)

    missingRefModalities <- dfCheckedRefModalities[warnings$areRefModalitiesMissing]

    # Build warning message with datatable
    warningMessageSection <- tagList(
      warningMessageSection,
      span(
        "Highlighted RefModalities don't match the template. Please contact the person in charge of the template to add new RefModalities.",
        style = WARNING_STYLE
      ),
      br(),
        # Create datatable
        datatable(dfCheckedRefModalities, options = list(scrollX = TRUE, dom = "rtip"), rownames = FALSE)  |> formatStyle(names(dfCheckedRefModalities), backgroundColor = styleEqual(missingRefModalities, WARNING_COLOR) , color = styleEqual(missingRefModalities, "white") )
    )
  }

  warningMessageSection
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
  df <- read_excel(pathToExcel, sheet = "nucleic_acid", na= c("", "NA"))
  names(df)[1] <- "Line_Number"
  df
}

# Returns a slightly formated version of the data in the plateplan sheet of the excel file as a dataframe
readPlateplanSheet <- function(pathToExcel){
  df <- read_excel(pathToExcel, sheet = "plateplan", range = "A1:M9", col_names = TRUE)
  df <- as.data.frame(df)
  rownames(df) <- df[[1]]
  df[[1]] <- NULL
  df
}

# Returns a slightly formated version of the data in the RefModalities sheet of the excel file as a dataframe
readRefModalitiesSheet <- function(pathToExcel){
  df <- read_excel(pathToExcel, sheet = "RefModalities", col_names = TRUE)
  df
}

# For each RefModalties of the sheet, checks if it has the wrong nomenclature and returns TRUE for each that does
checkNomenclatureRefModalities <- function(dfRefModalities){
  apply(dfRefModalities, 2, function(x) grepl("^[^a-zA-Z]",x) | grepl("[^a-zA-Z0-9_&=+-]",x))
}

# For each sample in the nucleic_acid sheet, checks if the names of the svs and xml files are not matching.
checkFileNamesBeaujonColumns <- function(dfNucleicAcid){

  svsMissing <- is.na(dfNucleicAcid$ID_scanSVS)
  xmlMissing <- is.na(dfNucleicAcid$ID_annotationXML)

  svsBase <- sub("\\.svs$", "", dfNucleicAcid$ID_scanSVS)
  xmlBase <- sub("\\.xml$", "", dfNucleicAcid$ID_annotationXML)

  idNotInSvs <- mapply(
    function(id, svs) if (is.na(svs)) FALSE else !grepl(id, svs, fixed = TRUE),
    dfNucleicAcid$ID_NucleicAcid,
    dfNucleicAcid$ID_scanSVS
  )

  idNotInXml <- mapply(
    function(id, xml) if (is.na(xml)) FALSE else !grepl(id, xml, fixed = TRUE),
    dfNucleicAcid$ID_NucleicAcid,
    dfNucleicAcid$ID_annotationXML
  )

  invalidFiles <-
    (!svsMissing & !grepl("\\.svs$", dfNucleicAcid$ID_scanSVS)) |
    (!xmlMissing & !grepl("\\.xml$", dfNucleicAcid$ID_annotationXML)) |
    (!(svsMissing | xmlMissing) & (svsBase != xmlBase)) |
    (!svsMissing & idNotInSvs) |
    (!xmlMissing & idNotInXml)

  return(invalidFiles)


}

# For each ID in the column, checks if it has the right nomenclature and returns a boolean
checkNomenclatureID <- function(nucleicAcidIDColumn){
  grepl("^[^a-zA-Z]",nucleicAcidIDColumn) | grepl("[^a-zA-Z0-9_]",nucleicAcidIDColumn)
}

# For each ID in the column, checks if there is another occurence of it. (Second check with from last needed to get all occurences not all-1)
checkDuplicatedID <- function(nucleicAcidIDColumn){
  duplicated(nucleicAcidIDColumn, incomparables = NA) | duplicated(nucleicAcidIDColumn, incomparables = NA, fromLast = TRUE)
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
  )[["acquisition"]]

}

checkIfProjectMissingFromSerge <- function(df, projectsInSerge, incrementSize = NULL){

  rows <- lapply(seq_len(nrow(df)), function(i) {

    # Progress update
    if (!is.null(incrementSize)) incProgress(incrementSize)

    (!(df$Project[i] %in% projectsInSerge))
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

checkPlateplanContent <- function(rowDfNucleicAcid, dfPlateplan){
  plateRow = gsub("[0-9]", "", rowDfNucleicAcid["Platewell"])
  plateCol = gsub("[A-Za-z]", "", rowDfNucleicAcid["Platewell"])
  id = rowDfNucleicAcid["ID_NucleicAcid"]
  if (is.na(id)){
    dfPlateplan[plateRow,plateCol] != "0"
  }else{
    id != dfPlateplan[plateRow,plateCol]
  }
}

# Factory function for data tables rendered in the results
createFormattedDataTable <- function(df, colnames, coloredColumn, backgroundColor, color){
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
  ) |> formatStyle(columns = coloredColumn, backgroundColor = backgroundColor, color = color)
}

# Factory function for the UI sections rendered in the results
createCheckResultSection <- function(df, colnames ,coloredColumn, errorMessage, successMessage, backgroundColorDT =  ERROR_COLOR, colorDT = "white"){

  # Defines the table to render containing the lines of the excel with a duplicated NucleicAcid_ID if there are
  if (nrow(df) > 0) {
    dtToBeRendered <- createFormattedDataTable(
      df,
      colnames,
      coloredColumn,
      backgroundColorDT,
      colorDT
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

createProcedureSection <- function(uiElements){
  if (any(grepl(ERROR_COLOR, sapply(uiElements, as.character)))) {
    tagList(span("Errors found. (Please check the procedure at the following location : ", a("RNASeqProcedureICM", href=LINK_PROCEDURE), " )", style= ERROR_STYLE), br())
  }else{
    tagList(NULL)
  }
}

createCoreNucleicAcidSheetSections <- function(dfNucleicAcidSheet, dfRefModalitiesSheet, incrementSize = NULL) {

  dfNucleicAcidSheetWithLimitedColumns <- dfNucleicAcidSheet[, c( "Species", "Project", "StorageBeforeExtraction", "RnaExtractedFrom", "SampleFrom", "ExtractionMethod")]

  missingValuesInRefModalitiesMask <- createMismatchMask(dfRefModalitiesSheet, dfNucleicAcidSheetWithLimitedColumns)

  missingValuesInRefModalities <- unique(dfNucleicAcidSheetWithLimitedColumns[do.call(cbind, missingValuesInRefModalitiesMask)])

  checkValuesInRefModalitiesSection <- createCheckResultSection(
    if (any(missingValuesInRefModalitiesMask)) dfNucleicAcidSheet else data.frame(),
    names(dfNucleicAcidSheet),
    names(dfNucleicAcidSheet),
    "Following values are not specified in the corresponding column of the RefModalities sheet.",
    "All limited values in the Nucleic Acid sheet exist in RefModalities.",
    backgroundColorDT = styleEqual(missingValuesInRefModalities, ERROR_COLOR),
    colorDT = styleEqual(missingValuesInRefModalities, "white")
  )

  # Dataframe subset generated by checking the nomenclature of the nucleic acid IDs. (Only letters, numbers or underscore allowed and only letters as the first character)
  wrongNomenclatureRows <- dfNucleicAcidSheet[checkNomenclatureID(dfNucleicAcidSheet$ID_NucleicAcid), ]

  checkNomenclatureSection <- createCheckResultSection(
    wrongNomenclatureRows,
    names(wrongNomenclatureRows),
    "ID_NucleicAcid",
    tagList(
      "Nucleic Acid IDs do not follow the right nomenclature for the following sample(s).",
      br(), 
      "(Please use IDs containing only letters, number and \"_\" and starting only with a letter (no number or \"_\" at the start of the ID.)"
    ),
    "All Nucleic Acid IDs follow the right nomenclature."
  )

  # Progress update
  if (!is.null(incrementSize)) incProgress(incrementSize)

  # Dataframe subset generated by checking if the nucleic acid ID is duplicated in the table. (from the beginning and from the end to get the first occurence too)
  duplicatedNucleicAcidIDs <- dfNucleicAcidSheet[checkDuplicatedID(dfNucleicAcidSheet$ID_NucleicAcid), ]

  checkDuplicatesSection <- createCheckResultSection(
    duplicatedNucleicAcidIDs,
    names(duplicatedNucleicAcidIDs),
    "ID_NucleicAcid",
    "Duplicate Nucleic Acid IDs found for the following samples.",
    "All samples have unique Nucleic Acid IDs."
  )

  # Progress update
    if (!is.null(incrementSize)) incProgress(incrementSize)

  # Defines a dataframe that will be used to store the names of the rawfiles registered in the Serge database for each NucleicAcid_ID if there are any
  dfFilesInSergeForSample <- getFilesInSerge(dfNucleicAcidSheet, con, incrementSize)

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

  projectsInSerge <- getUniqueProjectsInSerge(dfNucleicAcidSheet, con)

  # Defines a dataframe that will be used to store the names of the rawfiles registered in the Serge database for each NucleicAcid_ID if there are any
  dfSamplesWithMissingProjectInSerge <- dfNucleicAcidSheet[checkIfProjectMissingFromSerge(dfNucleicAcidSheet, projectsInSerge, incrementSize), ]

  checkProjectNotInSergeSection <- createCheckResultSection(
    dfSamplesWithMissingProjectInSerge, 
    names(dfSamplesWithMissingProjectInSerge),
    "Project",
    "Following projects are missing from Serge database for provided samples (check if new projects).",
    "All projects exist."
  )


  # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
  checkSamplesSection <- tagList(h3("Nucleic Acid Sheet Section"), checkValuesInRefModalitiesSection, checkNomenclatureSection, checkDuplicatesSection, checkAlreadyInSergeSection, checkProjectNotInSergeSection)


}

createBeaujonNucleicAcidSections <- function(dfNucleicAcidSheet, incrementSize = NULL){
  
  dfFileNamesNotMatchingBeaujonColumns <- dfNucleicAcidSheet[checkFileNamesBeaujonColumns(dfNucleicAcidSheet),]

  if (!is.null(incrementSize)) incProgress(incrementSize)

  checkBeaujonColumnsSection <- createCheckResultSection(
    dfFileNamesNotMatchingBeaujonColumns, 
    names(dfFileNamesNotMatchingBeaujonColumns),
    c("ID_NucleicAcid", "ID_scanSVS", "ID_annotationXML"),
    tagList(
      "Names of the svs and/or xml files for the following samples contain errors.", 
      br(), 
      "(File names must contain the corresponding sample ID, their respective extensions (.svs or .xml) and be identical except for the extension if both are provided.)"
    ),
    "All svs and xml file names are correct."
  )


}

createCorePlateplanSheetSections <- function(dfNucleicAcidSheet, dfPlateplanSheet, incrementSize = NULL) {

  wrongPlateplanContentRows <- dfNucleicAcidSheet[apply(dfNucleicAcidSheet, 1, checkPlateplanContent, dfPlateplan = dfPlateplanSheet), ]
  
  checkPlateplanContentSection <- createCheckResultSection(
    wrongPlateplanContentRows,
    names(wrongPlateplanContentRows),
    c("ID_NucleicAcid", "Platewell"),
    "Plateplan table not correctly filled for following sample(s). Please check if the right ID_NucleicAcid is provided at the corresponding coordinates (\"0\" if no ID provided).",
    "The Plateplan sheet table is filled correctly."
  )

  # Progress update
  if (!is.null(incrementSize)) incProgress(incrementSize)

  # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
  checkPlateplanContentSection <- tagList(h3("Plateplan Section"), checkPlateplanContentSection)

}

createCoreRefModalitiesSheetSections <- function(dfRefModalities, incrementSize = NULL) {

  wrongRefModalitiesNomenclatureMask <- checkNomenclatureRefModalities(dfRefModalities)
  wrongRefModalitiesNomenclatureValues <- dfRefModalities[wrongRefModalitiesNomenclatureMask]

  checkNomenclatureInRefModalitiesSection <- createCheckResultSection(
    if (any(wrongRefModalitiesNomenclatureMask)) dfRefModalities else data.frame(),
    names(dfRefModalities),
    names(dfRefModalities),
    "Following values do not have the right nomenclature in the RefModalities sheet.",
    "All values in the RefModalities sheet have the right nomenclature.",
    backgroundColorDT = styleEqual(wrongRefModalitiesNomenclatureValues, ERROR_COLOR),
    colorDT = styleEqual(wrongRefModalitiesNomenclatureValues, "white")
  )

  # Progress update
  if (!is.null(incrementSize)) incProgress(incrementSize)

  # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
  checkNomenclatureInRefModalitiesSection <- tagList(h3("RefModalities Section"), checkNomenclatureInRefModalitiesSection)

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

  # Checks if the provided excel file is the right one for the post-sequencing step
  validateExcelPost <- reactive({
    req(input$samplesInfoTablePostSeq)
    checkIfRightExcel(input$samplesInfoTablePostSeq$datapath)
  })

  # Checks if the excel provided is the Beajon version or the external one (some additionnal columns in the nucleic_acid sheet of the Beaujon one)
  isExcelBeaujonVersionPre <- reactive({
    req(input$samplesInfoTablePreSeq)
    !any(checkIfWrongSheet(readNucleicAcidSheet(input$samplesInfoTablePreSeq$datapath), templateColnames = BEAUJON_TEMPLATE_EXTRA_NUCLEIC_ACID_COLNAMES))
  })

  isExcelBeaujonVersionPost <- reactive({
    req(input$samplesInfoTablePostSeq)
    !any(checkIfWrongSheet(readNucleicAcidSheet(input$samplesInfoTablePostSeq$datapath), templateColnames = BEAUJON_TEMPLATE_EXTRA_NUCLEIC_ACID_COLNAMES))
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

    uiElements <- tagList(NULL)

    if (any(errors)) {
      uiElements <- tagList(uiElements,getErrorMessageWrongExcelSheet(errors))
    } else {
      uiElements <- tagList(uiElements, h2("Summary table"), DTOutput("summaryTablePreSeq"))
    }

    if (!(errors["isSheetRefModalitiesMissing"]) && !(errors["isSheetRefModalitiesWrong"])){

      warnings <- extraChecksExcel(input$samplesInfoTablePreSeq$datapath, isExcelBeaujonVersionPre())

      if ( any(warnings$areRefModalitiesMissing) ) {
        uiElements <- tagList(uiElements, getWarningMessageExcelSheet(warnings, input$samplesInfoTablePreSeq$datapath))
      }

    }

    uiElements
  })

  output$SamplesPostSeq <- renderUI({
    req(input$samplesInfoTablePostSeq)

    errors <- validateExcelPost()

    uiElements <- tagList(NULL)

    if (any(errors)) {
      uiElements <- tagList(uiElements,getErrorMessageWrongExcelSheet(errors))
    } else {
      uiElements <- tagList(uiElements, h2("Summary table"), DTOutput("summaryTablePostSeq"))
    }

    if (!(errors["isSheetRefModalitiesMissing"]) & !(errors["isSheetRefModalitiesWrong"])){

      warnings <- extraChecksExcel(input$samplesInfoTablePostSeq$datapath, isExcelBeaujonVersionPost())

      if (any(warnings$areRefModalitiesMissing)) {
        uiElements <- tagList(uiElements, getWarningMessageExcelSheet(warnings, input$samplesInfoTablePostSeq$datapath))
      }

    }

    uiElements
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

      # Imports the plateplan sheet of the excel file
      dataPlateplanSheetTablePreSeq <- readPlateplanSheet(input$samplesInfoTablePreSeq$datapath)

      # Imports the RefModalities sheet of the excel file
      dataRefModalitiesSheetTablePreSeq <- readRefModalitiesSheet(input$samplesInfoTablePreSeq$datapath)

      # Defines the real number of samples in the excel file used for the incrementation of the progress bar
      nbSamples <- length(dataNucleicAcidSheetTablePreSeq$ID_NucleicAcid)

      incrementSize <- 1/(nbSamples+4)

      incProgress(incrementSize)

      # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
      checkSamplesSection <- createCoreNucleicAcidSheetSections(dataNucleicAcidSheetTablePreSeq, dataRefModalitiesSheetTablePreSeq,incrementSize)

      if (isExcelBeaujonVersionPre()){
        checkBeaujonColumnsSection <- createBeaujonNucleicAcidSections(dataNucleicAcidSheetTablePreSeq, incrementSize)
      } else {
        checkBeaujonColumnsSection <- tagList(NULL)
      }

      checkSamplesSection <- tagList(checkSamplesSection, checkBeaujonColumnsSection)

      checkPlateplanSection <- createCorePlateplanSheetSections(dataNucleicAcidSheetTablePreSeq, dataPlateplanSheetTablePreSeq, incrementSize)

      checkRefModalitiesSection <- createCoreRefModalitiesSheetSections(dataRefModalitiesSheetTablePreSeq, incrementSize)

      validationReportSections <- tagList(checkSamplesSection, checkPlateplanSection, checkRefModalitiesSection)

      redirectToProcedureSection <- createProcedureSection(validationReportSections)

      validationReportSections <- tagList(redirectToProcedureSection, validationReportSections)

      #Returns all the sections to render if the excel sheets are right if not only the excel sheet check section
      resultsPre(validationReportSections)

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

          # Imports the plateplan sheet of the excel file
          dataPlateplanSheetTablePostSeq <- readPlateplanSheet(input$samplesInfoTablePostSeq$datapath)

          # Imports the RefModalities sheet of the excel file
          dataRefModalitiesSheetTablePostSeq <- readRefModalitiesSheet(input$samplesInfoTablePostSeq$datapath)

          # Defines the real number of samples in the excel file used for the incrementation of the progress bar
          nbSamples <- length(dataNucleicAcidSheetTablePostSeq$ID_NucleicAcid)

          incrementSize <- 1/(nbSamples*2+4)

          incProgress(incrementSize)

          checkSamplesSection <- createCoreNucleicAcidSheetSections(dataNucleicAcidSheetTablePostSeq, dataRefModalitiesSheetTablePostSeq, incrementSize)

          hasNoFiles <- logical()

          dataNucleicAcidSheetTablePostSeqNoNAID <- dataNucleicAcidSheetTablePostSeq[!is.na(dataNucleicAcidSheetTablePostSeq$ID_NucleicAcid),]

          # Sends a query to the Serge database and adds the needed data to the dataframe mentionned above for each NucleicAcid_ID if there is any data returned 
          for (i in seq_len(nrow(dataNucleicAcidSheetTablePostSeqNoNAID))){

            filesCheckResults <- tryCatch({
              rawToChar(ssh_exec_internal(sshSession, sprintf( "ls %s/%s_*.fastq*",input$rawfilesDirPathPostSeq, dataNucleicAcidSheetTablePostSeqNoNAID[i,"ID_NucleicAcid"]))$stdout)
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

          dfFilesNotInIFB <- dataNucleicAcidSheetTablePostSeqNoNAID[hasNoFiles, ]

          checkFilesInIFBSection <- createCheckResultSection(
            dfFilesNotInIFB, 
            names(dfFilesNotInIFB),
            "ID_NucleicAcid",
            "Files not found in provided IFB directory for the following sample(s).",
            "Files found for all samples."
          )

          if (isExcelBeaujonVersionPost()){
            checkBeaujonColumnsSection <- createBeaujonNucleicAcidSections(dataNucleicAcidSheetTablePostSeq, incrementSize)
          } else {
            checkBeaujonColumnsSection <- tagList(NULL)
          }

          # Defines UI elements to render for the section of the results about the content of the nucleic_acid sheet of the excel file (Sections about nomenclature, duplicates and already existing files in Serge)
          checkSamplesSection <- tagList( checkSamplesSection, checkFilesInIFBSection, checkBeaujonColumnsSection)

          checkPlateplanSection <- createCorePlateplanSheetSections(dataNucleicAcidSheetTablePostSeq, dataPlateplanSheetTablePostSeq, incrementSize)

          checkRefModalitiesSection <- createCoreRefModalitiesSheetSections(dataRefModalitiesSheetTablePostSeq, incrementSize)

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
          validationReportSections <- tagList(checkSamplesSection, checkPlateplanSection, checkRefModalitiesSection)

          redirectToProcedureSection <- createProcedureSection(validationReportSections)

          tagList(redirectToProcedureSection, validationReportSections)
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
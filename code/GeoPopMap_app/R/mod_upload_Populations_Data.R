#' Upload Populations Data UI Function
#'
#' @description A Shiny module for uploading population data file in various formats.
#' It allows users to specify separators, decimal formats, headers, and rename columns.
#'
#' @param id A unique identifier for the module.
#'
#' @importFrom shiny NS tagList fileInput selectInput checkboxInput uiOutput actionButton
#' @import shiny
#' @import readxl
#' @import data.table
#' @import promises
#' @import future
#' @noRd
mod_upload_Populations_Data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # File upload input
    fileInput(ns('file0'), 'Choose File for Populations Data',
              accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),

    # Separator selection for CSV/TXT files
    selectInput(ns('sep0'), 'Separator', c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ';'),

    # Decimal separator selection
    selectInput(ns("dec0"), "Decimal Separator",
                choices = c("Point (.)" = ".", "Comma (,)" = ","), selected = ","),

    # Checkbox for determining if the file contains a header
    checkboxInput(ns("header0"), "Does the file contain a header?", value = TRUE),

    # Selection for the common column in the dataset
    selectInput(ns("common_col0"), "Select Common Column", choices = NULL),

    # Checkbox to allow column renaming
    checkboxInput(ns("ChangeColumnName0"), "Do you need to rename a column?", value = FALSE),

    # Conditional panel for renaming columns
    conditionalPanel(
      condition = paste0("input['", ns("ChangeColumnName0"), "'] == true"),
      selectInput(ns("rename_col0"), "Select Column to Rename", choices = NULL),
      textInput(ns("new_col_name0"), "New Column Name", value = ""),
      actionButton(ns("apply_changes0"), "Apply Changes")
    )
  )
}

#' Upload Populations Data Server Function
#'
#' @description This server module handles file uploads, automatically detects delimiters,
#' loads data into a reactive object, and allows users to rename columns dynamically.
#'
#' @param id A unique identifier for the module.
#'
#' @return A reactive data frame containing the uploaded and processed data.
#'
#' @noRd
mod_upload_Populations_Data_server <- function(id, InputFilePopulations, SepFilePopulations, DecFilePopulations, HeaderFilePopulations,
                                               ApplyChangeFilePopulations, RenameColFilePopulations, NewColNameFilePopulations, CommonColNameFilePopulations
                                               ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Automatically save thresholds when inputs change
    observe({
      req(input$file0)
      InputFilePopulations(input$file0)
      SepFilePopulations(input$sep0)
      DecFilePopulations(input$dec0)
      HeaderFilePopulations(input$header0)
      ApplyChangeFilePopulations(input$apply_changes0)
      RenameColFilePopulations(input$rename_col0)
      NewColNameFilePopulations(input$new_col_name0)
      CommonColNameFilePopulations(input$common_col0)
    })

    # Reactive value to store uploaded data
    popdata <- reactiveVal(NULL)

    # Function to detect and read file with correct separator
    detect_and_read_file <- function(file_path, dec, header) {
      future({
        separators <- c(",", ";", "\t")
        decimal_marks <- if (dec == ",") c(".") else c(",")

        results <- NULL
        attempted <- list()  # Keep track of attempted configurations to avoid repetitions

        for (sep in separators) {
          for (decimal in decimal_marks) {
            if (sep != decimal && !paste(sep, decimal) %in% attempted) {
              attempted <- c(attempted, paste(sep, decimal))  # Record the attempt

              tryCatch({
                data <- fread(file_path, sep = sep, dec = decimal, header = header, data.table = FALSE, fill = TRUE)
                if (!is.null(data) && ncol(data) > 1) {
                  results <- list(data = data, separator = sep, decimal = decimal)
                  return(results)  # Successful read, return results
                }
              }, error = function(e) {
                print(paste("Failed to read data with separator", sep, "and decimal mark", decimal, ": ", e$message))
              })
            } else {
              print(paste("Skipping combination of separator", sep, "and decimal mark", decimal, "to avoid conflicts"))
            }
          }
          if (!is.null(results)) break;  # Break outer loop if successful
        }

        if (is.null(results)) {
          showNotification("No suitable separator or decimal mark found or file could not be read.", type = "error")
        }
        results
      })
    }

    # Observe file input and parameters
    observeEvent({
      InputFilePopulations()
      SepFilePopulations()
      DecFilePopulations()
      HeaderFilePopulations()
    }, {
      req(InputFilePopulations()) # Ensure a file is uploaded before proceeding
      file_ext <- tools::file_ext(InputFilePopulations()$name) # Extract file extension

      # Attempt to read the file based on its extension
      withProgress(message = 'Loading data...', {
        if (file_ext %in% c("csv", "txt", "tsv")) {
          detect_and_read_file(InputFilePopulations()$datapath, DecFilePopulations(), HeaderFilePopulations()) %>% then(~{
            if (!is.null(.$data)) {
              popdata(.$data)
              updateSelectInput(session, "sep0", selected = .$separator)
              updateUI(.$data)
            } else {
              showNotification("Failed to detect separator or load file.", type = "error")
            }
          }) %>% catch(~{
            showNotification("Error processing the file.", type = "error")
          })
        } else if (file_ext %in% c("xls", "xlsx")) {
          # Handling Excel files
          future({
            readxl::read_excel(InputFilePopulations()$datapath, col_names = HeaderFilePopulations())
          }) %>% then(~{
            if (!HeaderFilePopulations()) {
              colnames(.) <- paste0("V", seq_len(ncol(.)))
            }
            popdata(.)
            updateUI(.)
          }) %>% catch(~{
            showNotification("Error reading Excel file.", type = "error")
          })
        } else {
          stop("Unsupported file type. Please upload a CSV or Excel file.")
        }




      #popdata(data)

    })
    })


    updateUI <- function(data) {
      updateSelectInput(session, "common_col0", choices = names(data), selected = names(data)[1])
      updateSelectInput(session, "rename_col0", choices = names(data))
      showNotification("File loaded successfully.", type = "message")
    }
    # Observe renaming request
    observeEvent(ApplyChangeFilePopulations(), {
      req(popdata())
      data <- popdata()

      # Apply column renaming if valid
      if (!is.null(RenameColFilePopulations()) && RenameColFilePopulations() != "" && NewColNameFilePopulations() != "") {
        colnames(data)[colnames(data) == RenameColFilePopulations()] <- NewColNameFilePopulations()
      }

      # Update UI to reflect changes
      updateSelectInput(session, "common_col0", choices = names(data), selected = names(data)[1])
      updateSelectInput(session, "rename_col0", choices = names(data))

      popdata(data)
      updateUI(data)
      showNotification("Changes applied successfully.", type = "message")
    })

    # Return reactive dataset
    return(reactive({
      if (is.null(popdata())) {
        return(NULL)  # Return NULL if popdata is empty
      }
      req(popdata())
      popdata()
    }))
  })
}

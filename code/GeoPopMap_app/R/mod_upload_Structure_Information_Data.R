#' upload_Structure_Information_Data UI Function
#'
#' @description A Shiny module for uploading structure information data file in various formats.
#' It allows users to specify separators, decimal formats, headers, and rename columns.
#'
#' @param id A unique identifier for the module.
#'
#' @importFrom shiny NS tagList fileInput selectInput checkboxInput uiOutput actionButton
#' @import shiny
#' @import readxl
#' @noRd
mod_upload_Structure_Information_Data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # File upload input
    fileInput(ns('file3'), 'Choose File for Structure Information Data',
              accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),

    # Separator selection for CSV/TXT files
    selectInput(ns('sep3'), 'Separator', c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ';'),

    # Decimal separator selection
    selectInput(ns("dec3"), "Decimal Separator",
                choices = c("Point (.)" = ".", "Comma (,)" = ","), selected = ","),

    # Checkbox for determining if the file contains a header
    checkboxInput(ns("header3"), "Does the file contain a header?", value = TRUE),

    # Selection for the common column in the dataset
    selectInput(ns("common_col3"), "Select Common Column", choices = NULL),

    # Checkbox to allow column renaming
    checkboxInput(ns("ChangeColumnName3"), "Do you need to rename a column?", value = FALSE),

    # Conditional panel for renaming columns
    conditionalPanel(
      condition = paste0("input['", ns("ChangeColumnName3"), "'] == true"),
      selectInput(ns("rename_col3"), "Select Column to Rename", choices = NULL),
      textInput(ns("new_col_name3"), "New Column Name", value = ""),
      actionButton(ns("apply_changes3"), "Apply Changes")
    )
  )
}

#' Upload Structure Information Data Server Function
#'
#' @description This server module handles file uploads, automatically detects delimiters,
#' loads data into a reactive object, and allows users to rename columns dynamically.
#'
#' @param id A unique identifier for the module.
#'
#' @return A reactive data frame containing the uploaded and processed data.
#'
#' @noRd
mod_upload_Structure_Information_Data_server <- function(id, InputFileStructure, SepFileStructure, DecFileStructure, HeaderFileStructure,
                                                         ApplyChangeFileStructure, RenameColFileStructure, NewColNameFileStructure, CommonColNameFileStructure
                                                         ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Automatically save thresholds when inputs change
    observe({
      req(input$file3)
      InputFileStructure(input$file3)
      SepFileStructure(input$sep3)
      DecFileStructure(input$dec3)
      HeaderFileStructure(input$header3)
      ApplyChangeFileStructure(input$apply_changes3)
      RenameColFileStructure(input$rename_col3)
      NewColNameFileStructure(input$new_col_name3)
      CommonColNameFileStructure(input$common_col3)
    })

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

    # Reactive value to store uploaded data
    strdata <- reactiveVal(NULL)



    # Observe file input and parameters
    observeEvent({
      InputFileStructure()
      SepFileStructure()
      DecFileStructure()
      HeaderFileStructure()
    }, {
      req(InputFileStructure()) # Ensure a file is uploaded before proceeding
      file_ext <- tools::file_ext(InputFileStructure()$name) # Extract file extension

      # Attempt to read the file based on its extension
      withProgress(message = 'Loading data...', {
        if (file_ext %in% c("csv", "txt", "tsv")) {
          detect_and_read_file(InputFileStructure()$datapath, DecFileStructure(), HeaderFileStructure()) %>% then(~{
            if (!is.null(.$data)) {
              strdata(.$data)
              updateSelectInput(session, "sep3", selected = .$separator)
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
            readxl::read_excel(InputFileStructure()$datapath, col_names = HeaderFileStructure())
          }) %>% then(~{
            if (!HeaderFileStructure()) {
              colnames(.) <- paste0("V", seq_len(ncol(.)))
            }
            strdata(.)
            updateUI(.)
          }) %>% catch(~{
            showNotification("Error reading Excel file.", type = "error")
          })
        } else {
          stop("Unsupported file type. Please upload a CSV or Excel file.")
        }

      })

    })

    updateUI <- function(data) {
      updateSelectInput(session, "common_col3", choices = names(data), selected = names(data)[1])
      updateSelectInput(session, "rename_col3", choices = names(data))
      showNotification("File loaded successfully.", type = "message")
    }

    # Observe renaming request
    observeEvent(ApplyChangeFileStructure(), {
      req(strdata())
      data <- strdata()

      # Apply column renaming if valid
      if (!is.null(RenameColFileStructure()) && RenameColFileStructure() != "" && NewColNameFileStructure() != "") {
        colnames(data)[colnames(data) == RenameColFileStructure()] <- NewColNameFileStructure()
      }

      # Update UI to reflect changes
      updateSelectInput(session, "common_col3", choices = names(data), selected = names(data)[1])
      updateSelectInput(session, "rename_col3", choices = names(data))

      strdata(data)
      updateUI(data)
      showNotification("Changes applied successfully.", type = "message")
    })

    # Return reactive dataset
    return(reactive({
      if (is.null(strdata())) {
        return(NULL)  # Return NULL if strdata is empty
      }
      req(strdata())
      strdata()
    }))

  })
}

## To be copied in the UI
# mod_upload_Structure_Information_Data_ui("upload_Structure_Information_Data_3")

## To be copied in the server
# mod_upload_Structure_Information_Data_server("upload_Structure_Information_Data_3")

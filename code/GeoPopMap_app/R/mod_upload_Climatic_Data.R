#' upload_Climatic_Data UI Function
#'
#' @description A Shiny module for uploading climatic data file in various formats.
#' It allows users to specify separators, decimal formats, headers, and rename columns.
#'
#' @param id A unique identifier for the module.
#'
#' @importFrom shiny NS tagList fileInput selectInput checkboxInput uiOutput actionButton
#' @import shiny
#' @import readxl
#' @noRd
mod_upload_Climatic_Data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # File upload input
    fileInput(ns('file1'), 'Choose File for Climatic Data',
              accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),

    # Separator selection for CSV/TXT files
    selectInput(ns('sep1'), 'Separator', c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ';'),

    # Decimal separator selection
    selectInput(ns("dec1"), "Decimal Separator",
                choices = c("Point (.)" = ".", "Comma (,)" = ","), selected = ","),

    # Checkbox for determining if the file contains a header
    checkboxInput(ns("header1"), "Does the file contain a header?", value = TRUE),


    # Selection for the common column in the dataset
    selectizeInput(ns("common_col1"), "Select Common Column", choices = NULL,
                   options = list(maxOptions = 1000)),


    # Checkbox to allow column renaming
    checkboxInput(ns("ChangeColumnName1"), "Do you need to rename a column?", value = FALSE),

    # Conditional panel for renaming columns
    conditionalPanel(
      condition = paste0("input['", ns("ChangeColumnName1"), "'] == true"),
      selectizeInput(ns("rename_col1"), "Select Column to Rename", choices = NULL,
                     options = list(maxOptions = 1000)),

      textInput(ns("new_col_name1"), "New Column Name", value = ""),
      actionButton(ns("apply_changes1"), "Apply Changes")
    ),

    uiOutput(ns("col_selector_clim_ui")),

    actionButton(ns("apply_col_selection_clim"), "Apply Column Selection"),

  )
}

#' Upload Climatic Data Server Function
#'
#' @description This server module handles file uploads, automatically detects delimiters,
#' loads data into a reactive object, and allows users to rename columns dynamically.
#'
#' @param id A unique identifier for the module.
#'
#' @return A reactive data frame containing the uploaded and processed data.
#'
#' @noRd
mod_upload_Climatic_Data_server <- function(id, InputFileClimatic, SepFileClimatic, DecFileClimatic, HeaderFileClimatic,
                                            ApplyChangeFileClimatic, RenameColFileClimatic, NewColNameFileClimatic, CommonColNameFileClimatic
                                            ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Automatically save thresholds when inputs change
    observe({
      req(input$file1)
      InputFileClimatic(input$file1)
      SepFileClimatic(input$sep1)
      DecFileClimatic(input$dec1)
      HeaderFileClimatic(input$header1)
      ApplyChangeFileClimatic(input$apply_changes1)
      RenameColFileClimatic(input$rename_col1)
      NewColNameFileClimatic(input$new_col_name1)
      CommonColNameFileClimatic(input$common_col1)
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
    Climdata <- reactiveVal(NULL)

    # Observe file input and parameters
    observeEvent({
      InputFileClimatic()
      SepFileClimatic()
      DecFileClimatic()
      HeaderFileClimatic()
    }, {
      req(InputFileClimatic()) # Ensure a file is uploaded before proceeding
      file_ext <- tools::file_ext(InputFileClimatic()$name) # Extract file extension

      # Attempt to read the file based on its extension
      withProgress(message = 'Loading data...', {
        if (file_ext %in% c("csv", "txt", "tsv")) {
          detect_and_read_file(InputFileClimatic()$datapath, DecFileClimatic(), HeaderFileClimatic()) %>% then(~{
            if (!is.null(.$data)) {
              Climdata(.$data)
              updateSelectInput(session, "sep1", selected = .$separator)
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
            readxl::read_excel(InputFileClimatic()$datapath, col_names = HeaderFileClimatic())
          }) %>% then(~{
            if (!HeaderFileClimatic()) {
              colnames(.) <- paste0("V", seq_len(ncol(.)))
            }
            Climdata(.)
            updateUI(.)
          }) %>% catch(~{
            showNotification("Error reading Excel file.", type = "error")
          })
        } else {
          stop("Unsupported file type. Please upload a CSV or Excel file.")
        }

      })

    })


    is_likely_numeric <- function(x, sample_n = 5) {
      x <- as.character(x)
      x <- gsub(",", ".", x)                           # Convert decimal commas
      x <- x[!is.na(x) & x != "" & x != "NA"]          # Filter out blanks and literal "NA"
      x_sample <- head(x, sample_n)                    # Take first N values

      if (length(x_sample) == 0) return(FALSE)

      numeric_attempt <- suppressWarnings(as.numeric(x_sample))
      all(!is.na(numeric_attempt))
    }



    updateUI <- function(data) {

      max_col <- 100

      non_numeric_cols <- names(data)[!sapply(data, function(x) is_likely_numeric(as.character(x)))]

      if (length(non_numeric_cols) == 0) {
        showNotification(" No non-numeric columns found.", type = "error")
        non_numeric_cols <- character(0)
      } else if (length(non_numeric_cols) > max_col) {
        showNotification(paste0(" Too many non-numeric columns (", length(non_numeric_cols), "). Only first ", max_col, " shown in dropdowns."), type = "warning")
        non_numeric_cols <- non_numeric_cols[1:max_col]
      }



      updateSelectizeInput(
        session,
        inputId = "common_col1",
        choices = non_numeric_cols,
        selected = if (length(non_numeric_cols) > 0) non_numeric_cols[1] else NULL
      )

      updateSelectizeInput(
        session,
        inputId = "rename_col1",
        choices =non_numeric_cols
      )

      showNotification("File loaded successfully.", type = "message")
    }

    # Observe renaming request
    observeEvent(ApplyChangeFileClimatic(), {
      req(Climdata())

      if (is.null(FilteredClimdata())) {
        data <- Climdata()
      } else {
        data <- FilteredClimdata()
      }


      # Apply column renaming if valid
      if (!is.null(RenameColFileClimatic()) && RenameColFileClimatic() != "" && NewColNameFileClimatic() != "") {
        colnames(data)[colnames(data) == RenameColFileClimatic()] <- NewColNameFileClimatic()
      }

      if (is.null(FilteredClimdata())) {
        Climdata(data)
      } else {
        FilteredClimdata(data)
      }
      updateUI(data)
      showNotification("Changes applied successfully.", type = "message")
    })


    output$col_selector_clim_ui <- renderUI({
      ns <- session$ns

      req(Climdata())
      data <- Climdata()

      all_cols <- names(data)
      # Intelligent display: no overload on the UI side
      shinyWidgets::pickerInput(
        inputId = ns("columns_to_show_clim"),
        label = "Select columns to display:",
        choices = all_cols,
        selected = all_cols[1:min(20, length(all_cols))],
        options = list(`live-search` = TRUE, `actions-box` = TRUE, `size` = 10),
        multiple = TRUE
      )
    })

    FilteredClimdata <- reactiveVal(NULL)

    observeEvent(input$apply_col_selection_clim, {
      req(Climdata())
      req(input$columns_to_show_clim)
      selected_cols <- input$columns_to_show_clim
      data <- Climdata()[, selected_cols, drop = FALSE]
      FilteredClimdata(data)
    })


    # Return reactive dataset
    return(reactive({
      if (is.null(Climdata())) {
        return(NULL)  # Return NULL if Climdata is empty
      }
      req(Climdata())
      if (is.null(FilteredClimdata())) {
        return(Climdata())
      } else {
        return(FilteredClimdata())
      }

    }))
  })
}

## To be copied in the UI
# mod_upload_Climatic_Data_ui("upload_Climatic_Data_1")

## To be copied in the server
# mod_upload_Climatic_Data_server("upload_Climatic_Data_1")

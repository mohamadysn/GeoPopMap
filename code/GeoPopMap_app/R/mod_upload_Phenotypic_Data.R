#' upload_Phenotypic_Data UI Function
#'
#' @description A Shiny module for uploading phenotypic data file in various formats.
#' It allows users to specify separators, decimal formats, headers, and rename columns.
#'
#' @param id A unique identifier for the module.
#'
#' @importFrom shiny NS tagList fileInput selectInput checkboxInput uiOutput actionButton
#' @import shiny
#' @import readxl
#' @noRd
mod_upload_Phenotypic_Data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # File upload input
    fileInput(ns('file2'), 'Choose File for Phenotypic Data',
              accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),

    # Separator selection for CSV/TXT files
    selectInput(ns('sep2'), 'Separator', c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ';'),

    # Decimal separator selection
    selectInput(ns("dec2"), "Decimal Separator",
                choices = c("Point (.)" = ".", "Comma (,)" = ","), selected = ","),

    # Checkbox for determining if the file contains a header
    checkboxInput(ns("header2"), "Does the file contain a header?", value = TRUE),

    # Selection for the common column in the dataset
    selectizeInput(ns("common_col2"), "Select Common Column", choices = NULL,
                   options = list(maxOptions = 1000)),

    # Checkbox to allow column renaming
    checkboxInput(ns("ChangeColumnName2"), "Do you need to rename a column?", value = FALSE),

    # Conditional panel for renaming columns
    conditionalPanel(
      condition = paste0("input['", ns("ChangeColumnName2"), "'] == true"),
      selectizeInput(ns("rename_col2"), "Select Column to Rename", choices = NULL,
                     options = list(maxOptions = 1000)),
      textInput(ns("new_col_name2"), "New Column Name", value = ""),
      actionButton(ns("apply_changes2"), "Apply Changes")
    ),

    uiOutput(ns("col_selector_pheno_ui")),

    actionButton(ns("apply_col_selection_pheno"), "Apply Column Selection"),
  )
}

#' upload_Phenotypic_Data Server Functions
#'
#' @description This server module handles file uploads, automatically detects delimiters,
#' loads data into a reactive object, and allows users to rename columns dynamically.
#'
#' @param id A unique identifier for the module.
#'
#' @return A reactive data frame containing the uploaded and processed data.
#'
#' @noRd
mod_upload_Phenotypic_Data_server <- function(id, InputFilePhenotypic, SepFilePhenotypic, DecFilePhenotypic, HeaderFilePhenotypic,
                                            ApplyChangeFilePhenotypic, RenameColFilePhenotypic, NewColNameFilePhenotypic, CommonColNameFilePhenotypic
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Automatically save thresholds when inputs change
    observe({
      req(input$file2)
      InputFilePhenotypic(input$file2)
      SepFilePhenotypic(input$sep2)
      DecFilePhenotypic(input$dec2)
      HeaderFilePhenotypic(input$header2)
      ApplyChangeFilePhenotypic(input$apply_changes2)
      RenameColFilePhenotypic(input$rename_col2)
      NewColNameFilePhenotypic(input$new_col_name2)
      CommonColNameFilePhenotypic(input$common_col2)
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
    Phenotypicdata <- reactiveVal(NULL)

    # Observe file input and parameters
    observeEvent({
      InputFilePhenotypic()
      SepFilePhenotypic()
      DecFilePhenotypic()
      HeaderFilePhenotypic()
    }, {
      req(InputFilePhenotypic()) # Ensure a file is uploaded before proceeding
      file_ext <- tools::file_ext(InputFilePhenotypic()$name) # Extract file extension

      # Attempt to read the file based on its extension
      withProgress(message = 'Loading data...', {
        if (file_ext %in% c("csv", "txt", "tsv")) {
          detect_and_read_file(InputFilePhenotypic()$datapath, DecFilePhenotypic(), HeaderFilePhenotypic()) %>% then(~{
            # browser()
            if (!is.null(.$data)) {
              Phenotypicdata(.$data)
              updateSelectInput(session, "sep2", selected = .$separator)
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
            readxl::read_excel(InputFilePhenotypic()$datapath, col_names = HeaderFilePhenotypic())
          }) %>% then(~{
            if (!HeaderFilePhenotypic()) {
              colnames(.) <- paste0("V", seq_len(ncol(.)))
            }
            Phenotypicdata(.)
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
        inputId = "common_col2",
        choices = non_numeric_cols,
        selected = if (length(non_numeric_cols) > 0) non_numeric_cols[1] else NULL
      )

      updateSelectizeInput(
        session,
        inputId = "rename_col2",
        choices =non_numeric_cols
      )
      showNotification("File loaded successfully.", type = "message")
    }


    # Observe renaming request
    observeEvent(ApplyChangeFilePhenotypic(), {
      req(Phenotypicdata())

      if (is.null(FilteredPhenotypicdata())) {
        data <- Phenotypicdata()
      } else {
        data <- FilteredPhenotypicdata()
      }


      # Apply column renaming if valid
      if (!is.null(RenameColFilePhenotypic()) && RenameColFilePhenotypic() != "" && NewColNameFilePhenotypic() != "") {
        colnames(data)[colnames(data) == RenameColFilePhenotypic()] <- NewColNameFilePhenotypic()
      }

      if (is.null(FilteredPhenotypicdata())) {
        Phenotypicdata(data)
      } else {
        FilteredPhenotypicdata(data)
      }
      updateUI(data)
      showNotification("Changes applied successfully.", type = "message")
    })


    output$col_selector_pheno_ui <- renderUI({
      ns <- session$ns

      req(Phenotypicdata())
      data <- Phenotypicdata()

      all_cols <- names(data)
      # Intelligent display: no overload on the UI side
      shinyWidgets::pickerInput(
        inputId = ns("columns_to_show_pheno"),
        label = "Select columns to display:",
        choices = all_cols,
        selected = all_cols[1:min(20, length(all_cols))],
        options = list(`live-search` = TRUE, `actions-box` = TRUE, `size` = 10),
        multiple = TRUE
      )
    })


    FilteredPhenotypicdata <- reactiveVal(NULL)

    observeEvent(input$apply_col_selection_pheno, {
      req(Phenotypicdata())
      req(input$columns_to_show_pheno)
      selected_cols <- input$columns_to_show_pheno
      data <- Phenotypicdata()[, selected_cols, drop = FALSE]
      FilteredPhenotypicdata(data)
    })


    # Return reactive dataset
    return(reactive({
      if (is.null(Phenotypicdata())) {
        return(NULL)  # Return NULL if Phenotypicdata is empty
      }
      req(Phenotypicdata())
      if (is.null(FilteredPhenotypicdata())) {
        return(Phenotypicdata())
      } else {
        return(FilteredPhenotypicdata())
      }

    }))
  })
}

## To be copied in the UI
# mod_upload_Phenotypic_Data_ui("upload_Phenotypic_Data")

## To be copied in the server
# mod_upload_Phenotypic_Data_server("upload_Phenotypic_Data")

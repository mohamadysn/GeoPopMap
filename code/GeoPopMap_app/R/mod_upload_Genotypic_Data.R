#' upload_Genotypic_Data UI Function
#'
#' @description A Shiny module for uploading genotypic data file in various formats.
#' It allows users to specify separators, decimal formats, headers, and rename columns.
#'
#' @param id A unique identifier for the module.
#'
#' @importFrom shiny NS tagList fileInput selectInput checkboxInput uiOutput actionButton
#' @import shiny
#' @importFrom shinyWidgets pickerInput
#' @import readxl
#' @noRd
mod_upload_Genotypic_Data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns('file4'), 'Choose File for Genotypic Data',
              accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),

    selectInput(ns('sep4'), 'Separator', c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ';'),

    selectInput(ns("dec4"), "Decimal Separator",
                choices = c("Point (.)" = ".", "Comma (,)" = ","), selected = ","),

    checkboxInput(ns("header4"), "Does the file contain a header?", value = TRUE),

    selectizeInput(ns("common_col4"), "Select Common Column", choices = NULL,
                   options = list(maxOptions = 1000)),

    checkboxInput(ns("ChangeColumnName4"), "Do you need to rename a column?", value = FALSE),

    conditionalPanel(
      condition = paste0("input['", ns("ChangeColumnName4"), "'] == true"),
      selectizeInput(ns("rename_col4"), "Select Column to Rename", choices = NULL,
                     options = list(maxOptions = 1000)),
      textInput(ns("new_col_name4"), "New Column Name", value = ""),
      actionButton(ns("apply_changes4"), "Apply Changes")
    ),

    uiOutput(ns("col_selector_geno_ui")),
    uiOutput(ns("chromosome_selector_ui")),
    uiOutput(ns("position_range_ui")),
    actionButton(ns("apply_col_selection_geno"), "Apply Column Selection")
  )
}

#' upload_Genotypic_Data Server Functions
#'
#' @description This server module handles file uploads, automatically detects delimiters,
#' loads data into a reactive object, and allows users to rename columns dynamically.
#'
#' @param id A unique identifier for the module.
#'
#' @return A reactive data frame containing the uploaded and processed data.
#'
#' @noRd
mod_upload_Genotypic_Data_server <- function(id, InputFileGenotypic, SepFileGenotypic, DecFileGenotypic, HeaderFileGenotypic,
                                             ApplyChangeFileGenotypic, RenameColFileGenotypic, NewColNameFileGenotypic, CommonColNameFileGenotypic, ChromosomeVec, PositionVec, SpecialMode
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    Genotypicdata <- reactiveVal(NULL)

    FilteredGenotypicdata <- reactiveVal(NULL)

    observe({
      req(input$file4)
      InputFileGenotypic(input$file4)
      SepFileGenotypic(input$sep4)
      DecFileGenotypic(input$dec4)
      HeaderFileGenotypic(input$header4)
      ApplyChangeFileGenotypic(input$apply_changes4)
      RenameColFileGenotypic(input$rename_col4)
      NewColNameFileGenotypic(input$new_col_name4)
      CommonColNameFileGenotypic(input$common_col4)
    })

    detect_and_read_file <- function(file_path, dec, header) {
      future({
        separators <- c(",", ";", "\t")
        decimal_marks <- if (dec == ",") c(".") else c(",")
        results <- NULL
        attempted <- list()

        for (sep in separators) {
          for (decimal in decimal_marks) {
            if (sep != decimal && !paste(sep, decimal) %in% attempted) {
              attempted <- c(attempted, paste(sep, decimal))
              tryCatch({
                data <- fread(file_path, sep = sep, dec = decimal, header = header, data.table = FALSE, fill = TRUE)
                if (!is.null(data) && ncol(data) > 1) {
                  results <- list(data = data, separator = sep, decimal = decimal)
                  return(results)
                }
              }, error = function(e) {
                print(paste("Failed to read data with separator", sep, "and decimal mark", decimal, ": ", e$message))
              })
            } else {
              print(paste("Skipping combination of separator", sep, "and decimal mark", decimal, "to avoid conflicts"))
            }
          }
          if (!is.null(results)) break
        }

        if (is.null(results)) {
          showNotification("No suitable separator or decimal mark found or file could not be read.", type = "error")
        }
        results
      })
    }



    observeEvent({
      InputFileGenotypic()
      SepFileGenotypic()
      DecFileGenotypic()
      HeaderFileGenotypic()
    }, {
      req(InputFileGenotypic())
      file_ext <- tools::file_ext(InputFileGenotypic()$name)

      withProgress(message = 'Loading data...', {
        if (file_ext %in% c("csv", "txt", "tsv")) {
          detect_and_read_file(InputFileGenotypic()$datapath, DecFileGenotypic(), HeaderFileGenotypic()) %>% then(~{
            if (!is.null(.$data)) {
              data_read <- .$data
              updateSelectInput(session, "sep4", selected = .$separator)

              # Check if special format exists
              first_row <- as.character(data_read[1, ])
              second_row <- as.character(data_read[2, ])

              is_chr_line <- grepl("chr", first_row, ignore.case = TRUE)
              is_chr_line <- "TRUE" %in% is_chr_line
              is_pos_line <- grepl("pos", second_row, ignore.case = TRUE)
              is_pos_line <- "TRUE" %in% is_pos_line

              if (is_chr_line && is_pos_line) {
                SpecialMode(TRUE)
                ChromosomeVec(first_row[-c(1)])
                PositionVec(as.numeric(gsub(",", ".", second_row[-c(1)])))

                # Remove the first two rows: keep only real data
                data_final <- data_read[-c(1, 2), , drop = FALSE]
                rownames(data_final) <- NULL
                Genotypicdata(data_final)

                showNotification("Special format detected: Chromosomes and Positions extracted!", type = "message")
              } else {
                SpecialMode(FALSE)
                Genotypicdata(data_read)

                showNotification("Standard file loaded.", type = "message")
              }

              updateUI(Genotypicdata())
            } else {
              showNotification("Failed to detect separator or load file.", type = "error")
            }
          }) %>% catch(~{
            showNotification("Error processing the file.", type = "error")
          })
        } else if (file_ext %in% c("xls", "xlsx")) {
          future({
            readxl::read_excel(InputFileGenotypic()$datapath, col_names = HeaderFileGenotypic())
          }) %>% then(~{
            if (!HeaderFileGenotypic()) {
              colnames(.) <- paste0("V", seq_len(ncol(.)))
            }
            Genotypicdata(.)
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
      x <- gsub(",", ".", x)
      x <- x[!is.na(x) & x != "" & x != "NA"]
      x_sample <- head(x, sample_n)
      if (length(x_sample) == 0) return(FALSE)
      numeric_attempt <- suppressWarnings(as.numeric(x_sample))
      all(!is.na(numeric_attempt))
    }

    updateUI <- function(data) {
      max_col <- 100
      non_numeric_cols <- names(data)[!sapply(data, function(x) is_likely_numeric(as.character(x)))]

      if (length(non_numeric_cols) == 0) {
        showNotification("\u26A0\uFE0F No non-numeric columns found.", type = "error")
        non_numeric_cols <- character(0)
      } else if (length(non_numeric_cols) > max_col) {
        showNotification(paste0("\u26A0\uFE0F Too many non-numeric columns (", length(non_numeric_cols), "). Only first ", max_col, " shown in dropdowns."), type = "warning")
        non_numeric_cols <- non_numeric_cols[1:max_col]
      }

      updateSelectizeInput(
        session,
        inputId = "common_col4",
        choices = non_numeric_cols,
        selected = if (length(non_numeric_cols) > 0) non_numeric_cols[1] else NULL
      )

      updateSelectizeInput(
        session,
        inputId = "rename_col4",
        choices = non_numeric_cols
      )
      showNotification("File loaded successfully.", type = "message")
    }

    observeEvent(ApplyChangeFileGenotypic(), {
      req(Genotypicdata())
      if (is.null(FilteredGenotypicdata())) {
        data <- Genotypicdata()
      } else {
        data <- FilteredGenotypicdata()
      }
      if (!is.null(RenameColFileGenotypic()) && RenameColFileGenotypic() != "" && NewColNameFileGenotypic() != "") {
        colnames(data)[colnames(data) == RenameColFileGenotypic()] <- NewColNameFileGenotypic()
      }
      if (is.null(FilteredGenotypicdata())) {
        Genotypicdata(data)
      } else {
        FilteredGenotypicdata(data)
      }
      updateUI(data)
      showNotification("Changes applied successfully.", type = "message")
    })

    output$col_selector_geno_ui <- renderUI({
      ns <- session$ns
      req(Genotypicdata())

      if (is.null(FilteredGenotypicdata())) {
        data <- Genotypicdata()
      } else {
        data <- FilteredGenotypicdata()
      }


      all_cols <- names(data)
      shinyWidgets::pickerInput(
        inputId = ns("columns_to_show_geno"),
        label = "Select columns to display:",
        choices = all_cols,
        selected = all_cols[1:min(20, length(all_cols))],
        options = list(`live-search` = TRUE, `actions-box` = TRUE, `size` = 10),
        multiple = TRUE
      )
    })



    output$chromosome_selector_ui <- renderUI({
      ns <- session$ns
      req(SpecialMode(), ChromosomeVec())
      if (SpecialMode()) {
        selectInput(ns("chromosome_selected"), "Select Chromosome",
                    choices = c("All", unique(ChromosomeVec())),
                    selected = "All")
      }
    })

    output$position_range_ui <- renderUI({
      ns <- session$ns
      req(SpecialMode(), PositionVec(), input$chromosome_selected)
      if (SpecialMode()) {
        if (input$chromosome_selected == "All") {
          pos <- PositionVec()
        } else {
          idx_chr <- which(ChromosomeVec() == input$chromosome_selected)
          pos <- PositionVec()[idx_chr]
        }
        if (length(pos) > 0) {
          sliderInput(ns("position_range"),
                      "Select Position Range:",
                      min = min(pos, na.rm = TRUE),
                      max = max(pos, na.rm = TRUE),
                      value = range(pos, na.rm = TRUE))
        }
      }
    })


    # observeEvent(input$apply_col_selection_geno, {
    #   req(Genotypicdata())
    #   req(input$columns_to_show_geno)
    #   selected_cols <- input$columns_to_show_geno
    #   data <- Genotypicdata()[, selected_cols, drop = FALSE]
    #   FilteredGenotypicdata(data)
    # })

    observe({
      req(SpecialMode(), ChromosomeVec(), PositionVec(), input$chromosome_selected, input$position_range)

      if (input$chromosome_selected == "All") {
        idx <- PositionVec() >= input$position_range[1] & PositionVec() <= input$position_range[2]
      } else {
        idx_chr <- ChromosomeVec() == input$chromosome_selected
        idx_pos <- PositionVec() >= input$position_range[1] & PositionVec() <= input$position_range[2]
        idx <- idx_chr & idx_pos
      }

      common_col <- input$common_col4
      filtered_cols <- colnames(Genotypicdata())[idx]
      filtered_cols <- unique(c(common_col, filtered_cols))



      updatePickerInput(
        session,
        inputId = "columns_to_show_geno",
        choices = filtered_cols,
        selected = filtered_cols[1:min(5, length(filtered_cols))]
      )
    })



    observeEvent(input$apply_col_selection_geno, {
      req(Genotypicdata())

      if (SpecialMode()) {
        req(ChromosomeVec(), PositionVec(), input$chromosome_selected, input$position_range, input$columns_to_show_geno)

        if (input$chromosome_selected == "All") {
          idx_pos <- PositionVec() >= input$position_range[1] & PositionVec() <= input$position_range[2]
          geno_cols <- names(Genotypicdata())[idx_pos]
        } else {
          idx_chr <- ChromosomeVec() == input$chromosome_selected
          idx_pos <- PositionVec() >= input$position_range[1] & PositionVec() <= input$position_range[2]
          geno_cols <- names(Genotypicdata())[idx_chr & idx_pos]
        }

        # Always keep common_col4 first
        manual_cols <- input$columns_to_show_geno
        selected_cols <- intersect(manual_cols, geno_cols)
        common_col <- input$common_col4
        selected_cols <- unique(c(common_col, selected_cols))  # ensure no duplication
        data <- Genotypicdata()[, selected_cols, drop = FALSE]

      } else {
        req(input$columns_to_show_geno)
        selected_cols <- input$columns_to_show_geno

        # Always keep common_col4 first
        common_col <- input$common_col4
        selected_cols <- unique(c(common_col, selected_cols))  # ensure no duplication
        data <- Genotypicdata()[, selected_cols, drop = FALSE]
      }

      FilteredGenotypicdata(data)
    })



    # return(reactive({
    #   if (is.null(Genotypicdata())) {
    #     return(NULL)
    #   }
    #   req(Genotypicdata())
    #   if (is.null(FilteredGenotypicdata())) {
    #     return(Genotypicdata())
    #   } else {
    #     return(FilteredGenotypicdata())
    #   }
    # }))
    return(list(
      filtered = reactive({
        if (is.null(Genotypicdata())) return(NULL)
        if (is.null(FilteredGenotypicdata())) {
          return(Genotypicdata())
        } else {
          return(FilteredGenotypicdata())
        }
      }),
      original = reactive({
        Genotypicdata()
      })
    ))

  })
}

## To be copied in the UI
# mod_upload_Genotypic_Data_ui("upload_Genotypic_Data")

## To be copied in the server
# mod_upload_Genotypic_Data_server("upload_Genotypic_Data")

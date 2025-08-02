#' Merge Data UI Function
#'
#' @description A shiny Module for merging datasets.
#'
#' @param id Unique identifier for the module.
#'
#' @noRd
#'
#' @importFrom  shiny NS tagList fluidRow column h2 p tabsetPanel tabPanel
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @import shinythemes
#' @import shinyWidgets
#' @import dplyr
#' @import DT
mod_Merge_Data_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Merged Table", sidebarLayout(
    sidebarPanel(
      #Admixture threshold
      uiOutput(ns("admixture_threshold_ui")),

      uiOutput(ns("run_map_button_ui"))

    ),
    mainPanel(
      DT::DTOutput(ns('climdataviz'))
    )
  ))
}




#' Merge Data Server Function
#'
#' @description Server logic for merging datasets.
#' @noRd
mod_Merge_Data_server <- function(id,
                                  MergeButton, cleaned_popdata, CommonColNameFilePopulations, Climdata,
                                  CommonColNameFileClimatic, Phenotypicdata, CommonColNameFilePhenotypic, Genotypicdata , CommonColNameFileGenotypic,
                                  strdata, CommonColNameFileStructure, CommunColumn, AdmixtureThreshold, RunMap){
  moduleServer(id, function(input, output, session){
    ns <- session$ns





    output$run_map_button_ui <- renderUI({
      req(nrow(merged_climdata()) > 0) # Ensure cleaned data is available before rendering

      if (!is.data.frame(merged_climdata())) {
        showNotification("Error: Cleaned data is not a data frame.", type = "error")
        return(NULL) # Stop execution if cleaned data is invalid
      }
      tagList(
      tags$h4("If everything is ok "),
      actionButton(ns("run_Map"), "Run Map Visualisation")
      )
    })


    output$admixture_threshold_ui <- renderUI({
      if (!is.null(strdata()) && nrow(strdata()) > 0) {
        numericInput(
          ns("admixture_threshold"),
          "Admixture threshold :",
          value = 0.6,
          min = 0,
          max = 1,
          step = 0.01
        )
      } else {
        NULL
      }
    })

    # Save thresholds when inputs change
    observe({
      if (!is.null(input$admixture_threshold)) {
        AdmixtureThreshold(input$admixture_threshold)
      }
      RunMap(input$run_Map)

    })

    # Reactive value to store the merged data
    Merged_Climdata <- reactiveVal(NULL)
    displayed_merged_df <- reactiveVal(NULL) # Store the table to display
    na_modal_shown <- reactiveVal(FALSE) # Flag to control NA modal display

    # Function to automatically detect numeric columns
    detect_numeric_columns <- function(df) {
      is_numeric_col <- function(x) {
        # Remove commas and check if all non-NA values can be converted to numeric
        x <- gsub(",", ".", as.character(x))  # Replace commas with dots
        all(suppressWarnings(!is.na(as.numeric(x))))  # Check if all values can be numeric
      }
      numeric_cols <- names(df)[sapply(df, is_numeric_col)]  # Apply check to all columns
      return(numeric_cols)
    }

    is_likely_numeric <- function(x, sample_n = 5) {
      x <- as.character(x)
      x <- gsub(",", ".", x)                           # Convert decimal commas
      x <- x[!is.na(x) & x != "" & x != "NA"]          # Filter out blanks and literal "NA"
      x_sample <- head(x, sample_n)                    # Take first N values

      if (length(x_sample) == 0) return(FALSE)

      numeric_attempt <- suppressWarnings(as.numeric(x_sample))
      all(!is.na(numeric_attempt))
    }

    # Utility function to add Assignation columns
    add_assignation <- function(df, strdata, threshold) {
      if (!is.null(strdata) && nrow(strdata) > 0) {
        nb_grp <- dim(strdata)[2] - 1
        vec <- paste0("K", 1:nb_grp)
        if (all(vec %in% colnames(df))) {
          struct <- subset(df, select = vec)
          df$Assignation <- names(struct)[max.col(struct)]
          df$Assignation_value <- apply(struct, 1, max)
          df$Assignation[df$Assignation_value < threshold] <- "Admixed"
          df$Assignation <- as.factor(df$Assignation)
        }
      }
      return(df)
    }

    # Reactive value to store the merged dataset
    merged_climdata <- eventReactive({
      list(
        MergeButton(),
        AdmixtureThreshold = if (!is.null(strdata()) && nrow(strdata()) > 0) AdmixtureThreshold() else NULL
      )
      # MergeButton()
      # if (!is.null(strdata()) && nrow(strdata()) > 0) {
      #   AdmixtureThreshold()
      # }
    }, {
      req(cleaned_popdata(), MergeButton())   # Ensure required data exists before proceeding

      pop <- cleaned_popdata() # Load the population data
      datasets <- list(pop) # Initialize the list of datasets with the population data

      # Check if each dataset (climatic, phenotypic, and structure) is available and add it to the list
      if (!is.null(Climdata()) && nrow(Climdata()) > 0) datasets <- append(datasets, list(Climdata()))
      if (!is.null(Phenotypicdata()) && nrow(Phenotypicdata()) > 0) datasets <- append(datasets, list(Phenotypicdata()))
      if (!is.null(Genotypicdata()) && nrow(Genotypicdata()) > 0) datasets <- append(datasets, list(Genotypicdata()))
      if (!is.null(strdata()) && nrow(strdata()) > 0) datasets <- append(datasets, list(strdata()))


      # Check if the common column name is defined and valid
      if (!is.null(CommonColNameFilePopulations()) && CommonColNameFilePopulations() != "") {
        common_col <- CommonColNameFilePopulations()

        # Ensure the common column is consistent across all datasets
        for (i in 2:length(datasets)) {
          col_name <- colnames(datasets[[i]])[colnames(datasets[[i]]) %in% c(CommonColNameFileClimatic(),
                                                                             CommonColNameFilePhenotypic(),
                                                                             CommonColNameFileGenotypic(),
                                                                             CommonColNameFileStructure())]
          # Rename the column if it differs from the common column name
          if (length(col_name) > 0 && col_name != common_col) {
            colnames(datasets[[i]])[colnames(datasets[[i]]) == col_name] <- common_col
          }
        }

        # Merge all datasets using `reduce()`, ensuring only matching records are kept
        merged_df <- purrr::reduce(datasets, function(x, y) merge(x, y, by = common_col, all = FALSE))

        # Rename the first column to "Code"
        colnames(merged_df)[1] <- "Code"
        CommunColumn("Code")
        
        # Automatically detect numeric and qualitative columns
        numeric_cols <- names(merged_df)[sapply(merged_df, function(x) is_likely_numeric(as.character(x)))]
        qualitative_cols <- names(merged_df)[!sapply(merged_df, function(x) is_likely_numeric(as.character(x)))]

        # numeric_cols <- detect_numeric_columns(merged_df)
        # qualitative_cols <- setdiff(names(merged_df), numeric_cols)  # Everything else is qualitative

        # Convert only numeric columns
        merged_df[numeric_cols] <- lapply(merged_df[numeric_cols], function(x) {
          as.numeric(gsub(",", ".", x))  # Replace commas before conversion
        })

        # Ensure all qualitative columns remain as characters
        merged_df[qualitative_cols] <- lapply(merged_df[qualitative_cols], as.character)

        # --- Added: NA/empty cell detection and user choice ---
        na_or_empty <- function(df) {
          is_na <- is.na(df)
          is_empty <- df == ""
          is_empty[is.na(is_empty)] <- FALSE
          is_na | is_empty
        }

        if (any(na_or_empty(merged_df)) && !na_modal_shown()) {
          na_modal_shown(TRUE) # Set the flag so modal is not shown again until next merge
          temp_merged_df <- merged_df
          # Reset keep_na and remove_na action buttons
          session$sendInputMessage(ns("keep_na"), list(value = 0))
          session$sendInputMessage(ns("remove_na"), list(value = 0))
          showModal(
            modalDialog(
              title = "Missing values detected",
              "Some missing values (NA) or empty cells were detected in the merged table. This may cause errors or unexpected results in some statistical analyses (PCA, ANOVA, etc.), but will not affect the map visualization.",
              br(),
              "What would you like to do?",
              footer = div(
                style = "display: flex; gap: 10px; justify-content: center;",
                actionButton(ns("keep_na"), "Keep rows with NA/empty cells"),
                actionButton(ns("remove_na"), "Remove rows containing NA/empty cells")
              ),
              easyClose = FALSE
            )
          )

          # Observe the user's choice
          observeEvent(input$keep_na, {
            removeModal()
            df <- add_assignation(temp_merged_df, strdata(), AdmixtureThreshold())
            showNotification("Warning: Missing values/empty cells are kept. Some statistical analyses may fail or give biased results.", type = "warning")
            displayed_merged_df(df)
            Merged_Climdata(df)
            merged_df <- df
          }, once = TRUE)

          observeEvent(input$remove_na, {
            removeModal()
            filtered_df <- temp_merged_df[!apply(na_or_empty(temp_merged_df), 1, any), , drop = FALSE]
            df <- add_assignation(filtered_df, strdata(), AdmixtureThreshold())
            showNotification(paste0("Rows containing NA/empty cells removed (", nrow(temp_merged_df) - nrow(filtered_df), " rows removed)."), type = "message")
            
            displayed_merged_df(df)
            Merged_Climdata(df)
            merged_df <- df
          }, once = TRUE)

          # Return the temporary table for now (will be updated after the user's choice)
          return(merged_df)
        }
        # --- End added ---

        merged_df <- add_assignation(merged_df, strdata(), AdmixtureThreshold())
        Merged_Climdata(merged_df)  # Store the merged dataset
        displayed_merged_df(merged_df) # Default: show full merged table
        showNotification("Data merged successfully!", type = "message")
        return(merged_df)
      } else {
        showNotification("Common column not found!", type = "error")
      }
    })

    output$climdataviz <- DT::renderDataTable({

      req(!is.null(displayed_merged_df()))

      if (!is.data.frame(displayed_merged_df())) {
        showNotification("Error: Cleaned data is not a data frame.", type = "error")
        return(NULL) # Stop execution if cleaned data is invalid
      }

      # Render DataTable with scrolling and pagination options
      DT::datatable(
        displayed_merged_df(), # Display the network metrics in a table
        extensions = 'Buttons',
        options = list(
          dom = 'Blfrtip',
          buttons = list(
            list(extend = 'copy', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'excel', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'pdf', exportOptions = list(modifier = list(page = "all")))
          ),
          scrollX = TRUE,  # Enables horizontal scrolling
          paging = TRUE ,
          pageLength = 5,             # Set the number of rows displayed per page (optional)
          autoWidth = FALSE             # Automatically adjust column widths
        ),
        # we ensure that HTML code can be displayed
        escape = FALSE
      )
    }, server = FALSE )

    observeEvent(MergeButton(), {
      displayed_merged_df(NULL)
      na_modal_shown(FALSE) # Reset the flag at each merge
    })

    return(displayed_merged_df) # Return the merged dataset

  })
}

## To be copied in the UI
# mod_Merge_Data_ui("Merge_Data_1")

## To be copied in the server
# mod_Merge_Data_server("Merge_Data_1")

#' Stat_structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column h2 p tabsetPanel tabPanel
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @import shinythemes
#' @import ggplot2
#' @import dplyr
#' @import colourpicker
#' @import svglite


mod_Stat_structure_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Stat", sidebarLayout(
    sidebarPanel(
      width = 3,

      # Add scroll bar
      style = "height: 90vh; overflow-y: auto;",

      br(), # Space

      ### Structure parameters ----
      tags$h4(strong("Structure")),


      #Histogram bins
      sliderInput(ns("bins"), 'Number of bins for histograms :', min = 5, max = 100, value = 25)
    ),
    mainPanel(

      uiOutput(ns("Stat_panel"))

    )
  ))
}

#' Stat_structure Server Functions
#'
#' @noRd
mod_Stat_structure_server <- function(id,  cleaned_popdata, merged_climdata,
                                      strdata, ColumnSelectForColor,
                                      Col1, Col2, Col3, BinsHistogram,
                                      selected_points, CommunColumn){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Automatically save thresholds when inputs change
    observe({
      BinsHistogram(input$bins)
    })

    output$Stat_panel <- renderUI(

      ### Condition var quanti + structure ----
      if(!is.null(cleaned_popdata()) && !is.null(merged_climdata()) && !is.null(strdata()) && !is.null(ColumnSelectForColor()) && is.numeric(merged_climdata()[[ColumnSelectForColor()]])){

        ### Stat summaries ----
        tabPanel("Stat summaries",
                 tags$h3(strong("Distribution")),
                 column(12, downloadButton(ns("downloadHistSVG"), "Download plot as SVG"), downloadButton(ns("downloadHistPNG"), "Download plot as PNG")),
                 textOutput(ns("histInfo")),
                 br(),
                 plotOutput(ns("histclim"), width = "800px", height = "700px"),
                 # tags$h3(strong("Stats of the selected climatic variable")),
                 # tableOutput(ns("summary")))
                 tabsetPanel(
                   tabPanel(
                     "Summary of Selected Variable",
                     DT::DTOutput(ns("summary"))
                   ),
                   tabPanel(
                     "Summary of All Numeric Variables",
                     DT::DTOutput(ns("summary_all"))
                   )
                 )
        )
      }
      ### Condition var quanti only ----
      else if(!is.null(cleaned_popdata()) && !is.null(merged_climdata()) && !is.null(ColumnSelectForColor()) && is.numeric(merged_climdata()[[ColumnSelectForColor()]])){
        tabPanel("Stat summaries",
                 tags$h3(strong("Distribution")),
                 column(12, downloadButton(ns("downloadHistSVG"), "Download plot as SVG"), downloadButton(ns("downloadHistPNG"), "Download plot as PNG")),
                 textOutput(ns("histInfo")),
                 br(),
                 plotOutput(ns("histclim"), width = "800px", height = "700px"),
                 # tags$h3(strong("Stats of the selected climatic variable")),
                 # tableOutput(ns("summary")))
                 tabsetPanel(
                   tabPanel(
                     "Summary of Selected Variable",
                     DT::DTOutput(ns("summary"))
                   ),
                   tabPanel(
                     "Summary of All Numeric Variables",
                     DT::DTOutput(ns("summary_all"))
                   )
                 )
        )

      }
      ### Condition var quali ----
      else {
        tabPanel("Stat summaries",
                 tags$h3(strong("Distribution")),
                 p(style = "text-align: justify;",
                   "Quanlitative variable only, We can't calculate") ,
                 br())
      }
    )




    ## Stats clim ----

    ### Histogram ----

    histclim <- reactive({
      req(ColumnSelectForColor(), Col1()) # Ensure required inputs are available
      if (is.numeric(merged_climdata()[[ColumnSelectForColor()]])){
        #pop_clim <- merged_climdata()
        #value <- as.numeric(pop_clim[[ColumnSelectForColor()]]) # Convert selected column to numeric


        if (!is.null(selected_points()) && length(selected_points()) >= 2) {

          pop_clim <- merged_climdata()[merged_climdata()[[CommunColumn()]] %in% selected_points(), ]
          value <- as.numeric(pop_clim[[ColumnSelectForColor()]])
        } else {
          pop_clim <- merged_climdata()
          value <- as.numeric(pop_clim[[ColumnSelectForColor()]]) # Convert selected column to numeric
        }



        mean_val <- mean(value, na.rm = TRUE) # Calculate mean, handling missing values
        bins <- as.numeric(BinsHistogram()) # Get the number of bins

        # Create the histogram plot
        gp <- ggplot(data = pop_clim, aes(x = value, fill = after_stat(x))) +
          geom_histogram(aes(y = after_stat(density)), bins = bins)+ # after_stat(density) is used to set x axis as continuous for the color gradient
          scale_fill_gradientn(colors = c(Col1(),Col2(),Col3()))+ #Set colors for the gradient
          geom_density(color = "#000000", fill = "black", alpha = 0.3, linewidth = 0.5)+ # Density plot
          geom_vline(aes(xintercept = mean_val), linetype="dashed")+ # Vertical line for mean
          theme_classic()

        return(gp)
      }
    })

    output$histclim <- renderPlot({histclim()}, width = 800, height = 600)

    output$histInfo <- renderText({
      if (!is.null(selected_points()) && length(selected_points()) >= 2) {
        return("The histogram below shows the distribution for the selected area on the map.")
      } else {
        return("The histogram below shows the distribution for all available points.")
      }
    })


    output$downloadHistSVG <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".svg", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = histclim(), device = "svg", dpi = 600)

      },
      contentType = "image/svg+xml"
    )

    output$downloadHistPNG <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = histclim(), device = "png", dpi = 600)

      },
      contentType = "image/png"
    )
    ### Summary table ----

    summary_clim <- reactive({
      if (!is.null(selected_points()) && length(selected_points()) >= 2) {
        pop_clim <- merged_climdata()[merged_climdata()[[CommunColumn()]] %in% selected_points(), ]
        clim <- pop_clim[[ColumnSelectForColor()]]
      } else {
        pop_clim <- merged_climdata()
        clim <- pop_clim[[ColumnSelectForColor()]]
      }

      min <- round(min(clim, na.rm = TRUE), 3)
      max <- round(max(clim, na.rm = TRUE), 3)
      mean <- round(mean(clim, na.rm = TRUE), 3)
      med <- round(quantile(clim, 0.5, na.rm = TRUE), 3)
      q1 <- round(quantile(clim, 0.25, na.rm = TRUE), 3)
      q3 <- round(quantile(clim, 0.75, na.rm = TRUE), 3)
      std <- round(sd(clim, na.rm = TRUE), 3)

      summary <- data.frame(
        Variable = ColumnSelectForColor(),
        Min = min,
        Max = max,
        Mean = mean,
        Median = med,
        Q1 = q1,
        Q3 = q3,
        Std = std
      )

      return(summary)
    })



    summary_all_numeric <- reactive({
      req(merged_climdata())

      df <- merged_climdata()
      numeric_vars <- df[, sapply(df, is.numeric), drop = FALSE]

      numeric_vars <- numeric_vars[, !names(numeric_vars) %in% c("Latitude", "Longitude")]

      stats_df <- data.frame(
        Variable = character(),
        Min = numeric(),
        Max = numeric(),
        Mean = numeric(),
        Median = numeric(),
        Q1 = numeric(),
        Q3 = numeric(),
        Std = numeric(),
        stringsAsFactors = FALSE
      )

      for (var in names(numeric_vars)) {
        values <- numeric_vars[[var]]
        stats_df <- rbind(stats_df, data.frame(
          Variable = var,
          Min = round(min(values, na.rm = TRUE), 3),
          Max = round(max(values, na.rm = TRUE), 3),
          Mean = round(mean(values, na.rm = TRUE), 3),
          Median = round(median(values, na.rm = TRUE), 3),
          Q1 = round(quantile(values, 0.25, na.rm = TRUE), 3),
          Q3 = round(quantile(values, 0.75, na.rm = TRUE), 3),
          Std = round(sd(values, na.rm = TRUE), 3),
          stringsAsFactors = FALSE
        ))
      }

      return(stats_df)
    })


    output$summary<- DT::renderDataTable({
      req(summary_clim())
      DT::datatable(
        summary_clim(),
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'Blfrtip',
          buttons = list(
            list(extend = 'copy', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'excel', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'pdf', exportOptions = list(modifier = list(page = "all")))
          ),
          scrollX = TRUE,
          paging = TRUE,
          pageLength = 10,
          autoWidth = FALSE
        ),
        escape = FALSE
      )
    }, server = FALSE)


    output$summary_all<- DT::renderDataTable({
      req(summary_all_numeric())


      DT::datatable(
        summary_all_numeric(),
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'Blfrtip',
          buttons = list(
            list(extend = 'copy', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'excel', exportOptions = list(modifier = list(page = "all"))),
            list(extend = 'pdf', exportOptions = list(modifier = list(page = "all")))
          ),
          scrollX = TRUE,
          paging = TRUE,
          pageLength = 10,
          autoWidth = FALSE
        ),
        escape = FALSE
      )
    }, server = FALSE)



  })
}

## To be copied in the UI
# mod_Stat_structure_ui("Stat_structure_1")

## To be copied in the server
# mod_Stat_structure_server("Stat_structure_1")

#' Correlation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import DT
#' @import viridis
#' @import colourpicker
#' @importFrom colourpicker colourInput
#' @import shinydashboard
#' @import reshape2
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Correlation_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Correlation",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "height: 90vh; overflow-y: auto;",
        h3("Select Variables for Correlation"),

        checkboxInput(ns("showColumnsCorr"), "Select Variables for Correlation", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("showColumnsCorr"), "'] == true"),
          uiOutput(ns("selectNumericVars"))
        ),
        hr(),
        actionButton(ns("runCorr"), "Run Correlation")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Correlation Matrix",
                   DT::DTOutput(ns("corrMatrix"))
          ),
          tabPanel("Correlation Pvalue Matrix",
                   DT::DTOutput(ns("corrPval"))
          ),
          tabPanel("Correlation Plot",
                   uiOutput(ns("corrPlotContainer")),
                   hr(),
                   tags$div(
                     style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px; margin-top: 20px; background-color: #fafafa;",

                     # Box header with toggle button
                     tags$div(
                       style = "display: flex; justify-content: space-between; align-items: center;",
                       tags$h4("Graph Customization", style = "margin: 0;"),
                       actionButton(
                         ns("toggleCorrOptions"),
                         label = NULL,
                         icon = icon("minus"),
                         class = "btn btn-default btn-sm toggle-box-btn",
                         `data-target` = ns("corrCustomizationBox")
                       )
                     ),

                     # Collapsible content
                     tags$div(
                       id = ns("corrCustomizationBox"),
                       fluidRow(
                         column(4, sliderInput(ns("corrThreshold"), "Minimum Absolute Correlation", min = 0, max = 1, value = 0, step = 0.05)),
                         column(4, sliderInput(ns("corrPlotSize"), "Image Width (px)", min = 400, max = 1600, value = 800, step = 100)),
                         column(4, sliderInput(ns("corrPlotHeight"), "Image Height (px)", min = 400, max = 1600, value = 600, step = 100))
                       ),
                       fluidRow(
                         column(4, checkboxInput(ns("showValues"), "Show Correlation Coefficients", FALSE)),
                         column(4, checkboxInput(ns("useColor"), "Use Color Gradient", TRUE))
                       ),
                       fluidRow(
                         column(4, colourInput(ns("lowColor"), "Low Color", value = "blue")),
                         column(4, colourInput(ns("midColor"), "Mid Color", value = "white")),
                         column(4, colourInput(ns("highColor"), "High Color", value = "red"))
                       )
                     )
                   ) ,
                   hr(),
                   actionButton(ns("openDownloadCorrModal"), "Download Image")
          )
        )



      )
    )
  )
}

#' Correlation Server Functions
#'
#' @noRd
mod_Correlation_server <- function(id, merged_climdata, DownloadFormatCorr, OpenDownloadModalCorr, RunCorr,
                                   SelectedVarsCorr, CorrThresholdMod, UseColorCorr, LowColorCorr,MidColorCorr,
                                   HighColorCorr, ShowValuesCorr, CorrPlotSize, CorrPlotHeight,CorrPngBgColor,
                                   CorrDownloadWidth, CorrDownloadHeight,CorrDownloadDpi
                                   ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    selectedCols_default <- reactiveVal(NULL)

    observe({
      req(merged_climdata())
      DownloadFormatCorr(input$corrDownloadFormat)
      OpenDownloadModalCorr(input$openDownloadCorrModal)
      RunCorr(input$runCorr)
      SelectedVarsCorr(input$selectedVars)
      CorrThresholdMod(input$corrThreshold)
      UseColorCorr(input$useColor)
      LowColorCorr(input$lowColor)
      MidColorCorr(input$midColor)
      HighColorCorr(input$highColor)
      ShowValuesCorr(input$showValues)
      CorrPlotSize(input$corrPlotSize)
      CorrPlotHeight(input$corrPlotHeight)
      CorrPngBgColor(input$corrPngBgColor)
      CorrDownloadWidth(input$corrDownloadWidth)
      CorrDownloadHeight(input$corrDownloadHeight)
      CorrDownloadDpi(input$corrDownloadDpi)

    })




    observeEvent(merged_climdata(), {
      df <- merged_climdata()
      numeric_vars <- names(df)[sapply(df, is.numeric)]

      if (is.null(selectedCols_default())) {
        selectedCols_default(numeric_vars)
      }
    }, once = TRUE)

    # output$selectNumericVars <- renderUI({
    #   req(merged_climdata())
    #   df <- merged_climdata()
    #   numeric_vars <- names(df)[sapply(df, is.numeric)]
    #   checkboxGroupInput(ns("selectedVars"), "Numeric Variables", choices = numeric_vars, selected = numeric_vars)
    # })


    output$selectNumericVars <- renderUI({
      req(merged_climdata())
      df <- merged_climdata()
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      shinyWidgets::pickerInput(
        ns("selectedVars"),
        "Numeric Variables",
        choices = numeric_vars,
        selected = numeric_vars,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `size` = 10
        )
      )
    })

    get_p_matrix <- function(df) {
      n <- ncol(df)
      p_matrix <- matrix(NA, n, n)
      colnames(p_matrix) <- rownames(p_matrix) <- colnames(df)
      for (i in 1:n) {
        for (j in 1:n) {
          if (i != j) {
            test <- cor.test(df[[i]], df[[j]], use = "pairwise.complete.obs")
            p_matrix[i, j] <- test$p.value
          } else {
            p_matrix[i, j] <- NA
          }
        }
      }
      return(p_matrix)
    }


    corrData <- eventReactive(RunCorr(), {
      #req(SelectedVarsCorr())
      df <- merged_climdata()
      cols <- SelectedVarsCorr()%||% selectedCols_default()
      req(cols)
      df_selected <- df[, cols, drop = FALSE]
      cor(df_selected, use = "pairwise.complete.obs")
    })

    output$corrMatrix <- DT::renderDataTable({
      req(corrData())

      DT::datatable(
        round(corrData(), 2),
        rownames = TRUE,
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

    corrPval <- eventReactive(RunCorr(), {
      #req(SelectedVarsCorr())
      df <- merged_climdata()
      cols <- SelectedVarsCorr()%||% selectedCols_default()
      req(cols)
      df_selected <- df[, cols, drop = FALSE]
      get_p_matrix(df_selected)
    })

    output$corrPval<- DT::renderDataTable({
      req(corrPval())
      format_pval <- function(x) {
        if (is.na(x)) return(NA)
        if (x == 0) return("0")
        formatC(x, format = "e", digits = 3)
      }

      formatted_pval <- apply(corrPval(), c(1, 2), format_pval)

      diag(formatted_pval) <- "1"

      DT::datatable(
        formatted_pval,
        rownames = TRUE,
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


    corrPlotReactive <- reactive({
      req(RunCorr())
      df <- merged_climdata()
      cols <- SelectedVarsCorr() %||% selectedCols_default()
      req(cols)

      # If exactly two variables are selected, draw a scatter with regression line + R^2
      if (length(cols) == 2) {
        var1 <- cols[1]
        var2 <- cols[2]
        scatter_df <- df[, cols, drop = FALSE]

        # Compute R^2 from a linear model
        lm_mod <- lm(as.formula(paste(var2, "~", var1)), data = scatter_df)
        r2 <- summary(lm_mod)$r.squared

        # Determine the x,y coordinates at the right end of the line
        x_end <- max(scatter_df[[var1]], na.rm = TRUE)
        # compute corresponding y on the fit line
        # newdata must be named list matching var1
        newdata <- setNames(data.frame(x_end), var1)
        y_end <- predict(lm_mod, newdata)

        # Base ggplot: scatter points
        if (UseColorCorr()) {
          # English comment: color points by density estimate
          dens <- MASS::kde2d(scatter_df[[var1]], scatter_df[[var2]], n = 50)
          ix <- findInterval(scatter_df[[var1]], dens$x)
          iy <- findInterval(scatter_df[[var2]], dens$y)
          scatter_df$density <- dens$z[cbind(ix, iy)]

          p <- ggplot(scatter_df, aes_string(x = var1, y = var2, color = "density")) +
            geom_point(size = 2) +
            scale_color_gradientn(
              colours = c(LowColorCorr(), MidColorCorr(), HighColorCorr()),
              name = "Density"
            ) +
            theme_minimal()
        } else {
          p <- ggplot(scatter_df, aes_string(x = var1, y = var2)) +
            geom_point(size = 2) +
            theme_minimal()
        }

        # English comment: always overlay regression line
        p <- p +
          geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
          labs(
            x     = var1,
            y     = var2,
            title = paste("Scatterplot of", var1, "vs", var2)
          )

        # Add R^2 right at the end of the regression line
        p <- p +
          annotate(
            "text",
            x     = x_end,
            y     = y_end,
            label = paste0("R^2 = ", round(r2, 2)),
            hjust = -0.1,     # nudge a bit to the right
            vjust = -0.5,     # nudge a bit above
            size  = 4
          )


        return(p)
      }

      # Otherwise proceed with your original corrplot-style heatmap
      req(corrData())
      corr_melt <- reshape2::melt(corrData())

      colnames(corr_melt) <- c("Var1", "Var2", "Correlation")

      pval_melt <- reshape2::melt(corrPval())
      colnames(pval_melt) <- c("Var1", "Var2", "Pvalue")

      # Filter based on correlation threshold
      corr_melt <- corr_melt[abs(corr_melt$Correlation) >= CorrThresholdMod(), ]

      corr_melt <- merge(corr_melt, pval_melt, by = c("Var1", "Var2"))

      # Make sure we keep only upper triangle to mimic corrplot style
      corr_melt <- corr_melt[as.numeric(factor(corr_melt$Var1)) >= as.numeric(factor(corr_melt$Var2)), ]

      corr_melt$tooltip <- paste0(
        "Var 1: ", corr_melt$Var1,
        "<br>Var 2: ", corr_melt$Var2,
        "<br>Corr: ", round(corr_melt$Correlation, 2),
        "<br>P-value: ", signif(corr_melt$Pvalue, 3)
      )

      p <- ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation, text = tooltip)) +
        geom_tile() +
        coord_fixed() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(panel.grid = element_blank())

      if (UseColorCorr()) {
        p <- p + scale_fill_gradient2(
          low = LowColorCorr(), mid = MidColorCorr(), high = HighColorCorr(),
          midpoint = 0, limit = c(-1, 1)
        )
      } else {
        p <- p + scale_fill_gradient(low = "gray90", high = "gray40", limit = c(-1, 1))
      }


      if (ShowValuesCorr()) {
        p <- p + geom_text(aes(label = round(Correlation, 2)), color = "black")
      }
      return(p)
    })


    output$corrPlotInteractive <- renderPlotly({
      ggplotly(corrPlotReactive(), tooltip = "text", width = CorrPlotSize(), height = CorrPlotHeight())
    })

    output$corrPlotContainer <- renderUI({
      plotlyOutput(ns("corrPlotInteractive"), height = paste0(CorrPlotHeight(), "px"))
    })


    observeEvent(OpenDownloadModalCorr(), {
      showModal(modalDialog(
        title = "Download Image Settings",

        selectInput(ns("corrDownloadFormat"), "Select Format", choices = c("PNG", "SVG", "HTML")),
        numericInput(ns("corrDownloadWidth"), "Width (in inches)", value = 8, min = 4, max = 20, step = 0.5),
        numericInput(ns("corrDownloadHeight"), "Height (in inches)", value = 6, min = 4, max = 20, step = 0.5),

        conditionalPanel(
          condition = paste0("input['", ns("corrDownloadFormat"), "'] == 'PNG'"),
          numericInput(ns("corrDownloadDpi"), "DPI (only for PNG)", value = 300, min = 72, max = 600, step = 50),
          radioButtons(ns("corrPngBgColor"), "PNG Background Color:",
                       choices = c("White" = "white", "Transparent" = "transparent"),
                       selected = "white")
        ),

        downloadButton(ns("downloadCorrPlot"), "Download Plot", icon = icon("download")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })


    output$downloadCorrPlot <- downloadHandler(
      filename = function() {
        paste0("Correlation_plot.", tolower(DownloadFormatCorr()))
      },
      content = function(file) {
        req(corrPlotReactive())  # Get the stored ggplot

        # Define background color
        bg_color <- ifelse(CorrPngBgColor() == "transparent", NA, "white")

        if (DownloadFormatCorr() == "PNG") {
          ggsave(file, plot = corrPlotReactive(), device = "png",
                 width = CorrDownloadWidth(), height = CorrDownloadHeight(),
                 dpi = CorrDownloadDpi(), bg = bg_color)
        } else if (DownloadFormatCorr() == "SVG") {
          ggsave(file, plot = corrPlotReactive(), device = "svg",
                 width = CorrDownloadWidth(), height = CorrDownloadHeight())
        } else if (DownloadFormatCorr() == "HTML") {
          p <- ggplotly(corrPlotReactive())  # Convert only for HTML
          htmlwidgets::saveWidget(p, file)
        }
      }
    )


  })
}


## To be copied in the UI
# mod_Correlation_ui("Correlation_1")

## To be copied in the server
# mod_Correlation_server("Correlation_1")

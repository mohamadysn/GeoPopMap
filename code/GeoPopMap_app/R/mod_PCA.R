#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom  shiny NS tagList fluidRow column h2 p tabsetPanel tabPanel
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @import FactoMineR
#' @import factoextra
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import DT
#' @import viridis
#' @import colourpicker
#' @importFrom colourpicker colourInput
#' @import missMDA
#' @import shinydashboard
#' @import ape

mod_PCA_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "PCA",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # Enable vertical scrolling in sidebar
        style = "height: 90vh; overflow-y: auto;",

        radioButtons(ns("methodType"), "Analysis Type:",
                     choices = c("PCA" = "pca", "PCoA" = "pcoa"),
                     selected = "pca"),

        h3("Select Variables for PCA"),
        checkboxInput(ns("showColumns"), "Select Variables for PCA or PCOA", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("showColumns"), "'] == true"),
          uiOutput(ns("selectColsUI"))
        ),
        hr(),
        h3("PCA Parameters"),
        numericInput(ns("ncp"), "Number of Principal Components", value = 5, min = 2),
        uiOutput(ns("compSelectUI")),
        checkboxInput(ns("normalizeData"), "Normalize Data", value = TRUE),
        hr(),
        radioButtons(ns("visualisationMode"), "Choose Visualization Mode:",
                     choices = c("Individuals only", "Variables only", "Individuals and Variables"),
                     selected = "Individuals and Variables"),
        h3("Grouping Selection (for Coloring)"),
        uiOutput(ns("groupingUI")),
        hr(),
        actionButton(ns("runPCA"), "Run PCA")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("PCA Statistics",
                   DT::DTOutput(ns("pcaStats")),
                   hr(),
                   conditionalPanel(
                     condition = paste0("output['", ns("varContributions_ready"), "']"),
                     tags$div(
                       style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px; margin-top: 20px; background-color: #fafafa;",

                       # Box header with minimize button
                       tags$div(
                         style = "display: flex; justify-content: space-between; align-items: center;",
                         tags$h4("Variance Explained Plot", style = "margin: 0;"),
                         actionButton(
                           ns("toggleVarianceOptions"),
                           label = NULL,
                           icon = icon("minus"),
                           class = "btn btn-default btn-sm toggle-box-btn",
                           `data-target` = ns("variancePlotBox")
                         )
                       ),
                       # Collapsible content
                       tags$div(
                         id = ns("variancePlotBox"),

                         plotOutput(ns("variancePlot")),
                         hr(),
                         # Plot types
                         fluidRow(
                           column(6,
                                  div(class = "tooltip-custom",
                                    selectInput(
                                      ns("variancePlotType"),
                                      "Variance Plot Type",
                                      choices = c("bar", "line"),
                                      selected = "bar",
                                      multiple = TRUE
                                    ),
                                    span(class = "tooltiptext", "Choose between bar plot and/or line plot of explained variance.")
                                  )
                           ),
                           column(6,
                                  div(class = "tooltip-custom",
                                    selectInput(
                                      ns("lineTypeVariance"),
                                      "Line Type for Variance Curve",
                                      choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                      selected = "solid"
                                    ),
                                    span(class = "tooltiptext", "Choose how the variance line is styled.")
                                  )
                           )
                         ),
                         # Cumulative variance line
                         fluidRow(
                           column(6,
                                  div(class = "tooltip-custom",
                                    checkboxInput(
                                      ns("showCumulativeLine"),
                                      "Show Cumulative Variance Line",
                                      value = TRUE
                                    ),
                                    span(class = "tooltiptext", "Add a line showing the cumulative variance explained by PCs.")
                                  )
                           ),
                           column(6,
                                  conditionalPanel(
                                    condition = paste0("input['", ns("showCumulativeLine"), "'] == true"),
                                    div(class = "tooltip-custom",
                                      selectInput(
                                        ns("lineTypeCumulative"),
                                        "Line Type for Cumulative Line",
                                        choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                        selected = "dashed"
                                      ),
                                      span(class = "tooltiptext", "Select the line style for cumulative variance.")
                                    )
                                  )
                           )
                         ),
                         # Other visual options
                         fluidRow(
                           column(4,
                                  div(class = "tooltip-custom",
                                    numericInput(
                                      ns("numPCs"),
                                      "Number of PCs to Display",
                                      value = 5,
                                      min = 2,
                                      max = 20
                                    ),
                                    span(class = "tooltiptext", "Set how many principal components you want to show.")
                                  )
                           ),
                           column(4,
                                  div(class = "tooltip-custom",
                                    sliderInput(
                                      ns("barWidth"),
                                      "Bar Width",
                                      min = 0.2,
                                      max = 1,
                                      value = 0.7,
                                      step = 0.1
                                    ),
                                    span(class = "tooltiptext", "Adjust the width of the bars (between 0.2 and 1).")
                                  )
                           ),
                           column(4,
                                  div(class = "tooltip-custom",
                                    checkboxInput(
                                      ns("addLabels"),
                                      "Show Values on Bars",
                                      value = TRUE
                                    ),
                                    span(class = "tooltiptext", "Display exact variance percentage on each bar.")
                                  )
                           )
                         )
                       )
                     )
                   )
                   
          ),
          tabPanel("Interactive Plot",
                   conditionalPanel(
                     condition = paste0("output['", ns("varContributions_ready"), "']"),
                     plotlyOutput(ns("pcaPlotInteractive")),
                     conditionalPanel(
                       condition = paste0(
                         "input['", ns("visualisationMode"), "'] == 'Individuals only' || ",
                         "input['", ns("visualisationMode"), "'] == 'Individuals and Variables'"
                       ),
                       checkboxInput(ns("changeColor"), "Change Color", value = FALSE),
                       conditionalPanel(
                         condition = paste0("input['", ns("changeColor"), "'] == true"),
                         uiOutput(ns("colorPickerUI"))
                       )
                     ),
                     hr(),
                     tags$div(
                       style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px; margin-top: 20px; background-color: #fafafa;",

                       # Box header with toggle button
                       tags$div(
                         style = "display: flex; justify-content: space-between; align-items: center;",
                         tags$h4("Graph Customization", style = "margin: 0;"),
                         actionButton(
                           ns("toggleGraphOptions"),
                           label = NULL,
                           icon = icon("minus"),
                           class = "btn btn-default btn-sm toggle-box-btn",
                           `data-target` = ns("graphCustomizationBox")
                         )
                       ),

                       # Collapsible content
                       tags$div(
                         id = ns("graphCustomizationBox"),

                         # Ellipse options
                         div(class = "tooltip-custom",
                           checkboxInput(ns("addEllipses"), "Add Ellipses", value = TRUE),
                           span(class = "tooltiptext", "Display confidence ellipses around groups.")
                         ),

                         conditionalPanel(
                           condition = paste0("input['", ns("addEllipses"), "'] == true"),

                           fluidRow(
                             column(4, div(class = "tooltip-custom",
                               checkboxInput(ns("fillEllipses"), "Fill Ellipses", value = FALSE),
                               span(class = "tooltiptext", "Fill the ellipses with group color.")
                             )),
                             column(4, div(class = "tooltip-custom",
                               selectInput(ns("ellipseType"), "Ellipse Type",
                                 choices = c("convex", "t", "norm", "euclid"), selected = "t"),
                               span(class = "tooltiptext", "Choose the statistical method used to compute ellipses.")
                             )),
                             column(4, div(class = "tooltip-custom",
                               selectInput(ns("ellipseLinetype"), "Ellipse Line Type",
                                 choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                 selected = "solid"),
                               span(class = "tooltiptext", "Select the line style for the ellipse borders.")
                             ))
                           ),

                           fluidRow(
                             column(4,
                                    div(class = "tooltip-custom",
                                      sliderInput(ns("ellipseAlpha"), "Ellipse Transparency", min = 0, max = 1, value = 0.3, step = 0.1),
                                    span(class = "tooltiptext", "Adjust ellipse transparency (0 = transparent, 1 = opaque)")
                                    )
                             )
                           )
                         ),

                         hr(),

                         # Color palette
                         fluidRow(
                           column(4, div(class = "tooltip-custom",
                             selectInput(ns("palette"), "Color Palette", choices = c("Default", "Viridis", "Rainbow"), selected = "Default"),
                             span(class = "tooltiptext", "Choose the color palette for the PCA plot.")
                           )),
                           column(4, div(class = "tooltip-custom",
                             radioButtons(ns("individualDisplayMode"), "Individuals Display Mode:",
                               choices = c("Points", "Labels"),
                               selected = "Points"),
                             span(class = "tooltiptext", "Choose how individuals are shown on the PCA plot.")
                           ))
                         ),
                         
                         hr(),
                         # Layout for scale, point size, arrow width
                         
                         fluidRow(
                           column(4, div(class = "tooltip-custom",
                             numericInput(ns("scaleFactor"), "Variable Scale", value = 0.7, min = 0.1, max = 2, step = 0.1),
                             span(class = "tooltiptext", "Set the scale for variable representation.")
                           )),
                           column(4, div(class = "tooltip-custom",
                             numericInput(ns("pointSize"), "Point Size", value = 2, min = 1, max = 5, step = 0.5),
                             span(class = "tooltiptext", "Set the size of points on the PCA plot.")
                           )),
                        
                         column(4, div(class = "tooltip-custom",
                           numericInput(ns("arrowWidth"), "Arrow Width", value = 0.5, min = 0.1, max = 2, step = 0.1),
                           span(class = "tooltiptext", "Set the thickness of arrows representing variables.")
                         ))
                         ),

                         # Label size and nudge
                         fluidRow(
                           column(4, div(class = "tooltip-custom",
                             numericInput(ns("labelSize"), "Label Size", value = 3, min = 1, max = 10, step = 0.5),
                             span(class = "tooltiptext", "Set the size of variable labels.")
                           )),
                           column(4, div(class = "tooltip-custom",
                             sliderInput(ns("nudgeFactor"), "Label Offset (Nudge)", min = 0.1, max = 1, value = 0.5, step = 0.1),
                             span(class = "tooltiptext", "Set the offset for variable labels.")
                           )),
                           column(4, div(class = "tooltip-custom",
                             sliderInput(ns("arrowAlpha"), "Arrow Transparency",
                               min = 0, max = 1, value = 1, step = 0.1),
                             span(class = "tooltiptext", "Set the transparency of variable arrows.")
                           ))
                         )
                       )
                     )
                     ,
                     hr(),
                     actionButton(ns("openDownloadModal"), "Download Image")
                   )
          ),
          tabPanel("Variables Contributions",
                   DT::DTOutput(ns("varContributions"))
          ),
          tabPanel("Data Export",
                   DT::DTOutput(ns("selectedData"))
          )
        )
      )

    )
  )
}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id,
                           merged_climdata, SelectedCols, NCP , GroupCol,PalettePCA, RunPCA, NormalizeDataPCA,
                           CompX, CompY, NudgeFactorPCAVariables, ScaleFactorPCAVariables,
                           VisualisationModePCA, AddEllipsesPCA, FillEllipsesPCA, EllipseLinetype,
                           EllipseType, EllipseAlpha, ArrowWidthPCABariables, LabelSizePCAVariables,
                           NumPCs, PointSize , BarWidthPCA, LineTypePCAStats, lineTypeCumulativePCAStats, AddLabelsPCAStats,
                           VariancePlotTypePCA, ShowCumulativeLinePCAStats, OpenDownloadModal,
                           DownloadFormatPCA, PngBgColorPCA, DownloadWidthPCAImg, DownloadHeightPDAImg, DownloadDpiPCAImg,
                           IndividualDisplayModePCA, ArrowAlphaPCAVariables, Genotypicdata, ChromosomeVec, PositionVec, SpecialMode, CommonColNameFileGenotypic, CommunColumn
                           ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    `%||%` <- function(a, b) if (!is.null(a)) a else b

    selectedCols_default <- reactiveVal(NULL)
    selectedColsGenotype_default <- reactiveVal(NULL)

    observeEvent(merged_climdata(), {

      df <- if (input$methodType == "pcoa") convert_to_numeric(Genotypicdata()) else merged_climdata()


      numeric_vars <- names(df)[sapply(df, is.numeric)]

      if (is.null(selectedCols_default())) {
        selectedCols_default(numeric_vars)
      }


    }, once = TRUE)


    observe({
      req(input$methodType)

      df <- if (input$methodType == "pcoa") convert_to_numeric(Genotypicdata()) else merged_climdata()

      numeric_vars <- names(df)[sapply(df, is.numeric)]
      if (input$methodType == "pcoa") {
        if (is.null(selectedColsGenotype_default())) {
          selectedColsGenotype_default(numeric_vars)
        }
      }

    })

    # Automatically save thresholds when inputs change
    observe({
      req(merged_climdata())
      SelectedCols(input$selectedCols)
      NCP(input$ncp)
      GroupCol(input$groupCol)
      PalettePCA(input$palette)
      RunPCA(input$runPCA)
      NormalizeDataPCA(input$normalizeData)
      CompX(input$compX)
      CompY(input$compY)
      NudgeFactorPCAVariables(input$nudgeFactor)
      ScaleFactorPCAVariables(input$scaleFactor)
      VisualisationModePCA(input$visualisationMode)
      AddEllipsesPCA(input$addEllipses)
      FillEllipsesPCA(input$fillEllipses)
      EllipseLinetype(input$ellipseLinetype)
      EllipseType(input$ellipseType)
      EllipseAlpha(input$ellipseAlpha)
      ArrowWidthPCABariables(input$arrowWidth)
      LabelSizePCAVariables(input$labelSize)
      NumPCs(input$numPCs)
      PointSize(input$pointSize)
      BarWidthPCA(input$barWidth)
      LineTypePCAStats(input$lineTypeVariance)
      lineTypeCumulativePCAStats(input$lineTypeCumulative)
      AddLabelsPCAStats(input$addLabels)
      VariancePlotTypePCA(input$variancePlotType)
      ShowCumulativeLinePCAStats(input$showCumulativeLine)
      OpenDownloadModal(input$openDownloadModal)
      DownloadFormatPCA(input$downloadFormat)
      PngBgColorPCA(input$pngBgColor)
      DownloadWidthPCAImg(input$downloadWidth)
      DownloadHeightPDAImg(input$downloadHeight)
      DownloadDpiPCAImg(input$downloadDpi)
      IndividualDisplayModePCA(input$individualDisplayMode)
      ArrowAlphaPCAVariables(input$arrowAlpha)
    })

    convert_to_numeric <- function(df) {
      df[] <- lapply(df, function(x) {
        x <- as.character(x)
        x <- gsub(",", ".", x)  # Convertir les virgules en points
        if (all(suppressWarnings(!is.na(as.numeric(x[x != ""])))) && any(x != "")) {
          return(as.numeric(x))
        } else {
          return(x)
        }
      })
      return(df)
    }
    
    get_geno_with_qual <- function() {
      req(Genotypicdata(), merged_climdata(), CommonColNameFileGenotypic())
      geno <- convert_to_numeric(Genotypicdata())
      clim <- merged_climdata()
      colnames(clim)[which(colnames(clim) == CommunColumn())] <- CommonColNameFileGenotypic()
      
      col_commune <- CommonColNameFileGenotypic()

      qual_cols <- names(clim)[sapply(clim, function(col) is.factor(col) || is.character(col))]

      qual_cols <- setdiff(qual_cols, CommonColNameFileGenotypic())
      clim_qual <- clim[, c(col_commune, qual_cols), drop = FALSE]

      geno_qual <- merge(geno, clim_qual, by = col_commune, all.x = FALSE, sort = FALSE)

      rownames(geno_qual) <- geno_qual[[col_commune]]
      
      colnames(geno_qual)[which(colnames(geno_qual) == CommonColNameFileGenotypic())] <- CommunColumn()
      geno_qual
    }

    output$selectColsUI <- renderUI({
      req(merged_climdata())
      if (input$methodType == "pcoa") {
        req(Genotypicdata())
        # df <- convert_to_numeric(Genotypicdata())
        # df <- df[, sapply(df, is.numeric)]
        df <- get_geno_with_qual()
      } else {
        df <- merged_climdata()
      }

      numeric_vars <- names(df)[sapply(df, is.numeric)]
      shinyWidgets::pickerInput(
        ns("selectedCols"),
        "Numerical Variables to Include",
        choices = numeric_vars,
        selected = selectedCols_default(),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `size` = 10
        )
      )
    })

    output$groupingUI <- renderUI({
      if (input$methodType == "pcoa") {
        req(Genotypicdata())
        # df <- convert_to_numeric(Genotypicdata())
        df <- get_geno_with_qual()
      } else {
        req(merged_climdata())
        df <- merged_climdata()
      }

      # Keep only qualitative columns (factor or character)
      qual_cols <- names(df)[sapply(df, function(col) is.factor(col) || is.character(col))]

      # If none, return a message
      if (length(qual_cols) == 0) {
        return(tags$em("No qualitative column available for grouping."))
      }

      selectizeInput(ns("groupCol"), "Grouping Column",
                     choices = c("", qual_cols),
                     selected = "",
                     multiple = FALSE,
                     options = list(placeholder = 'Select a qualitative column'))
    })

    output$compSelectUI <- renderUI({
      req(NCP())
      comps <- as.character(1:NCP())
      tagList(
        selectInput(ns("compX"), "Component for X-axis", choices = comps, selected = "1"),
        selectInput(ns("compY"), "Component for Y-axis", choices = comps, selected = "2")
      )
    })


    # Dynamic UI for Manual Color Selection
    # output$colorPickerUI <- renderUI({
    #   req(GroupCol(), PalettePCA(), RunPCA(), merged_climdata())
    #
    #   df <- merged_climdata()
    #   if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(NULL)
    #
    #   group_levels <- unique(as.character(df[[GroupCol()]]))
    #   if (length(group_levels) == 0) return(NULL)
    #
    #
    #   default_colors <- switch(PalettePCA(),
    #                            "Viridis" = viridis::viridis(length(group_levels)),
    #                            "Rainbow" = rainbow(length(group_levels)),
    #                            "Default" = scales::hue_pal()(length(group_levels)),
    #                            scales::hue_pal()(length(group_levels)))
    #
    #   color_inputs <- lapply(seq_along(group_levels), function(i) {
    #     colourpicker::colourInput(
    #       inputId = ns(paste0("color_", group_levels[i])),
    #                 label = group_levels[i],
    #                 value = default_colors[i])
    #   })
    #
    #   do.call(tagList, color_inputs)
    # })

    output$colorPickerUI <- renderUI({
      if (input$methodType == "pcoa") {
        req(Genotypicdata())
        # df <- convert_to_numeric(Genotypicdata())
        df <- get_geno_with_qual()
      } else {
        req(GroupCol(), PalettePCA(), RunPCA(), merged_climdata())
        df <- merged_climdata()
      }
      if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(NULL)
      group_levels <- unique(as.character(df[[GroupCol()]]))
      if (length(group_levels) == 0) return(NULL)
      default_colors <- switch(PalettePCA(),
                               "Viridis" = viridis::viridis(length(group_levels)),
                               "Rainbow" = rainbow(length(group_levels)),
                               "Default" = scales::hue_pal()(length(group_levels)),
                               scales::hue_pal()(length(group_levels)))
      color_inputs <- lapply(seq_along(group_levels), function(i) {
        colourpicker::colourInput(
          inputId = ns(paste0("color_", group_levels[i])),
          label = group_levels[i],
          value = default_colors[i]
        )
      })
      return(do.call(tagList, color_inputs))
    })
    
    observeEvent(PalettePCA(), {
      req(GroupCol())
      if (input$methodType == "pcoa") {
        req(Genotypicdata())
        # df <- convert_to_numeric(Genotypicdata())
        df <- get_geno_with_qual()
      } else {
        req(merged_climdata())
        df <- merged_climdata()
      }
      
      if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(NULL)
      group_levels <- unique(as.character(df[[GroupCol()]]))
      if (length(group_levels) == 0) return(NULL)
      default_colors <- switch(PalettePCA(),
                               "Viridis" = viridis::viridis(length(group_levels)),
                               "Rainbow" = rainbow(length(group_levels)),
                               "Default" = scales::hue_pal()(length(group_levels)),
                               scales::hue_pal()(length(group_levels)))
      lapply(seq_along(group_levels), function(i) {
        colourpicker::updateColourInput(session, inputId = paste0("color_", group_levels[i]), value = default_colors[i])
      })
    })


    # observeEvent(PalettePCA(), {
    #   req(GroupCol(), merged_climdata())
    #
    #   df <- merged_climdata()
    #   if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(NULL)
    #   group_levels <- unique(as.character(df[[GroupCol()]]))
    #   if (length(group_levels) == 0) return(NULL)
    #   default_colors <- switch(PalettePCA(),
    #                            "Viridis" = viridis::viridis(length(group_levels)),
    #                            "Rainbow" = rainbow(length(group_levels)),
    #                            "Default" = scales::hue_pal()(length(group_levels)),
    #                            scales::hue_pal()(length(group_levels)))
    #
    #   lapply(seq_along(group_levels), function(i) {
    #     colourpicker::updateColourInput(session, inputId = paste0("color_", group_levels[i]), value = default_colors[i])
    #   })
    # })


    dataPCA <- reactive({
      if (input$methodType == "pcoa") {
        req(Genotypicdata())
        # df <- convert_to_numeric(Genotypicdata())
        df <- get_geno_with_qual()
        df <- df[, sapply(df, is.numeric)]
      } else {
        df <- merged_climdata()
      }

      cols <- if (input$methodType == "pcoa") selectedColsGenotype_default() else SelectedCols() %||% selectedCols_default()
      # cols <- SelectedCols() %||% selectedCols_default()

      req(cols)
      df_selected <- df %>% select(all_of(cols))
    })

    grouping <- reactive({
      req(merged_climdata())
      req(pcaRes())

      # df <- if (input$methodType == "pcoa") convert_to_numeric(Genotypicdata()) else merged_climdata()
      df <- if (input$methodType == "pcoa") get_geno_with_qual() else merged_climdata()

      if (GroupCol() == "" || !(GroupCol() %in% names(df))) {
        return(factor(rep("ALL", nrow(df))))  # Default to "None" group
      }
      as.factor(df[[GroupCol()]])
    })

    pcaRes <- eventReactive(RunPCA(), {
      req(dataPCA())

      data_selected <- dataPCA()
      data_selected <- convert_to_numeric(data_selected)
      data_selected <- data_selected[, sapply(data_selected, is.numeric)]

      if (ncol(data_selected) > 0) {
        data_selected <- data_selected[, apply(data_selected, 2, function(x) var(x, na.rm = TRUE) > 0), drop = FALSE]
      }
      
      
      if (ncol(data_selected) == 0) {
        showNotification("No variable with variance > 0 for PCA.", type = "error")
        return(NULL)
      }

      # Case 1: PCoA
      if (input$methodType == "pcoa") {
        

        if (any(is.na(data_selected))) {
          showNotification("PCoA does not support missing values. Please impute or clean your data.", type = "error")
          return(NULL)
        }

        dist_mat <- dist(data_selected)  # Default = Euclidean distance
        pcoa_result <- ape::pcoa(dist_mat)
        return(pcoa_result)
      }
      # Case 2: PCA (with or without missing values)
      # Check for missing data, use PCA() with imputation if any NAs exist
      if (any(is.na(data_selected))) {
        imputed_data <- missMDA::imputePCA(data_selected, ncp = NCP())$completeObs
        pca_result <- prcomp(imputed_data, scale. = NormalizeDataPCA())
      } else {
        pca_result <- prcomp(data_selected, scale. = NormalizeDataPCA())  # Using prcomp for standard PCA
      }
      return(pca_result)
    })

    # selected_colors <- reactive({
    #   req(merged_climdata())
    #   df <- merged_climdata()
    #   if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(c("ALL" = "gray40"))
    #
    #   group_levels <- unique(as.character(df[[GroupCol()]]))
    #   if (length(group_levels) == 0) return(NULL)
    #   default_colors <- switch(PalettePCA(),
    #                            "Viridis" = viridis::viridis(length(group_levels)),
    #                            "Rainbow" = rainbow(length(group_levels)),
    #                            "Default" = scales::hue_pal()(length(group_levels)),
    #                            scales::hue_pal()(length(group_levels)))
    #
    #
    #
    #   colors <- sapply(seq_along(group_levels), function(i) {
    #     val <- input[[paste0("color_", group_levels[i])]]
    #     if (!is.null(val)) val else default_colors[i]
    #   })
    #
    #   names(colors) <- group_levels
    #   colors
    # })

    selected_colors <- reactive({
      if (input$methodType == "pcoa") {
        req(Genotypicdata())
        # df <- convert_to_numeric(Genotypicdata())
        df <- get_geno_with_qual()
      } else {
        req(merged_climdata())
        df <- merged_climdata()
      }
      if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(c("ALL" = "gray40"))
      group_levels <- unique(as.character(df[[GroupCol()]]))
      if (length(group_levels) == 0) return(NULL)
      default_colors <- switch(PalettePCA(),
                               "Viridis" = viridis::viridis(length(group_levels)),
                               "Rainbow" = rainbow(length(group_levels)),
                               "Default" = scales::hue_pal()(length(group_levels)),
                               scales::hue_pal()(length(group_levels)))
      colors <- sapply(seq_along(group_levels), function(i) {
        val <- input[[paste0("color_", group_levels[i])]]
        if (!is.null(val)) val else default_colors[i]
      })
      names(colors) <- group_levels
      colors
    })

    output$pcaStats <- DT::renderDataTable({
      req(pcaRes())

      if (input$methodType == "pcoa") {
        eigenvalues <- pcaRes()$values$Eigenvalues
        variance_explained <- pcaRes()$values$Relative_eig * 100
      } else {
        eigenvalues <- pcaRes()$sdev^2
        variance_explained <- (eigenvalues / sum(eigenvalues)) * 100
      }

      cumulative_variance <- cumsum(variance_explained)
      PC = factor(paste0("PC", seq_along(eigenvalues)), levels = paste0("PC", seq_along(eigenvalues)))

      stats <- data.frame(
        "PC" = PC,
        "Eigenvalue" = eigenvalues,
        "Percentage of Variance" = round(variance_explained, 2),
        "Cumulative Variance" = round(cumulative_variance, 2)
      )

      DT::datatable(
        stats,
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

    output$varContributions <- DT::renderDataTable({
      req(pcaRes())

      # Only compute variable contributions for PCA
      if (input$methodType != "pca") {
        return(DT::datatable(
          data.frame(Note = "Variable contributions are only available for PCA."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      loadings <- pcaRes()$rotation^2  # Squared loadings
      eigenvalues <- pcaRes()$sdev^2  # Eigenvalues
      contrib <- sweep(loadings, 2, eigenvalues, FUN = "*")  # Multiply by eigenvalues
      contrib <- sweep(contrib, 2, colSums(contrib), FUN = "/") * 100  # Normalize to percentage
      DT::datatable(
        as.data.frame(round(contrib, 2)),
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

    output$varContributions_ready <- reactive({
      req(pcaRes())  # Only activate when PCA has been computed
      TRUE
    })

    outputOptions(output, "varContributions_ready", suspendWhenHidden = FALSE)

    pcaPlotReactive <- reactive({
      req(pcaRes(), merged_climdata(), selected_colors())

      # df <- if (input$methodType == "pcoa") convert_to_numeric(Genotypicdata()) else merged_climdata()
      df <- if (input$methodType == "pcoa") get_geno_with_qual() else merged_climdata()

      
      if (GroupCol() != "" || (GroupCol() %in% names(df))){
        req(df[[GroupCol()]])
      }


      # Extract coordinates based on method
      coords <- if (input$methodType == "pcoa") {
        vectors <- pcaRes()$vectors
        if (is.null(vectors) || ncol(vectors) == 0) return(NULL)
        df_coords <- as.data.frame(vectors)
        colnames(df_coords) <- paste0("PC", seq_len(ncol(df_coords)))
        df_coords
      } else {
        x <- pcaRes()$x
        if (is.null(x) || ncol(x) == 0) return(NULL)
        df_coords <- as.data.frame(x)
        colnames(df_coords) <- paste0("PC", seq_len(ncol(df_coords)))
        df_coords
      }

      # Set rownames
      if (!CommunColumn() %in% colnames(df)) {
        showNotification("Column 'Code' not found in your dataset.", type = "error")
        return(NULL)
      }

      rownames(coords) <- df[[CommunColumn()]]

      # Axis names
      compX_name <- paste0("PC", CompX())
      compY_name <- paste0("PC", CompY())

      # Variance explained
      if (input$methodType == "pcoa") {
        eigenvalues <- pcaRes()$values$Eigenvalues
        variance_expl <- pcaRes()$values$Relative_eig * 100
      } else {
        eigenvalues <- pcaRes()$sdev^2
        variance_expl <- (eigenvalues / sum(eigenvalues)) * 100
      }

      x_label <- paste0(compX_name, " (", round(variance_expl[as.numeric(CompX())], 1), "%)")
      y_label <- paste0(compY_name, " (", round(variance_expl[as.numeric(CompY())], 1), "%)")

      # Individuals data
      plot_data <- data.frame(
        Name = rownames(coords),
        Group = as.character(grouping()),
        X = coords[[compX_name]],
        Y = coords[[compY_name]]
      )

      plot_data$tooltip_text <- paste0(
        "<b>Name:</b> ", plot_data$Name,
        "<br><b>Group:</b> ", plot_data$Group,
        "<br><b>Coordinates:</b> (", round(plot_data$X, 2), ", ", round(plot_data$Y, 2), ")"
      )

      colors <- selected_colors()

      plot_title <- switch(VisualisationModePCA(),
                           "Individuals only" = "PCA - Visualization of Individuals",
                           "Variables only" = "PCA - Visualization of Variables",
                           "Individuals and Variables" = "PCA - Visualization of Individuals and Variables"
      )

      # Base plot
      p0 <- ggplot() +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        labs(title = plot_title, x = x_label, y = y_label) +
        theme_minimal() +
        theme(panel.grid = element_blank())

      # Show individuals
      if (VisualisationModePCA() %in% c("Individuals only", "Individuals and Variables")) {
        if (IndividualDisplayModePCA() == "Labels") {
          p1 <- p0 + geom_text(data = plot_data, aes(x = X, y = Y, label = Name, color = Group), size = PointSize())
        } else {
          p1 <- p0 + geom_point(data = plot_data, aes(x = X, y = Y, color = Group, text = tooltip_text), size = PointSize())
        }
        p1 <- p1 + scale_color_manual(values = colors)
      } else {
        p1 <- p0
      }

      # Add ellipses
      if (AddEllipsesPCA() && VisualisationModePCA() %in% c("Individuals only", "Individuals and Variables")) {
        ellipse_groups <- plot_data %>%
          filter(Group %in% names(table(Group))[table(Group) >= 3])
        if (FillEllipsesPCA()) {
          p1 <- p1 + stat_ellipse(data = ellipse_groups,
                                  aes(x = X, y = Y, group = Group, fill = Group, color = Group),
                                  type = EllipseType(), geom = "polygon", alpha = EllipseAlpha(), linetype = EllipseLinetype())
        } else {
          p1 <- p1 + stat_ellipse(data = ellipse_groups,
                                  aes(x = X, y = Y, group = Group, color = Group),
                                  type = EllipseType(), linetype = EllipseLinetype())
        }
      }

      # Variables Data (Arrows) — PCA only
      if (input$methodType == "pca" && VisualisationModePCA() %in% c("Individuals and Variables", "Variables only")) {
        var_coords <- as.data.frame(pcaRes()$rotation)
        colnames(var_coords) <- paste0("PC", seq_len(ncol(var_coords)))

        r <- min(
          (max(plot_data$X) - min(plot_data$X)) / (max(var_coords[, compX_name]) - min(var_coords[, compX_name])),
          (max(plot_data$Y) - min(plot_data$Y)) / (max(var_coords[, compY_name]) - min(var_coords[, compY_name]))
        )
        scale_factor <- r * ScaleFactorPCAVariables()

        var_data <- data.frame(
          Variable = rownames(var_coords),
          Xstart = 0, Ystart = 0,
          Xend = var_coords[, compX_name] * scale_factor,
          Yend = var_coords[, compY_name] * scale_factor,
          Xlabel = var_coords[, compX_name] * scale_factor + sign(var_coords[, compX_name]) * NudgeFactorPCAVariables(),
          Ylabel = var_coords[, compY_name] * scale_factor + sign(var_coords[, compY_name]) * NudgeFactorPCAVariables()
        )

        p1 <- p1 +
          geom_segment(data = var_data, aes(x = Xstart, y = Ystart, xend = Xend, yend = Yend),
                       arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                       color = "gray60", alpha = ArrowAlphaPCAVariables(), linewidth = ArrowWidthPCABariables()) +
          geom_text(data = var_data, aes(x = Xlabel, y = Ylabel, label = Variable),
                    color = "black", size = LabelSizePCAVariables())
      }

      return(p1)
    })


    # pcaPlotReactive <- reactive({
    #   req(pcaRes())
    #   req(merged_climdata())
    #   req(selected_colors())
    #   df <- merged_climdata()
    #
    #   if (GroupCol() == "" || !(GroupCol() %in% names(df))) return(NULL)
    #   req(df[[GroupCol()]])
    #
    #   # Get coordinates from PCA or PCoA
    #   coords <- if (input$methodType == "pcoa") {
    #     vectors <- pcaRes()$vectors
    #     if (is.null(vectors) || ncol(vectors) == 0) return(NULL)
    #     df_coords <- as.data.frame(vectors)
    #     colnames(df_coords) <- paste0("PC", seq_len(ncol(df_coords)))
    #     df_coords
    #   } else {
    #     x <- pcaRes()$x
    #     if (is.null(x) || ncol(x) == 0) return(NULL)
    #     df_coords <- as.data.frame(x)
    #     colnames(df_coords) <- paste0("PC", seq_len(ncol(df_coords)))
    #     df_coords
    #   }
    #
    #   # Set row names
    #   if (!"Code" %in% colnames(df)) {
    #     showNotification("Column 'Code' not found. Please check your dataset.", type = "error")
    #     return(NULL)
    #   }
    #   rownames(coords) <- df$Code
    #
    #
    #   compX_name <- paste0("PC", CompX())
    #   compY_name <- paste0("PC", CompY())
    #
    #   if (input$methodType == "pcoa") {
    #     eigenvalues <- pcaRes()$values$Eigenvalues
    #     variance_expl <- pcaRes()$values$Relative_eig * 100
    #   } else {
    #     eigenvalues <- pcaRes()$sdev^2
    #     variance_expl <- (eigenvalues / sum(eigenvalues)) * 100
    #   }
    #
    #
    #   x_label <- paste0(compX_name, " (", round(variance_expl[as.numeric(CompX())], 1), "%)")
    #   y_label <- paste0(compY_name, " (", round(variance_expl[as.numeric(CompY())], 1), "%)")
    #
    #   # Individuals Data
    #   plot_data <- data.frame(
    #     Name = rownames(coords),
    #     Group = as.character(grouping()),
    #     X = coords[, compX_name],
    #     Y = coords[, compY_name]
    #   )
    #
    #   plot_data$tooltip_text <- paste0(
    #     "<b>Name:</b> ", plot_data$Name,
    #     "<br><b>Group:</b> ", plot_data$Group,
    #     "<br><b>Coordinates:</b> (", round(plot_data$X, 2), ", ", round(plot_data$Y, 2), ")"
    #   )
    #
    #
    #   # Variables Data (Arrows)
    #   var_coords <- as.data.frame(pcaRes()$rotation)  # For prcomp, the variable loadings are stored in 'rotation'
    #   colnames(var_coords) <- paste0("PC", 1:ncol(var_coords))
    #
    #   # ** Scale Variables Separately from Individuals**
    #   r <- min(
    #     (max(plot_data$X) - min(plot_data$X)) / (max(var_coords[, compX_name]) - min(var_coords[, compX_name])),
    #     (max(plot_data$Y) - min(plot_data$Y)) / (max(var_coords[, compY_name]) - min(var_coords[, compY_name]))
    #   )
    #
    #   scale_factor <- r * ScaleFactorPCAVariables()   # **User-defined Scale Factor**
    #
    #   var_data <- data.frame(
    #     Variable = rownames(var_coords),
    #     Xstart = 0, Ystart = 0,
    #     Xend = var_coords[, compX_name] * scale_factor,
    #     Yend = var_coords[, compY_name] * scale_factor,
    #     Xlabel = var_coords[, compX_name] * scale_factor + sign(var_coords[, compX_name]) * NudgeFactorPCAVariables(),
    #     Ylabel = var_coords[, compY_name] * scale_factor + sign(var_coords[, compY_name]) * NudgeFactorPCAVariables()
    #   )
    #
    #   # Filter Groups for Ellipses
    #   ellipse_groups <- plot_data %>%
    #     filter(Group %in% names(table(Group))[table(Group) >= 3])
    #
    #   colors <- selected_colors()
    #
    #   plot_title <- switch(VisualisationModePCA(),
    #                        "Individuals only" = "PCA - Visualization of Individuals",
    #                        "Variables only" = "PCA - Visualization of Variables",
    #                        "Individuals and Variables" = "PCA - Visualization of Individuals and Variables")
    #
    #
    #   # Base Plot
    #   p0 <- ggplot() +
    #     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    #     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    #     labs(title = plot_title,
    #          x = x_label, y = y_label) +
    #     theme_minimal() +
    #     theme(panel.grid = element_blank())
    #
    #   # Show Individuals if Selected
    #   if (VisualisationModePCA() %in% c("Individuals only", "Individuals and Variables")) {
    #     if (IndividualDisplayModePCA() == "Labels") {
    #       # Display individuals as text labels
    #       if (AddEllipsesPCA()) {
    #         p1 <- p0 + geom_text(data = plot_data, aes(x = X, y = Y, label = Name, color = Group), size = PointSize()) +
    #           scale_color_manual(values = colors)
    #       } else {
    #         p <- p0 + geom_text(data = plot_data, aes(x = X, y = Y, label = Name, color = Group), size = PointSize()) +
    #           scale_color_manual(values = colors)
    #       }
    #     } else {
    #       # Display individuals as points
    #       if (AddEllipsesPCA()) {
    #         p1 <- p0 + geom_point(data = plot_data, aes(x = X, y = Y, color = Group, text = tooltip_text), size = PointSize()) +
    #           scale_color_manual(values = colors)
    #       } else {
    #         p <- p0 + geom_point(data = plot_data, aes(x = X, y = Y, color = Group, text = tooltip_text), size = PointSize()) +
    #           scale_color_manual(values = colors)
    #       }
    #     }
    #   }
    #
    #
    #   # Add Ellipses Only If Individuals Are Displayed
    #   if (AddEllipsesPCA() && VisualisationModePCA() %in% c("Individuals only", "Individuals and Variables")) {
    #     if (FillEllipsesPCA()) {
    #       p <- p1 + stat_ellipse(data = ellipse_groups, aes(x = X, y = Y, group = Group, fill = Group, color = Group),
    #                              type = EllipseType(), geom = "polygon", alpha = EllipseAlpha(), linetype = EllipseLinetype())
    #     } else {
    #       p <- p1 + stat_ellipse(data = ellipse_groups, aes(x = X, y = Y, group = Group, color = Group),
    #                              type = EllipseType(), linetype = EllipseLinetype())
    #     }
    #   }
    #   # Add Variable Arrows if Selected
    #   if (VisualisationModePCA() %in% c("Individuals and Variables")) {
    #     p <- p +
    #       geom_segment(data = var_data, aes(x = Xstart, y = Ystart, xend = Xend, yend = Yend),
    #                    arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "gray60", alpha = ArrowAlphaPCAVariables(), linewidth = ArrowWidthPCABariables()) +
    #       geom_text(data = var_data, aes(x = Xlabel, y = Ylabel, label = Variable), color = "black", size = LabelSizePCAVariables())
    #   }
    #
    #
    #
    #   # Variables Only Mode
    #   if (VisualisationModePCA() %in% c("Variables only")) {
    #     p <- p0 +
    #       geom_segment(data = var_data, aes(x = Xstart, y = Ystart, xend = Xend, yend = Yend),
    #                    arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = "gray60", alpha = ArrowAlphaPCAVariables(), linewidth = ArrowWidthPCABariables()) +
    #       geom_text(data = var_data, aes(x = Xlabel, y = Ylabel, label = Variable), color = "black", size = LabelSizePCAVariables()) +
    #       scale_fill_manual(values = colors)
    #   }
    #
    #   return(p)
    # })

    output$pcaPlotInteractive <- renderPlotly({
      req(pcaPlotReactive())  # Use the stored ggplot
      ggplotly(pcaPlotReactive(), tooltip = c("text"))  # Convert ggplot to plotly
    })

    output$variancePlot <- renderPlot({
      req(pcaRes())  # Ensure PCA has been computed
      req(merged_climdata())
      df <- merged_climdata()
      # Compute eigenvalues, variance explained, and cumulative variance
      if (input$methodType == "pcoa") {
        eigenvalues <- pcaRes()$values$Eigenvalues
        variance_explained <- pcaRes()$values$Relative_eig * 100
      } else {
        eigenvalues <- pcaRes()$sdev^2
        variance_explained <- (eigenvalues / sum(eigenvalues)) * 100
      }
      cumulative_variance <- cumsum(variance_explained)

      # Get user-selected number of PCs (default = 5, max = total components)
      num_pcs_to_show <- min(NumPCs(), length(eigenvalues))

      # Create dataframe for plotting
      df_var <- data.frame(
        PC = factor(paste0("PC", seq_along(eigenvalues)), levels = paste0("PC", seq_along(eigenvalues))),
        Variance = variance_explained,
        Cumulative = cumulative_variance
      ) %>% slice(1:num_pcs_to_show)  # Show selected PCs only

      # Extract user preferences
      bar_width <- BarWidthPCA()  # User-defined bar width
      linetype <- LineTypePCAStats()   # User-selected line type
      add_labels <- AddLabelsPCAStats()  # Toggle labels

      # Create base plot
      p <- ggplot(df_var, aes(x = PC, y = Variance, group = 1))

      # Add bars if selected
      if ("bar" %in% VariancePlotTypePCA()) {
        p <- p + geom_bar(stat = "identity", fill = "steelblue", color = "black", width = bar_width)
      }

      # Add variance explained line if selected
      if ("line" %in% VariancePlotTypePCA()) {
        p <- p + geom_line(color = "red", linetype = linetype, size = 1) +
          geom_point(color = "red", size = 2)
      }

      # Add cumulative variance line if selected
      if (ShowCumulativeLinePCAStats()) {
        p <- p + geom_line(aes(y = Cumulative), color = "darkgreen", linetype = lineTypeCumulativePCAStats(), size = 1) +
          geom_point(aes(y = Cumulative), color = "darkgreen", size = 2) +
          geom_text(aes(y = Cumulative, label = paste0(round(Cumulative, 1), "%")),
                    vjust = -1.5, size = 4, color = "darkgreen")  # Add cumulative labels
      }

      # Add variance labels on bars if selected
      if (add_labels) {
        p <- p + geom_text(aes(label = paste0(round(Variance, 1), "%")), vjust = -0.5, hjust = 0.6, size = 4)
      }
      y_max <- max(df_var$Cumulative, df_var$Variance) * 1.1  # 10% extra space

      # Finalize plot
      p <- p +
        scale_y_continuous(
          limits = c(0, y_max),
          expand = expansion(mult = c(0, 0.05))
        ) + labs(
          title = "Scree Plot - Variance Explained",
          x = "Principal Components (PC)",
          y = "Percentage of Variance Explained"
        ) + theme_minimal() +
        theme(panel.grid = element_blank())
      return(p)
    })

    output$selectedData <- DT::renderDataTable({

      # df <- if (input$methodType == "pcoa") convert_to_numeric(Genotypicdata()) else merged_climdata()
      df <- if (input$methodType == "pcoa") get_geno_with_qual() else merged_climdata()
      
      
      SelectedData = dataPCA()
      #row.names(SelectedData) <- df[CommunColumn()]
      if (nrow(SelectedData) == length(df[[CommunColumn()]])) {
        row.names(SelectedData) <- df[[CommunColumn()]]
      }
      
      DT::datatable(
        SelectedData,
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
          pageLength = 5,
          autoWidth = FALSE
        ),
        escape = FALSE
      )
    }, server = FALSE)

    observeEvent(OpenDownloadModal(), {
      showModal(modalDialog(
        title = "Download Image Settings",

        selectInput(ns("downloadFormat"), "Select Format", choices = c("PNG", "SVG", "HTML")),

        numericInput(ns("downloadWidth"), "Width (in inches)", value = 8, min = 4, max = 20, step = 0.5),
        numericInput(ns("downloadHeight"), "Height (in inches)", value = 6, min = 4, max = 20, step = 0.5),

        conditionalPanel(
          condition = paste0("input['", ns("downloadFormat"), "'] == 'PNG'"),
          numericInput(ns("downloadDpi"), "DPI (only for PNG)", value = 300, min = 72, max = 600, step = 50),
          radioButtons(ns("pngBgColor"), "PNG Background Color:",
                       choices = c("White" = "white", "Transparent" = "transparent"),
                       selected = "white")
        ),

        downloadButton(ns("downloadPlot"), "Download Plot", icon = icon("download")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })


    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0("PCA_plot.", tolower(DownloadFormatPCA()))
      },
      content = function(file) {
        req(pcaPlotReactive())  # Get the stored ggplot

        # Define background color
        bg_color <- ifelse(PngBgColorPCA() == "transparent", NA, "white")

        if (DownloadFormatPCA() == "PNG") {
          ggsave(file, plot = pcaPlotReactive(), device = "png",
                 width = DownloadWidthPCAImg(), height = DownloadHeightPDAImg(),
                 dpi = DownloadDpiPCAImg(), bg = bg_color)
        } else if (DownloadFormatPCA() == "SVG") {
          ggsave(file, plot = pcaPlotReactive(), device = "svg",
                 width = DownloadWidthPCAImg(), height = DownloadHeightPDAImg())
        } else if (DownloadFormatPCA() == "HTML") {
          p <- ggplotly(pcaPlotReactive())  # Convert only for HTML
          htmlwidgets::saveWidget(p, file)
        }
      }
    )


  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_1")

## To be copied in the server
# mod_PCA_server("PCA_1")

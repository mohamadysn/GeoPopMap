#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @import ggplot2
#' @import shinyjs
#' @import shinyWidgets
#' @import maps
#' @import dplyr
#' @import plotly
#' @import colourpicker
#' @import rcompanion
#' @import multcompView
#' @import multcomp
#' @import car
#' @import svglite
#' @import RColorBrewer
#' @import leaflet
#' @import jsonlite
#' @import chromote
#' @import htmlwidgets
#' @import base64enc
#' @import rsvg
#' @import leaflet.extras
#' @import data.table
#' @import promises
#' @import future
#' @import FactoMineR
#' @import factoextra
#' @import DT
#' @import viridis
#' @import missMDA
#' @import shinydashboard
#' @import ape
#' @importFrom stats dist var
#' @noRd
#'
#'
#'

# options(chromote.chrome.args = c("--no-sandbox"))
options(chromote.chrome_executable = "/usr/bin/google-chrome")
options(shiny.trace = FALSE)
options(shiny.maxRequestSize = 350*1024^2)


app_server <- function(input, output, session) {



  ## Reactive sidebar hide/show button ----
  # Observes clicks on the toggle button and hides or shows the sidebar accordingly
  observeEvent(input$toggleSidebar, {
    sidebar_id <- "sidebar"
    if (input$toggleSidebar %% 2 == 1) {
      hide(selector = paste0("#", sidebar_id))
      updateActionButton(session, "toggleSidebar", icon = icon("chevron-right"))
    } else {
      show(selector = paste0("#", sidebar_id))
      updateActionButton(session, "toggleSidebar", icon = icon("chevron-left"))
    }
  })

  mod_application_overview_server("App_overview")

  ## Files and Data Frames ----

  ### mod_upload_Populations_Data
  InputFilePopulations <- reactiveVal(NULL)
  SepFilePopulations <- reactiveVal(NULL)
  DecFilePopulations <- reactiveVal(NULL)
  HeaderFilePopulations <- reactiveVal(NULL)
  ApplyChangeFilePopulations <- reactiveVal(NULL)
  RenameColFilePopulations <- reactiveVal(NULL)
  NewColNameFilePopulations <- reactiveVal(NULL)
  CommonColNameFilePopulations <- reactiveVal(NULL)

  # Initialize the population data module
  popdata = mod_upload_Populations_Data_server("upload_Populations_Data",
                                                InputFilePopulations = InputFilePopulations,
                                                SepFilePopulations = SepFilePopulations,
                                                DecFilePopulations = DecFilePopulations,
                                                HeaderFilePopulations = HeaderFilePopulations,
                                                ApplyChangeFilePopulations = ApplyChangeFilePopulations,
                                                RenameColFilePopulations = RenameColFilePopulations,
                                                NewColNameFilePopulations = NewColNameFilePopulations,
                                                CommonColNameFilePopulations = CommonColNameFilePopulations)


  # Reactive function to clean and filter uploaded population data
  cleaned_popdata <- reactive({
    req(popdata()) # Ensure data is available before processing

    data <- popdata()

    if (!is.data.frame(data)) {
      showNotification("Error: Uploaded data is not in a valid format.", type = "error")
      return(NULL) # Stop execution if data is invalid
    }

    # Ensure 'Latitude' and 'Longitude' exist in the dataset
    required_cols <- c("Latitude", "Longitude")
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      showNotification(
        paste("Warning: Missing columns:", paste(missing_cols, collapse = ", ")),
        type = "warning"
      )
      return(data)  # Return the unfiltered data instead of stopping execution
    }

    # Filter out rows where both Latitude and Longitude are missing
    popdata_filtered <- data %>%
      dplyr::filter(!is.na(Latitude) | !is.na(Longitude))

    return(popdata_filtered)
  })

  output$populationDataTable <- DT::renderDataTable({

    req(cleaned_popdata()) # Ensure cleaned data is available before rendering

    if (!is.data.frame(cleaned_popdata())) {
      showNotification("Error: Cleaned data is not a data frame.", type = "error")
      return(NULL) # Stop execution if cleaned data is invalid
    }

    # Render DataTable with scrolling and pagination options
    DT::datatable(
      cleaned_popdata(), # Display the network metrics in a table
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




  #####################################################
  ### Clim informations ----

  ### mod_upload_Climatic_Data
  InputFileClimatic <- reactiveVal(NULL)
  SepFileClimatic <- reactiveVal(NULL)
  DecFileClimatic <- reactiveVal(NULL)
  HeaderFileClimatic <- reactiveVal(NULL)
  ApplyChangeFileClimatic <- reactiveVal(NULL)
  RenameColFileClimatic <- reactiveVal(NULL)
  NewColNameFileClimatic <- reactiveVal(NULL)
  CommonColNameFileClimatic <- reactiveVal(NULL)

  Climdata = mod_upload_Climatic_Data_server("upload_Climatic_Data",
                                             InputFileClimatic = InputFileClimatic,
                                             SepFileClimatic = SepFileClimatic,
                                             DecFileClimatic = DecFileClimatic,
                                             HeaderFileClimatic = HeaderFileClimatic,
                                             ApplyChangeFileClimatic = ApplyChangeFileClimatic,
                                             RenameColFileClimatic = RenameColFileClimatic,
                                             NewColNameFileClimatic = NewColNameFileClimatic,
                                             CommonColNameFileClimatic = CommonColNameFileClimatic)


  # Render a DataTable to display the cleaned population data
  output$ClimdataeDataTable <- DT::renderDataTable({

    req(Climdata()) # Ensure cleaned data is available before rendering

    if (!is.data.frame(Climdata())) {
      showNotification("Error: Cleaned data is not a data frame.", type = "error")
      return(NULL) # Stop execution if cleaned data is invalid
    }

    data = Climdata()
    max_col <- 1000
    all_columns <- names(data)

    if (length(all_columns) > max_col) {
      showNotification(paste0("Warning: Too many columns (", length(all_columns), "). Only first ", max_col, " shown in table."), type = "warning")
      limited_columns <- all_columns[1:max_col]

    } else {
      limited_columns <- all_columns
    }

    # Render DataTable with scrolling and pagination options
    DT::datatable(

      Climdata()[,1: length(limited_columns)], # Display the network metrics in a table
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

  #####################################################
  ### Phonotypic informations ----

  ### mod_upload_Phenotypic_Data
  InputFilePhenotypic<- reactiveVal(NULL)
  SepFilePhenotypic<- reactiveVal(NULL)
  DecFilePhenotypic<- reactiveVal(NULL)
  HeaderFilePhenotypic<- reactiveVal(NULL)
  ApplyChangeFilePhenotypic<- reactiveVal(NULL)
  RenameColFilePhenotypic<- reactiveVal(NULL)
  NewColNameFilePhenotypic<- reactiveVal(NULL)
  CommonColNameFilePhenotypic<- reactiveVal(NULL)

  Phenotypicdata = mod_upload_Phenotypic_Data_server("upload_Phenotypic_Data",
                                                     InputFilePhenotypic= InputFilePhenotypic,
                                                     SepFilePhenotypic= SepFilePhenotypic,
                                                     DecFilePhenotypic= DecFilePhenotypic,
                                                     HeaderFilePhenotypic= HeaderFilePhenotypic,
                                                     ApplyChangeFilePhenotypic= ApplyChangeFilePhenotypic,
                                                     RenameColFilePhenotypic= RenameColFilePhenotypic,
                                                     NewColNameFilePhenotypic= NewColNameFilePhenotypic,
                                                     CommonColNameFilePhenotypic= CommonColNameFilePhenotypic)


  # Render a DataTable to display the cleaned Phenotypic data
  output$PhenotypicdataeDataTable <- DT::renderDataTable({

    req(Phenotypicdata()) # Ensure cleaned data is available before rendering

    if (!is.data.frame(Phenotypicdata())) {
      showNotification("Error: Cleaned data is not a data frame.", type = "error")
      return(NULL) # Stop execution if cleaned data is invalid
    }

    # Render DataTable with scrolling and pagination options
    DT::datatable(
      Phenotypicdata(), # Display the network metrics in a table
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

  #####################################################
  ### Genotypic informations ----

  InputFileGenotypic <- reactiveVal(NULL)
  SepFileGenotypic <- reactiveVal(NULL)
  DecFileGenotypic <- reactiveVal(NULL)
  HeaderFileGenotypic <- reactiveVal(NULL)
  ApplyChangeFileGenotypic <- reactiveVal(NULL)
  RenameColFileGenotypic <- reactiveVal(NULL)
  NewColNameFileGenotypic <- reactiveVal(NULL)
  CommonColNameFileGenotypic <- reactiveVal(NULL)
  ChromosomeVec <- reactiveVal(NULL)
  PositionVec <- reactiveVal(NULL)
  SpecialMode <- reactiveVal(FALSE)
  # Appel du module Genotypic
  Genotypicdata <- mod_upload_Genotypic_Data_server("upload_Genotypic_Data",
                                                    InputFileGenotypic = InputFileGenotypic,
                                                    SepFileGenotypic = SepFileGenotypic,
                                                    DecFileGenotypic = DecFileGenotypic,
                                                    HeaderFileGenotypic = HeaderFileGenotypic,
                                                    ApplyChangeFileGenotypic = ApplyChangeFileGenotypic,
                                                    RenameColFileGenotypic = RenameColFileGenotypic,
                                                    NewColNameFileGenotypic = NewColNameFileGenotypic,
                                                    CommonColNameFileGenotypic = CommonColNameFileGenotypic,
                                                    ChromosomeVec = ChromosomeVec,
                                                    PositionVec = PositionVec,
                                                    SpecialMode = SpecialMode)

  output$GenotypicdataDataTable <- DT::renderDataTable({

    req(Genotypicdata$filtered())

    if (!is.data.frame(Genotypicdata$filtered())) {
      showNotification("Error: Cleaned data is not a data frame.", type = "error")
      return(NULL)
    }

    DT::datatable(
      Genotypicdata$filtered(),
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

  #####################################################
  ###Structure informations ----

  ### mod_upload_Structure_Information_Data
  InputFileStructure <- reactiveVal(NULL)
  SepFileStructure <- reactiveVal(NULL)
  DecFileStructure <- reactiveVal(NULL)
  HeaderFileStructure <- reactiveVal(NULL)
  ApplyChangeFileStructure <- reactiveVal(NULL)
  RenameColFileStructure <- reactiveVal(NULL)
  NewColNameFileStructure <- reactiveVal(NULL)
  CommonColNameFileStructure <- reactiveVal(NULL)


  strdata = mod_upload_Structure_Information_Data_server("upload_Structure_Information_Data",
                                              InputFileStructure = InputFileStructure,
                                              SepFileStructure = SepFileStructure,
                                              DecFileStructure = DecFileStructure,
                                              HeaderFileStructure = HeaderFileStructure,
                                              ApplyChangeFileStructure = ApplyChangeFileStructure,
                                              RenameColFileStructure = RenameColFileStructure,
                                              NewColNameFileStructure = NewColNameFileStructure,
                                              CommonColNameFileStructure = CommonColNameFileStructure)


  # Render a DataTable to display the cleaned population data
  output$StructureDataTable <- DT::renderDataTable({

    req(strdata()) # Ensure cleaned data is available before rendering

    if (!is.data.frame(strdata())) {
      showNotification("Error: Cleaned data is not a data frame.", type = "error")
      return(NULL) # Stop execution if cleaned data is invalid
    }

    # Render DataTable with scrolling and pagination options
    DT::datatable(
      strdata(), # Display the network metrics in a table
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
        autoWidth = TRUE             # Automatically adjust column widths
      ),
      # we ensure that HTML code can be displayed
      escape = FALSE
    )
  }, server = FALSE )


  #####################################################

  MergeButton <- reactiveVal(NULL)

  # Activate shinyjs to control UI elements dynamically
  shinyjs::useShinyjs()

  files_loaded <- reactive({
    list(
      pop = !is.null(cleaned_popdata()) && nrow(cleaned_popdata()) > 0,
      clim = !is.null(Climdata()) && nrow(Climdata()) > 0,
      pheno = !is.null(Phenotypicdata()) && nrow(Phenotypicdata()) > 0,
      geno = !is.null(Genotypicdata$filtered()) && nrow(Genotypicdata$filtered()) > 0,
      struct = !is.null(strdata()) && nrow(strdata()) > 0
    )
  })

  # Observe the number of uploaded datasets and show/hide the verification box
  observe({
    input_count <- sum(as.numeric(unlist(files_loaded())), na.rm = TRUE)
    # Show the verification box only if at least two datasets are uploaded
    if (!is.na(input_count) && input_count >= 2) {
      shinyjs::show("verification_box")
    } else {
      shinyjs::hide("verification_box")
    }
  })

  # Observe the verification checkboxes and show/hide the "Run Merge" button accordingly
  observe({
    # Use isTRUE to avoid NA errors
    if (isTRUE(input$check_inputs) && isTRUE(input$check_column_choice) && isTRUE(input$check_column_naming)) {
      shinyjs::show("run_merge")
    } else {
      shinyjs::hide("run_merge")
    }
  })



  # Observe when a new dataset is uploaded after merging and reset the verification process
  observeEvent({
    input$InputDataPop
    input$InputDataClim
    input$InputDataStruct
    input$InputDataPheno
    input$InputDataGeno
  }, {
    # Count the number of uploaded datasets again
    input_count <- sum(input$InputDataPop, input$InputDataClim, input$InputDataStruct, input$InputDataPheno, input$InputDataGeno)

    # If a new dataset is added after the merge process, reset checkboxes and hide the "Run Merge" button
    if (input_count > 2) {
      updateCheckboxInput(session, "check_inputs", value = FALSE)
      updateCheckboxInput(session, "check_column_choice", value = FALSE)
      updateCheckboxInput(session, "check_column_naming", value = FALSE)
      shinyjs::hide("run_merge")
    }
  })


  CheckBoxInputDataPop <- reactiveVal(NULL)
  CheckBoxInputDataClim <- reactiveVal(NULL)
  CheckBoxInputDataStruct <- reactiveVal(NULL)
  CheckBoxInputDataPheno <- reactiveVal(NULL)
  CheckBoxInputDataGeno <- reactiveVal(NULL)


  observe({
    CheckBoxInputDataPop(input$InputDataPop)
    CheckBoxInputDataClim(input$InputDataClim)
    CheckBoxInputDataStruct(input$InputDataStruct)
    CheckBoxInputDataPheno(input$InputDataPheno)
    CheckBoxInputDataGeno(input$InputDataGeno)
  })


  observe({
    MergeButton(input$run_merge)

  })


  #####################################################
  # Reactive value to store the common column name
  CommunColumn <- reactiveVal(NULL)
  AdmixtureThreshold <- reactiveVal(NULL)
  RunMap <- reactiveVal(NULL)


  merged_climdata =  mod_Merge_Data_server("Merge_Data",
                                           MergeButton = MergeButton,
                                           cleaned_popdata = cleaned_popdata,
                                           CommonColNameFilePopulations = CommonColNameFilePopulations,
                                           Climdata = Climdata,
                                           CommonColNameFileClimatic = CommonColNameFileClimatic,
                                           Phenotypicdata = Phenotypicdata,
                                           CommonColNameFilePhenotypic = CommonColNameFilePhenotypic,
                                           Genotypicdata = Genotypicdata$filtered,
                                           CommonColNameFileGenotypic = CommonColNameFileGenotypic,
                                           strdata = strdata,
                                           CommonColNameFileStructure = CommonColNameFileStructure,
                                           CommunColumn = CommunColumn,
                                           AdmixtureThreshold = AdmixtureThreshold,
                                           RunMap = RunMap
  )


  #####################################################

  Map1_react <- reactiveVal(NULL) # Reactive variable to store the latest map
  Map2_react <- reactiveVal(NULL) # Reactive variable to store the latest map
  Map1Type  <- reactiveVal(NULL)
  Map2Type  <- reactiveVal(NULL)
  RegionChoiceMap1 <- reactiveVal(NULL)
  RegionChoiceMap2 <- reactiveVal(NULL)
  ColumnSelectForColorMap1 <- reactiveVal(NULL)
  ColumnSelectForColorMap2 <- reactiveVal(NULL)
  Col1_map1 <- reactiveVal(NULL)
  Col1_map2 <- reactiveVal(NULL)
  Col2_map1 <- reactiveVal(NULL)
  Col2_map2 <- reactiveVal(NULL)
  Col3_map1 <- reactiveVal(NULL)
  Col3_map2 <- reactiveVal(NULL)
  DotSize <- reactiveVal(NULL)
  Map1MarkerClick <- reactiveVal(NULL)
  Map2MarkerClick <- reactiveVal(NULL)
  Map1DrawNewFeature <- reactiveVal(NULL)
  Map2DrawNewFeature <- reactiveVal(NULL)
  ResetSelectionMap1 <- reactiveVal(NULL)
  ResetSelectionMap2 <- reactiveVal(NULL)
  SelectedTableRowsSelectedMap1 <- reactiveVal(NULL)
  SelectedTableRowsSelectedMap2 <- reactiveVal(NULL)
  selected_points_map1 <- reactiveVal(NULL)
  selected_points_map2 <- reactiveVal(NULL)
  selected_points_table_map1 <- reactiveVal(NULL)
  selected_points_table_map2 <- reactiveVal(NULL)

  CheckBoxInputSecondMap <- reactiveVal(NULL)
  color_ui_map1_ready <- reactiveVal(FALSE)
  color_ui_map2_ready <- reactiveVal(FALSE)

  mod_Map_Visualization_server("Map_Visualization",
                               merged_climdata = merged_climdata,
                               cleaned_popdata = cleaned_popdata,
                               Phenotypicdata = Phenotypicdata,
                               RunMap = RunMap,
                               CommunColumn = CommunColumn,
                               CommonColNameFilePopulations = CommonColNameFilePopulations,
                               CommonColNameFilePhenotypic = CommonColNameFilePhenotypic,
                               Map1_react = Map1_react,
                               Map2_react = Map2_react,
                               Map1Type = Map1Type,
                               Map2Type = Map2Type,
                               RegionChoiceMap1 = RegionChoiceMap1,
                               RegionChoiceMap2 = RegionChoiceMap2,
                               ColumnSelectForColorMap1 = ColumnSelectForColorMap1,
                               ColumnSelectForColorMap2 = ColumnSelectForColorMap2,
                               Col1_map1 = Col1_map1,
                               Col1_map2 = Col1_map2,
                               Col2_map1 = Col2_map1,
                               Col2_map2 = Col2_map2,
                               Col3_map1 = Col3_map1,
                               Col3_map2 = Col3_map2,
                               DotSize = DotSize,
                               Map1MarkerClick = Map1MarkerClick,
                               Map2MarkerClick = Map2MarkerClick,
                               Map1DrawNewFeature = Map1DrawNewFeature,
                               Map2DrawNewFeature = Map2DrawNewFeature,
                               ResetSelectionMap1 = ResetSelectionMap1,
                               ResetSelectionMap2 = ResetSelectionMap2,
                               CheckBoxInputDataPheno = CheckBoxInputDataPheno,
                               SelectedTableRowsSelectedMap1 = SelectedTableRowsSelectedMap1,
                               SelectedTableRowsSelectedMap2 = SelectedTableRowsSelectedMap2,
                               selected_points_map1 = selected_points_map1,
                               selected_points_map2 = selected_points_map2,
                               selected_points_table_map1 = selected_points_table_map1,
                               selected_points_table_map2 = selected_points_table_map2,
                               CheckBoxInputSecondMap = CheckBoxInputSecondMap,
                               color_ui_map1_ready = color_ui_map1_ready,
                               color_ui_map2_ready = color_ui_map2_ready)


  #####################################################
  AOVPlot <- reactiveVal(NULL)

  mod_Anova_structure_server("Anova_structure",
                             cleaned_popdata = cleaned_popdata,
                             merged_climdata = merged_climdata,
                             strdata = strdata,
                             ColumnSelectForColor = ColumnSelectForColorMap1,
                             AOVPlot = AOVPlot)

  #####################################################
  BinsHistogram <- reactiveVal(NULL)

  mod_Stat_structure_server("Stat_structure",
                            cleaned_popdata = cleaned_popdata,
                            merged_climdata = merged_climdata,
                            strdata = strdata,
                            ColumnSelectForColor = ColumnSelectForColorMap1,
                            Col1 = Col1_map1,
                            Col2 = Col2_map1,
                            Col3 = Col3_map1,
                            BinsHistogram = BinsHistogram,
                            selected_points = selected_points_map1,
                            CommunColumn = CommunColumn)


  #####################################################
  #PCA
  SelectedCols <- reactiveVal(NULL)
  NCP <- reactiveVal(NULL)
  GroupCol <- reactiveVal(NULL)
  PalettePCA <- reactiveVal(NULL)
  RunPCA <- reactiveVal(NULL)
  NormalizeDataPCA <- reactiveVal(NULL)
  CompX <- reactiveVal(NULL)
  CompY <- reactiveVal(NULL)
  NudgeFactorPCAVariables <- reactiveVal(NULL)
  ScaleFactorPCAVariables <- reactiveVal(NULL)
  VisualisationModePCA <- reactiveVal(NULL)
  AddEllipsesPCA <- reactiveVal(NULL)
  FillEllipsesPCA <- reactiveVal(NULL)
  EllipseLinetype <- reactiveVal(NULL)
  EllipseType <- reactiveVal(NULL)
  EllipseAlpha <- reactiveVal(NULL)
  ArrowWidthPCABariables <- reactiveVal(NULL)
  LabelSizePCAVariables <- reactiveVal(NULL)
  NumPCs <- reactiveVal(NULL)
  PointSize <- reactiveVal(NULL)
  BarWidthPCA <- reactiveVal(NULL)
  LineTypePCAStats <- reactiveVal(NULL)
  lineTypeCumulativePCAStats <- reactiveVal(NULL)
  AddLabelsPCAStats <- reactiveVal(NULL)
  VariancePlotTypePCA <- reactiveVal(NULL)
  ShowCumulativeLinePCAStats <- reactiveVal(NULL)
  OpenDownloadModal <- reactiveVal(NULL)
  DownloadFormatPCA <- reactiveVal(NULL)
  PngBgColorPCA <- reactiveVal(NULL)
  DownloadWidthPCAImg <- reactiveVal(NULL)
  DownloadHeightPDAImg <- reactiveVal(NULL)
  DownloadDpiPCAImg <- reactiveVal(NULL)
  ArrowAlphaPCAVariables <- reactiveVal(1)
  IndividualDisplayModePCA <- reactiveVal(NULL)

  mod_PCA_server("PCA",
                 merged_climdata = merged_climdata,
                 SelectedCols = SelectedCols,
                 NCP = NCP,
                 GroupCol = GroupCol,
                 PalettePCA = PalettePCA,
                 RunPCA  = RunPCA,
                 NormalizeDataPCA = NormalizeDataPCA,
                 CompX = CompX,
                 CompY  = CompY,
                 NudgeFactorPCAVariables = NudgeFactorPCAVariables,
                 ScaleFactorPCAVariables = ScaleFactorPCAVariables,
                 VisualisationModePCA = VisualisationModePCA,
                 AddEllipsesPCA = AddEllipsesPCA,
                 FillEllipsesPCA = FillEllipsesPCA,
                 EllipseLinetype = EllipseLinetype,
                 EllipseType = EllipseType,
                 EllipseAlpha = EllipseAlpha,
                 ArrowWidthPCABariables = ArrowWidthPCABariables,
                 LabelSizePCAVariables = LabelSizePCAVariables,
                 NumPCs = NumPCs,
                 PointSize = PointSize,
                 BarWidthPCA = BarWidthPCA,
                 LineTypePCAStats = LineTypePCAStats,
                 lineTypeCumulativePCAStats = lineTypeCumulativePCAStats,
                 AddLabelsPCAStats = AddLabelsPCAStats,
                 VariancePlotTypePCA = VariancePlotTypePCA,
                 ShowCumulativeLinePCAStats = ShowCumulativeLinePCAStats,
                 OpenDownloadModal = OpenDownloadModal,
                 DownloadFormatPCA = DownloadFormatPCA,
                 PngBgColorPCA = PngBgColorPCA,
                 DownloadWidthPCAImg = DownloadWidthPCAImg,
                 DownloadHeightPDAImg = DownloadHeightPDAImg,
                 DownloadDpiPCAImg = DownloadDpiPCAImg,
                 IndividualDisplayModePCA = IndividualDisplayModePCA,
                 ArrowAlphaPCAVariables = ArrowAlphaPCAVariables,
                 Genotypicdata = Genotypicdata$original,
                 ChromosomeVec = ChromosomeVec,
                 PositionVec = PositionVec,
                 SpecialMode = SpecialMode,
                 CommonColNameFileGenotypic = CommonColNameFileGenotypic,
                 CommunColumn = CommunColumn
                 )

  #####################################################

  OpenDownloadModalCorr <- reactiveVal(NULL)
  DownloadFormatCorr <- reactiveVal(NULL)
  RunCorr <- reactiveVal(NULL)
  SelectedVarsCorr <- reactiveVal(NULL)
  CorrThresholdMod <- reactiveVal(NULL)
  UseColorCorr <- reactiveVal(NULL)
  LowColorCorr <- reactiveVal(NULL)
  MidColorCorr <- reactiveVal(NULL)
  HighColorCorr <- reactiveVal(NULL)
  ShowValuesCorr <- reactiveVal(NULL)
  CorrPlotSize <- reactiveVal(NULL)
  CorrPlotHeight <- reactiveVal(NULL)
  CorrPngBgColor <- reactiveVal(NULL)
  CorrDownloadWidth <- reactiveVal(NULL)
  CorrDownloadHeight <- reactiveVal(NULL)
  CorrDownloadDpi <- reactiveVal(NULL)

  mod_Correlation_server("Correlation",
                         merged_climdata = merged_climdata,
                         DownloadFormatCorr = DownloadFormatCorr,
                         OpenDownloadModalCorr = OpenDownloadModalCorr,
                         RunCorr = RunCorr,
                         SelectedVarsCorr = SelectedVarsCorr,
                         CorrThresholdMod = CorrThresholdMod,
                         UseColorCorr = UseColorCorr,
                         LowColorCorr = LowColorCorr,
                         MidColorCorr = MidColorCorr,
                         HighColorCorr = HighColorCorr,
                         ShowValuesCorr = ShowValuesCorr,
                         CorrPlotSize = CorrPlotSize,
                         CorrPlotHeight = CorrPlotHeight,
                         CorrPngBgColor = CorrPngBgColor,
                         CorrDownloadWidth = CorrDownloadWidth,
                         CorrDownloadHeight = CorrDownloadHeight,
                         CorrDownloadDpi = CorrDownloadDpi)



}

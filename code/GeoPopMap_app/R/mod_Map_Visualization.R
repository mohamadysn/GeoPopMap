#' Map Visualization UI Function
#'
#' @description Shiny module UI for map visualization.
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
#' @import shinyjs
#' @import shinyWidgets
#' @import dplyr
#' @import leaflet
#' @import jsonlite
#' @import chromote
#' @import htmlwidgets
#' @import base64enc
#' @import leaflet.extras
mod_Map_Visualization_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # Enable vertical scrolling in sidebar
        style = "height: 90vh; overflow-y: auto;",
        
        # Map settings header
        tags$h4(strong("Map settings")),
        
        # Variable selection header
        tags$h5(strong("Variable selection for first Map")),
        
        # Dropdown for selecting a column to use for coloring
        selectizeInput(
          inputId = ns("column_select_map1"),
          label = "Choose a column to color Map 1:",
          choices = "",  # NULL
          selected = NULL,
          multiple = FALSE,
          options = list(
            placeholder = 'Search or select a column',
            allowEmptyOption = TRUE
          )
        ),
        
        
        # Dropdown to choose a region for Map1
        selectInput(
          ns("region_choice_map1"),
          label = "Map choice :",
          choices = c("World", "Europe", "France", "Germany", "USA"),
          selected = "Europe"
        ),
        
        # Dropdown to choose the tile type for Map1
        selectInput(
          ns("Map1_type"),
          label = "Map type :",
          choices = c(
            "OpenStreetMap" = "OpenStreetMap.Mapnik",
            "CartoDB Positron" = "CartoDB.Positron",
            "Esri Satellite" = "Esri.WorldImagery"
          ),
          selected = "CartoDB.Positron"
        ),
        
        # Checkbox to toggle the display of the second map
        checkboxInput(ns("SecondMap"), "Show Second Map", value = FALSE),
        conditionalPanel(
          condition =  paste0("input['", ns("SecondMap"), "'] == true"),
          # Header for second map variable selection
          tags$h5(strong("Variable selection for second map")),
          
          # Dropdown to select a column for coloring (Map2)
          selectizeInput(
            inputId = ns("column_select_map2"),
            label = "Choose a column to color Map 2:",
            choices = "",  # NULL
            selected = NULL,
            multiple = FALSE,  # TRUE
            options = list(
              placeholder = 'Search or select a column',
              allowEmptyOption = TRUE
            )
          ),
          
          # Dropdown to choose the region for the map
          selectInput(
            ns("region_choice_map2"),
            label = "Map choice:",
            choices = c("World", "Europe", "France", "Germany", "USA"),
            selected = "Europe"
          ),
          
          # Dropdown to choose the tile type for Map2
          selectInput(
            ns("Map2_type"),
            label = "Map type for phenotypic :",
            choices = c("OpenStreetMap" = "OpenStreetMap.Mapnik",
                        "CartoDB Positron" = "CartoDB.Positron",
                        "Esri Satellite" = "Esri.WorldImagery"),
            selected = "CartoDB.Positron"
          )
        ),
        
        # Slider for adjusting dot size
        sliderInput(
          ns("dot_size"),
          label ="Dot size",
          min = 0, max = 50, value = 5, step = 5
        ),
        
        # Header for point colors selection
        tags$h5(strong("Points colours")),
        
        # Dynamic UI outputs for color selection for both maps.
        uiOutput(ns("col_select_map1")),
        br(),br(),
        uiOutput(ns("col_select_map2"))
      ),
      
      # Main panel to display maps and related outputs
      mainPanel(
        leafletOutput(ns("Map1")),
        br(),
        div(
          style = "display: flex; gap: 10px;",
          actionButton(ns("reset_selection_map1"), "Reset selection"),
          actionButton(ns("openDownloadModalMap1"), "Download Map")
        ),
        br(),
        conditionalPanel(
          condition = paste0("input['", ns("SecondMap"), "'] == true"),
          leafletOutput(ns("Map2")),
          br(),
          actionButton(ns("openDownloadModalMap2"), "Download Map"),
          br(), br()
        ),
        DT::dataTableOutput(ns("selected_table_map1"))
      )
    )
  )
}

#' Map Visualization Server Function
#'
#' @description Shiny module server logic for map visualization.
#'
#' @noRd
mod_Map_Visualization_server <- function(id,
                                         merged_climdata, cleaned_popdata, Phenotypicdata, RunMap, CommunColumn,
                                         CommonColNameFilePopulations, CommonColNameFilePhenotypic, Map1_react,
                                         Map2_react, Map1Type, Map2Type,RegionChoiceMap1, RegionChoiceMap2, ColumnSelectForColorMap1,
                                         ColumnSelectForColorMap2, Col1_map1, Col1_map2, Col2_map1, Col2_map2, Col3_map1,
                                         Col3_map2, DotSize,Map1MarkerClick, Map2MarkerClick, Map1DrawNewFeature, Map2DrawNewFeature,
                                         ResetSelectionMap1, ResetSelectionMap2, CheckBoxInputDataPheno, SelectedTableRowsSelectedMap1,
                                         SelectedTableRowsSelectedMap2, selected_points_map1, selected_points_map2, selected_points_table_map1,
                                         selected_points_table_map2, CheckBoxInputSecondMap, color_ui_map1_ready, color_ui_map2_ready) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ### Update Column Selection Inputs ###
    # Update the choices for the column selection dropdowns based on the dataset.
    observe({
      req(merged_climdata())
      var_names <- colnames(merged_climdata())
      
      # Preserve the previously selected column for Map1
      isolate({
        selected_column <- ColumnSelectForColorMap1()
        if (is.null(selected_column) || !(selected_column %in% var_names) ) {
          selected_column <- ""
        }
        updateSelectizeInput(
          session,
          inputId = "column_select_map1",
          choices = var_names,
          selected = selected_column,
          server = TRUE
        )
        
      })
    })
    
    observe({
      req(merged_climdata())
      var_names <- colnames(merged_climdata())
      
      # Preserve the previously selected column for Map2
      isolate({
        selected_column <- ColumnSelectForColorMap2()
        if (is.null(selected_column) || !(selected_column %in% var_names)) {
          selected_column <- ""
        }
        updateSelectizeInput(
          session,
          inputId = "column_select_map2",
          choices = var_names,
          selected = selected_column,
          server = TRUE
        )
      })
    })
    
    ### Initialize Reactive Values for Colors ###
    # For numeric variables, store a vector of colors.
    num_colors_map1 <- reactiveVal(c("#00CCFF", "#989799", "#FF5500"))
    num_colors_map2 <- reactiveVal(c("#00CCFF", "#989799", "#FF5500"))
    
    # For categorical variables, store a named list of colors.
    category_colors_map1 <- reactiveVal(list())
    category_colors_map2 <- reactiveVal(list())
    
    # Update numeric color values for Map1 when inputs change.
    observe({
      req(ColumnSelectForColorMap1(), merged_climdata())
      var <- merged_climdata()[[ColumnSelectForColorMap1()]]
      if (is.numeric(var)) {
        req(input[[ns("col1_map1")]], input[[ns("col2_map1")]], input[[ns("col3_map1")]])
        num_colors_map1(c(input[[ns("col1_map1")]], input[[ns("col2_map1")]], input[[ns("col3_map1")]]))
      }
    })
    
    # Update numeric color values for Map2 when inputs change.
    observe({
      req(ColumnSelectForColorMap2(), merged_climdata())
      var <- merged_climdata()[[ColumnSelectForColorMap2()]]
      if (is.numeric(var)) {
        req(input[[ns("col1_map2")]], input[[ns("col2_map2")]], input[[ns("col3_map2")]])
        num_colors_map2(c(input[[ns("col1_map2")]], input[[ns("col2_map2")]], input[[ns("col3_map2")]]))
      }
    })
    
    # Update categorical colors for Map1.
    observe({
      req(ColumnSelectForColorMap1(), merged_climdata())
      var <- merged_climdata()[[ColumnSelectForColorMap1()]]
      if (!is.numeric(var)) {
        qual_levels <- unique(as.character(var))
        new_colors <- isolate(category_colors_map1())
        for (level in qual_levels) {
          color_input_id <- paste0("color_", level)
          if (!is.null(input[[color_input_id]])) {
            new_colors[[level]] <- input[[color_input_id]]
          }
        }
        category_colors_map1(new_colors)
      }
    })
    
    # Update categorical colors for Map2.
    observe({
      req(ColumnSelectForColorMap2(), merged_climdata())
      var <- merged_climdata()[[ColumnSelectForColorMap2()]]
      if (!is.numeric(var)) {
        qual_levels <- unique(as.character(var))
        new_colors <- isolate(category_colors_map2())
        
        for (level in qual_levels) {
          color_input_id <- paste0("color_", level)
          if (!is.null(input[[color_input_id]])) {
            new_colors[[level]] <- input[[color_input_id]]
          }
        }
        category_colors_map2(new_colors)
      }
    })
    
    ### Render UI for Color Selection ###
    # For Map1, dynamically generate UI based on whether the variable is numeric or categorical.
    observe({
      output$col_select_map1 <- renderUI({
        req(merged_climdata())
        if (is.null(ColumnSelectForColorMap1()) || ColumnSelectForColorMap1() == "") {
          color_ui_map1_ready(FALSE)
          return(NULL)
        }
        var <- merged_climdata()[[ColumnSelectForColorMap1()]]
        if (is.numeric(var)) {
          # For numeric variables, use a gradient color selection.
          color_ui_map1_ready(TRUE)
          tagList(
            colourpicker::colourInput(ns("col1_map1"), " ", "#00CCFF"),
            colourpicker::colourInput(ns("col2_map1"), " ", "#989799"),
            colourpicker::colourInput(ns("col3_map1"), " ", "#FF5500")
          )
        } else {
          # For categorical variables, generate a color picker for each unique level.
          qual_levels <- unique(as.character(var))
          if (length(qual_levels) == 0) {
            message(" No categorical values found, points will not be displayed.")
            color_ui_map1_ready(FALSE)
            return(NULL)
          }
          default_colors <- if (length(qual_levels) <= 3) {
            RColorBrewer::brewer.pal(length(qual_levels), "Dark2")
          } else {
            colorRampPalette(RColorBrewer::brewer.pal(max(3, min(8, length(qual_levels))), "Dark2"))(length(qual_levels))
          }
          colorInputs <- lapply(seq_along(qual_levels), function(i) {
            level <- qual_levels[i]
            colourpicker::colourInput(ns(paste0("color_", level)), paste("Color for", level), value = default_colors[i])
          })
          color_ui_map1_ready(TRUE)
          return(tagList(colorInputs))
        }
      })
      
      # Same logic for Map2.
      output$col_select_map2 <- renderUI({
        req(merged_climdata())
        if (ColumnSelectForColorMap2() == "") {
          color_ui_map2_ready(FALSE)
          return(NULL)
        }
        var <- merged_climdata()[[ColumnSelectForColorMap2()]]
        if (is.numeric(var)) {
          color_ui_map2_ready(TRUE)
          tagList(
            colourpicker::colourInput(ns("col1_map2"), " ", "#00CCFF"),
            colourpicker::colourInput(ns("col2_map2"), " ", "#989799"),
            colourpicker::colourInput(ns("col3_map2"), " ", "#FF5500")
          )
        } else {
          qual_levels <- unique(as.character(var))
          if (length(qual_levels) == 0) {
            color_ui_map2_ready(FALSE)
            message(" No categorical values found, points will not be displayed.")
            return(NULL)
          }
          default_colors <- if (length(qual_levels) <= 3) {
            RColorBrewer::brewer.pal(length(qual_levels), "Dark2")
          } else {
            colorRampPalette(RColorBrewer::brewer.pal(max(3, min(8, length(qual_levels))), "Dark2"))(length(qual_levels))
          }
          colorInputs <- lapply(seq_along(qual_levels), function(i) {
            level <- qual_levels[i]
            colourpicker::colourInput(ns(paste0("color_", level)), paste("Color for", level), value = default_colors[i])
          })
          color_ui_map2_ready(TRUE)
          tagList(colorInputs)
        }
      })
    })
    
    ### Update Map Layers Based on Color Selections ###
    # When the color inputs (either numeric or categorical) are updated, re-render the maps.
    observeEvent({category_colors_map1()
      num_colors_map1()
    }, {
      req(merged_climdata(), color_ui_map1_ready())
      if (!is.null(ColumnSelectForColorMap1()) && ColumnSelectForColorMap1() != "" && ColumnSelectForColorMap1() %in% colnames(merged_climdata())) {
        
        #req(ColumnSelectForColorMap1())
        isolate({
          data_filtered <- merged_climdata()
          var <- data_filtered[[ColumnSelectForColorMap1()]]
          # Determine color palette based on variable type
          if (is.numeric(var)) {
            req(Col1_map1(), Col2_map1(), Col3_map1())
            colors <- num_colors_map1()
            color_pal <- colorNumeric(palette = colors, domain = var)
          } else {
            qual_levels <- unique(as.character(var))
            if (length(qual_levels) == 0) {
              return(NULL)
            }
            color_list <- category_colors_map1()
            for (level in qual_levels) {
              if (is.null(color_list[[level]])) {
                color_list[[level]] <- "#D3D3D3"
              }
            }
            color_pal <- colorFactor(unlist(color_list), domain = qual_levels)
          }
          
          # Update Map1 using leafletProxy
          leafletProxy(ns("Map1"), session) %>%
            clearTiles() %>%
            clearMarkers() %>%
            clearGroup("selected_points_map1") %>%
            addProviderTiles(Map1Type()) %>%
            addCircleMarkers(
              data = data_filtered,
              lng = ~Longitude, lat = ~Latitude,
              radius = ~DotSize(),
              color = ~color_pal(var),
              popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var),
              layerId = ~Code
            )
          
        })
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Similar observer for Map2
    observeEvent({category_colors_map2()
      num_colors_map2()
    }, {
      req(merged_climdata(), color_ui_map2_ready())
      if (!is.null(ColumnSelectForColorMap2()) && ColumnSelectForColorMap2() != "" && ColumnSelectForColorMap2() %in% colnames(merged_climdata())) {
        #req(ColumnSelectForColorMap2())
        isolate({
          data_filtered <- merged_climdata()
          var <- data_filtered[[ColumnSelectForColorMap2()]]
          if (is.numeric(var)) {
            req(Col1_map2(), Col2_map2(), Col3_map2())
            colors <- num_colors_map2()
            color_pal <- colorNumeric(palette = colors, domain = var)
          } else {
            qual_levels <- unique(as.character(var))
            if (length(qual_levels) == 0) {
              return(NULL)  # Exit if no categories exist
            }
            color_list <- category_colors_map2()
            for (level in qual_levels) {
              if (is.null(color_list[[level]])) {
                color_list[[level]] <- "#D3D3D3"
              }
            }
            color_pal <- colorFactor(unlist(color_list), domain = qual_levels)
          }
          
          # Update Map2 using leafletProxy
          leafletProxy(ns("Map2"), session) %>%
            clearTiles() %>%
            clearMarkers() %>%
            clearGroup("selected_points_map2") %>%
            addProviderTiles(Map2Type()) %>%
            addCircleMarkers(
              data = data_filtered,
              lng = ~Longitude, lat = ~Latitude,
              radius = ~DotSize(),
              color = ~color_pal(var),
              popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var),
              layerId = ~Code
            )
          
        })
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    ### Store User Input into Reactive Values ###
    # This observer captures and stores all changes in user inputs.
    observe({
      Map1Type(input$Map1_type)
      Map2Type(input$Map2_type)
      RegionChoiceMap1(input$region_choice_map1)
      RegionChoiceMap2(input$region_choice_map2)
      ColumnSelectForColorMap1(input$column_select_map1)
      ColumnSelectForColorMap2(input$column_select_map2)
      Col1_map1(input$col1_map1)
      Col1_map2(input$col1_map2)
      Col2_map1(input$col2_map1)
      Col2_map2(input$col2_map2)
      Col3_map1(input$col3_map1)
      Col3_map2(input$col3_map2)
      DotSize(input$dot_size)
      Map1MarkerClick(input$Map1_marker_click)
      Map2MarkerClick(input$Map2_marker_click)
      Map1DrawNewFeature(input$Map1_draw_new_feature)
      Map2DrawNewFeature(input$Map2_draw_new_feature)
      ResetSelectionMap1(input$reset_selection_map1)
      ResetSelectionMap2(input$reset_selection_map2)
      SelectedTableRowsSelectedMap1(input$selected_table_map1_rows_selected)
      SelectedTableRowsSelectedMap2(input$selected_table_map2_rows_selected)
      CheckBoxInputSecondMap(input$SecondMap)
    })
    
    ### Define Predefined Regions ###
    # A list of regions with preset latitude, longitude, and zoom level.
    regions <- list(
      "World" = list(lat = 20, lng = 0, zoom = 2),
      "Europe" = list(lat = 50, lng = 10, zoom = 4),
      "France" = list(lat = 46.603354, lng = 1.888334, zoom = 6),
      "Germany" = list(lat = 51.1657, lng = 10.4515, zoom = 6),
      "USA" = list(lat = 37.0902, lng = -95.7129, zoom = 4)
    )
    
    ### Initialize and Update Maps Based on User Interaction ###
    # When RunMap or a column selection changes, (re)initialize Map1
    observeEvent(RunMap(),
                 {
                   req(RunMap(), merged_climdata(), Map1Type(), RegionChoiceMap1())
                   output$Map1 <- renderLeaflet({
                     leaflet() %>%
                       addProviderTiles(Map1Type()) %>%
                       setView(
                         lng = regions[[RegionChoiceMap1()]]$lng,
                         lat = regions[[RegionChoiceMap1()]]$lat,
                         zoom = regions[[RegionChoiceMap1()]]$zoom
                       ) %>%
                       leaflet.extras::addDrawToolbar(
                         targetGroup = "selected_area_Map1",
                         polylineOptions = FALSE,
                         polygonOptions = TRUE,
                         rectangleOptions = TRUE,
                         circleOptions = FALSE,
                         markerOptions = FALSE,
                         editOptions = list(remove = TRUE)
                       )
                   })
                 }, ignoreInit = TRUE)
    
    
    # For Map2, initialize and update if the second map checkbox is enabled.
    observeEvent({CheckBoxInputSecondMap()
      ColumnSelectForColorMap2()}, {
        req(isTRUE(CheckBoxInputSecondMap()), RunMap(), merged_climdata(), Map2Type(), RegionChoiceMap2(), DotSize())
        
        output$Map2 <- renderLeaflet({
          leaflet() %>%
            addProviderTiles(Map2Type()) %>%
            setView(
              lng = regions[[RegionChoiceMap2()]]$lng,
              lat = regions[[RegionChoiceMap2()]]$lat,
              zoom = regions[[RegionChoiceMap2()]]$zoom
            ) %>%
            leaflet.extras::addDrawToolbar(
              targetGroup = "selected_area_Map2",
              polylineOptions = FALSE,
              polygonOptions = TRUE,
              rectangleOptions = TRUE,
              circleOptions = FALSE,
              markerOptions = FALSE,
              editOptions = list(remove = TRUE)
            )
        })
      }, ignoreInit = TRUE)
    
    ### Function to Update Both Maps Dynamically ###
    update_maps <- function() {
      req(merged_climdata(), Map1Type(), RegionChoiceMap1(), DotSize(), ColumnSelectForColorMap1())
      
      # --- MAP 1 ---
      if (!is.null(ColumnSelectForColorMap1()) && ColumnSelectForColorMap1() != "") {
        data_filtered <- merged_climdata()
        var <- data_filtered[[ColumnSelectForColorMap1()]]
        
        # Palette
        if (is.numeric(var)) {
          req(Col1_map1(), Col2_map1(), Col3_map1())
          color_pal <- colorNumeric(palette = c(Col1_map1(), Col2_map1(), Col3_map1()), domain = var)
        } else {
          qual_levels <- unique(as.character(var))
          if (length(qual_levels) == 0) return(NULL)
          color_list <- sapply(qual_levels, function(level) {
            id <- paste0("color_", level)
            if (!is.null(input[[id]])) isolate(input[[id]]) else "#D3D3D3"
          }, simplify = TRUE, USE.NAMES = TRUE)
          color_pal <- colorFactor(color_list, domain = qual_levels)
        }
        
        # Proxy update only
        leafletProxy(ns("Map1"), session) %>%
          clearTiles() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%
          clearGroup("selected_points_map1") %>%
          addProviderTiles(Map1Type()) %>%
          addCircleMarkers(
            data = data_filtered,
            lng = ~Longitude, lat = ~Latitude,
            radius = DotSize(),
            color = ~color_pal(var),
            popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var),
            layerId = ~Code
          )
      }
      
      # --- MAP 2 ---
      if (isTRUE(CheckBoxInputSecondMap()) &&
          !is.null(ColumnSelectForColorMap2()) &&
          ColumnSelectForColorMap2() != "") {
        req(Map2Type(), RegionChoiceMap2())
        
        data_filtered_map2 <- merged_climdata()
        var_map2 <- data_filtered_map2[[ColumnSelectForColorMap2()]]
        
        if (is.numeric(var_map2)) {
          req(Col1_map2(), Col2_map2(), Col3_map2())
          color_pal_map2 <- colorNumeric(palette = c(Col1_map2(), Col2_map2(), Col3_map2()), domain = var_map2)
        } else {
          qual_levels_map2 <- unique(as.character(var_map2))
          if (length(qual_levels_map2) == 0) return(NULL)
          color_list_map2 <- sapply(qual_levels_map2, function(level) {
            id <- paste0("color_", level)
            if (!is.null(input[[id]])) isolate(input[[id]]) else "#D3D3D3"
          }, simplify = TRUE, USE.NAMES = TRUE)
          color_pal_map2 <- colorFactor(color_list_map2, domain = qual_levels_map2)
        }
        
        leafletProxy(ns("Map2"), session) %>%
          clearTiles() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%
          clearGroup("selected_points_map2") %>%
          addProviderTiles(Map2Type()) %>%
          addCircleMarkers(
            data = data_filtered_map2,
            lng = ~Longitude, lat = ~Latitude,
            radius = DotSize(),
            color = ~color_pal_map2(var_map2),
            popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var_map2),
            layerId = ~Code
          )
      }
    }
    
    
    ### Handle Changes in the Second Map Checkbox ###
    # When the checkbox is toggled, reset related inputs and reactive values.
    observeEvent(CheckBoxInputSecondMap(), {
      req(merged_climdata())
      updateSelectizeInput(
        session,
        inputId = "column_select_map1",
        choices = colnames(merged_climdata()),
        selected = "",
        server = TRUE
      )
      ColumnSelectForColorMap1("") 
      if (isTRUE(CheckBoxInputSecondMap())) {
        updateSelectizeInput(
          session,
          inputId = "column_select_map2",
          choices =  colnames(merged_climdata()),
          selected = "",
          server = TRUE
        )
        ColumnSelectForColorMap2("") 
      } else {
        ColumnSelectForColorMap2("")
        ColumnSelectForColorMap1("") 
      }
      
      
      

      
      # Reset reactive color values using the setter functions
      category_colors_map1(list())
      category_colors_map2(list())
      num_colors_map1(c("#00CCFF", "#989799", "#FF5500"))
      num_colors_map2(c("#00CCFF", "#989799", "#FF5500"))
      color_ui_map1_ready(FALSE)
      color_ui_map2_ready(FALSE)
      RunMap(TRUE)   # Trigger map update
    }, ignoreInit = TRUE)
    
    
    observe({
      req(RunMap())
      if (is.null(ColumnSelectForColorMap1()) || ColumnSelectForColorMap1() == "") {
        leafletProxy(ns("Map1"), session) %>%
          clearTiles() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%
          addProviderTiles(Map1Type()) %>%
          setView(
            lng = regions[[RegionChoiceMap1()]]$lng,
            lat = regions[[RegionChoiceMap1()]]$lat,
            zoom = regions[[RegionChoiceMap1()]]$zoom
          )
      }
    })
    
    
    ### Continuous Map Updates ###
    # Re-run the update_maps function whenever RunMap or the dataset changes.
    observe({
      req(RunMap(), merged_climdata())
      update_maps()
    })
    
    ### Handle Marker Clicks on Map1 ###
    observeEvent(Map1MarkerClick(), {
      click_id <- Map1MarkerClick()$id
      data_filtered <- merged_climdata()
      clicked_row <- data_filtered[data_filtered[[CommunColumn()]] == click_id, ]
      if (nrow(clicked_row) == 0) {
        showNotification(" Point not found!", type = "warning")
        return()
      }
      click_value <- clicked_row[[CommunColumn()]]
      current_selection_map1 <- selected_points_map1()
      current_selection_map2 <- selected_points_map2()
      
      # Toggle selection: if already selected, remove; otherwise, add.
      if (click_value %in% current_selection_map1 ) {
        current_selection_map1 <- setdiff(current_selection_map1, click_value)
        selected_points_map1(current_selection_map1)
        if (isTRUE(CheckBoxInputSecondMap())) {
          current_selection_map2 <- setdiff(current_selection_map2, click_value)
          selected_points_map2(current_selection_map2)
        }
      } else {
        current_selection_map1 <- c(current_selection_map1, click_value)
        selected_points_map1(current_selection_map1)
        if (isTRUE(CheckBoxInputSecondMap())) {
          current_selection_map2 <- c(current_selection_map2, click_value)
          selected_points_map2(current_selection_map2)
        }
      }
      update_selected_points(data_filtered)
    })
    
    ### Handle Marker Clicks on Map2 ###
    observeEvent(Map2MarkerClick(), {
      click_id <- Map2MarkerClick()$id
      data_filtered <- merged_climdata()
      clicked_row <- data_filtered[data_filtered[[CommunColumn()]] == click_id, ]
      if (nrow(clicked_row) == 0) {
        showNotification(" Point not found!", type = "warning")
        return()
      }
      click_value <- clicked_row[[CommunColumn()]]
      current_selection_map1 <- selected_points_map1()
      current_selection_map2 <- selected_points_map2()
      
      if (click_value %in% current_selection_map2 ) {
        selected_points_map2(setdiff(current_selection_map2, click_value))
        selected_points_map1(setdiff(current_selection_map1, click_value))
      } else {
        selected_points_map2(c(current_selection_map2, click_value))
        selected_points_map1(c(current_selection_map1, click_value))
      }
      update_selected_points(data_filtered)
    })
    
    ### Update Selected Points on Maps ###
    # Function to highlight selected points on both maps.
    update_selected_points <- function(data_filtered) {
      if (ColumnSelectForColorMap1() != "") {
        req(merged_climdata(), RegionChoiceMap1())
        region_map1 <- regions[[RegionChoiceMap1()]]
        var_map1 <- data_filtered[[ColumnSelectForColorMap1()]]
        # Choose color palette based on variable type.
        if (is.numeric(var_map1)) {
          color_pal_map1 <- colorNumeric(palette = c(Col1_map1(), Col2_map1(), Col3_map1()), domain = var_map1)
        } else {
          qual_levels_map1 <- unique(as.character(var_map1))
          if (length(qual_levels_map1) == 0) {
            message(" No categorical values found, points will not be displayed.")
            return(NULL)
          }
          color_list_map1 <- sapply(qual_levels_map1, function(level) {
            color_input_id_map1 <- paste0("color_", level)
            if (!is.null(input[[color_input_id_map1]])) {
              isolate(input[[color_input_id_map1]])
            } else {
              "#D3D3D3"
            }
          }, simplify = TRUE, USE.NAMES = TRUE)
          color_pal_map1 <- colorFactor(color_list_map1, domain = qual_levels_map1)
        }
        leafletProxy(ns("Map1"), session) %>%
          clearGroup("selected_points_map1") %>%
          addCircleMarkers(
            data = data_filtered[data_filtered[[CommunColumn()]] %in% selected_points_map1(), ],
            lng = ~Longitude, lat = ~Latitude,
            radius = ~DotSize(),
            color = "red",
            group = "selected_points_map1",
            popup = ~paste0("<strong>Code:</strong> ", Code),
            layerId = ~Code
          )
        
      }
      
      # Update Map2 if it is enabled.
      if (isTRUE(CheckBoxInputSecondMap())){
        if (ColumnSelectForColorMap2() != "") {
          req(RegionChoiceMap2())
          region_map2 <- regions[[RegionChoiceMap2()]]
          var_map2 <- data_filtered[[ColumnSelectForColorMap2()]]
          color_pal_map2 <- if (is.numeric(var_map2)) {
            colorNumeric(palette = c(Col1_map2(), Col2_map2(), Col3_map2()), domain = var_map2)
          } else {
            qual_levels_map2 <- unique(as.character(var_map2))  # Ensure levels exist even if there are missing values
            if (length(qual_levels_map2) == 0) {
              message(" No categorical values found, points will not be displayed.")
              return(NULL)
            }
            color_list_map2 <- sapply(qual_levels_map2, function(level) {
              color_input_id_map2 <- paste0("color_", level)
              if (!is.null(input[[color_input_id_map2]])) {
                isolate(input[[color_input_id_map2]])
              } else {
                "#D3D3D3"
              }
            }, simplify = TRUE, USE.NAMES = TRUE)
            color_pal_map2 <- colorFactor(color_list_map2, domain = qual_levels_map2)
          }
          
          leafletProxy(ns("Map2"), session) %>%
            clearGroup("selected_points_map2") %>%
            addCircleMarkers(
              data = data_filtered[data_filtered[[CommunColumn()]] %in% selected_points_map2(), ],
              lng = ~Longitude, lat = ~Latitude,
              radius = ~DotSize(),
              color = "red",
              group = "selected_points_map2",
              popup = ~paste0("<strong>Code:</strong> ", Code),
              layerId = ~Code
            )
          
        }
      }
    }
    
    ### Handle Drawing Features on the Maps ###
    # When the user draws a polygon on Map1, update the selection accordingly.
    observeEvent(Map1DrawNewFeature(), {
      req(merged_climdata(), Map1DrawNewFeature(), RegionChoiceMap1())
      feature_map1 <- Map1DrawNewFeature()
      coords_map1 <- feature_map1$geometry$coordinates[[1]]
      region_map1 <- regions[[RegionChoiceMap1()]]
      # Convert drawn coordinates to an sf polygon
      drawn_polygon_map1 <- sf::st_polygon(list(do.call(rbind, lapply(coords_map1, function(coord) {
        c(coord[[1]], coord[[2]])
      }))))
      drawn_sf_map1 <- sf::st_sfc(drawn_polygon_map1, crs = 4326)
      # Convert climate data to sf object
      clim_data_sf_map1 <- sf::st_as_sf(merged_climdata(), coords = c("Longitude", "Latitude"), crs = 4326)
      # Identify points within the drawn polygon
      points_inside_map1 <- clim_data_sf_map1[sf::st_within(clim_data_sf_map1, drawn_sf_map1, sparse = FALSE), ]
      # Update selected points for Map1 and synchronize with Map2 if enabled
      new_selected_points <- unique(c(selected_points_map1(), points_inside_map1[[CommunColumn()]]))
      selected_points_map1(new_selected_points)
      if (isTRUE(CheckBoxInputSecondMap())) {
        selected_points_map2(new_selected_points)  # Sync selection with Map2
      }
      data_filtered <- merged_climdata()
      update_selected_points_feature(data_filtered)
    })
    
    # Similar logic for Map2 drawing
    observeEvent(Map2DrawNewFeature(), {
      req(merged_climdata(), Map2DrawNewFeature(), RegionChoiceMap2())
      feature_map2 <- Map2DrawNewFeature()
      coords_map2 <- feature_map2$geometry$coordinates[[1]]
      region_map2 <- regions[[RegionChoiceMap2()]]
      # Convert drawn shape to an sf polygon
      drawn_polygon_map2 <- sf::st_polygon(list(do.call(rbind, lapply(coords_map2, function(coord) {
        c(coord[[1]], coord[[2]])
      }))))
      drawn_sf_map2 <- sf::st_sfc(drawn_polygon_map2, crs = 4326)
      # Convert climate data to sf object
      clim_data_sf_map2 <- sf::st_as_sf(merged_climdata(), coords = c("Longitude", "Latitude"), crs = 4326)
      # Identify points within the drawn polygon
      points_inside_map2 <- clim_data_sf_map2[sf::st_within(clim_data_sf_map2, drawn_sf_map2, sparse = FALSE), ]
      # Update selected points for Map2 and synchronize with Map1
      new_selected_points <- unique(c(selected_points_map2(), points_inside_map2[[CommunColumn()]]))
      selected_points_map2(new_selected_points)
      selected_points_map1(new_selected_points)
      data_filtered <- merged_climdata()
      update_selected_points_feature(data_filtered)
    })
    
    # Function to update selected points after drawing features
    update_selected_points_feature <- function(data_filtered) {
      if (ColumnSelectForColorMap1() != "") {
        var_map1 <- data_filtered[[ColumnSelectForColorMap1()]]
        if (is.numeric(var_map1)) {
          color_pal_map1 <- colorNumeric(palette = c(Col1_map1(), Col2_map1(), Col3_map1()), domain = var_map1)
        } else {
          qual_levels_map1 <- unique(as.character(var_map1))
          if (length(qual_levels_map1) == 0) {
            message(" No categorical values found, points will not be displayed.")
            return(NULL)
          }
          color_list_map1 <- sapply(qual_levels_map1, function(level) {
            color_input_id_map1 <- paste0("color_", level)
            if (!is.null(input[[color_input_id_map1]])) {
              isolate(input[[color_input_id_map1]])
            } else {
              "#D3D3D3"
            }
          }, simplify = TRUE, USE.NAMES = TRUE)
          color_pal_map1 <- colorFactor(color_list_map1, domain = qual_levels_map1)
        }
        leafletProxy(ns("Map1"), session) %>%
          clearGroup("selected_points_map1") %>%
          addCircleMarkers(
            data = data_filtered[data_filtered[[CommunColumn()]] %in% selected_points_map1(), ],
            lng = ~Longitude, lat = ~Latitude,
            radius = ~DotSize(),
            color = "red",
            group = "selected_points_map1",
            popup = ~paste0("<strong>Code:</strong> ", Code),
            layerId = ~Code
          )
        
      }
      
      # Update Map2 if enabled
      if (isTRUE(CheckBoxInputSecondMap())) {
        if (ColumnSelectForColorMap2() != "") {
          req(RegionChoiceMap2())
          region_map2 <- regions[[RegionChoiceMap2()]]
          var_map2 <- data_filtered[[ColumnSelectForColorMap2()]]
          color_pal_map2 <- if (is.numeric(var_map2)) {
            colorNumeric(palette = c(Col1_map2(), Col2_map2(), Col3_map2()), domain = var_map2)
          } else {
            qual_levels_map2 <- unique(as.character(var_map2))  # Ensure levels exist even if there are missing values
            if (length(qual_levels_map2) == 0) {
              message(" No categorical values found, points will not be displayed.")
              return(NULL)
            }
            color_list_map2 <- sapply(qual_levels_map2, function(level) {
              color_input_id_map2 <- paste0("color_", level)
              if (!is.null(input[[color_input_id_map2]])) {
                isolate(input[[color_input_id_map2]])
              } else {
                "#D3D3D3"
              }
            }, simplify = TRUE, USE.NAMES = TRUE)
            color_pal_map2 <- colorFactor(color_list_map2, domain = qual_levels_map2)
          }
          
          
          leafletProxy(ns("Map2"), session) %>%
            clearGroup("selected_points_map2") %>%
            addCircleMarkers(
              data = data_filtered[data_filtered[[CommunColumn()]] %in% selected_points_map2(), ],
              lng = ~Longitude, lat = ~Latitude,
              radius = ~DotSize(),
              color = "red",
              group = "selected_points_map2",
              popup = ~paste0("<strong>Code:</strong> ", Code),
              layerId = ~Code
            )
        }
      }
    }
    
    ### Render DataTable for Selected Points (Map1) ###
    output$selected_table_map1 <- DT::renderDataTable({
      req(RunMap(), merged_climdata())
      if (!is.null(selected_points_map1())) {
        data_selected <- merged_climdata()[merged_climdata()[[CommunColumn()]] %in% selected_points_map1(), ]
      } else {
        data_selected <- merged_climdata()
      }
      # Display a message if no points are selected
      if (nrow(data_selected) == 0) {
        return(data.frame(Message = "No points selected"))
      }
      # Render the DataTable with export options
      DT::datatable(
        data_selected,
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
        escape = FALSE,
        selection = list(mode = "multiple")
      )
    }, server = FALSE)
    
    ### Handle Row Selection from DataTable ###
    observeEvent(SelectedTableRowsSelectedMap1(), {
      req(SelectedTableRowsSelectedMap1())
      selected_rows <- SelectedTableRowsSelectedMap1()
      data_selected <- merged_climdata()
      if (nrow(data_selected) == 0 || length(selected_rows) == 0) {
        return()
      }
      # Extract selected point codes
      selected_codes <- data_selected[selected_rows, CommunColumn()]
      selected_points_table_map1(selected_codes)
      # Sync selection with Map2 if enabled
      if (isTRUE(CheckBoxInputSecondMap())) {
        selected_points_table_map2(selected_codes)
      }
      # Highlight selected points on Map1
      leafletProxy(ns("Map1"), session) %>%
        clearGroup("selected_points_table_map1") %>%
        addCircleMarkers(
          data = data_selected[data_selected[[CommunColumn()]] %in% selected_points_table_map1(), ],
          lng = ~Longitude, lat = ~Latitude,
          radius = ~DotSize(),
          color = "red",
          group = "selected_points_table_map1",
          popup = ~paste0("<strong>Code:</strong> ", Code),
          layerId = ~Code
        )
      
      # Highlight selected points on Map2 if enabled
      if (isTRUE(CheckBoxInputSecondMap())) {
        leafletProxy(ns("Map2"), session) %>%
          clearGroup("selected_points_table_map2") %>%
          addCircleMarkers(
            data = data_selected[data_selected[[CommunColumn()]] %in% selected_points_table_map2(), ],
            lng = ~Longitude, lat = ~Latitude,
            radius = ~DotSize(),
            color = "red",
            group = "selected_points_table_map2",
            popup = ~paste0("<strong>Code:</strong> ", Code),
            layerId = ~Code
          )
      }
    })
    
    
    ### Reset Selection for Map1 ###
    observeEvent(ResetSelectionMap1(), {
      selected_points_map1(NULL)
      selected_points_map2(NULL)
      selected_points_table_map1(NULL)
      selected_points_table_map2(NULL)
      SelectedTableRowsSelectedMap1(NULL)
      
      req(merged_climdata(), ColumnSelectForColorMap1(), RegionChoiceMap1())
      region_map1 <- regions[[RegionChoiceMap1()]]
      data_filtered <- merged_climdata()
      var_map1 <- data_filtered[[ColumnSelectForColorMap1()]]
      if (is.numeric(var_map1)) {
        color_pal_map1 <- colorNumeric(palette = c(Col1_map1(), Col2_map1(), Col3_map1()), domain = var_map1)
      } else {
        qual_levels_map1 <- unique(as.character(var_map1))
        if (length(qual_levels_map1) == 0) {
          message(" No categorical values found, points will not be displayed.")
          return(NULL)
        }
        color_list_map1 <- sapply(qual_levels_map1, function(level) {
          color_input_id_map1 <- paste0("color_", level)
          if (!is.null(input[[color_input_id_map1]])) {
            isolate(input[[color_input_id_map1]])
          } else {
            "#D3D3D3"
          }
        }, simplify = TRUE, USE.NAMES = TRUE)
        color_pal_map1 <- colorFactor(color_list_map1, domain = qual_levels_map1)
      }
      
      leafletProxy(ns("Map1"), session) %>%
        clearGroup("selected_points_map1") %>%
        clearGroup("selected_area_Map1") %>%
        clearShapes() %>%
        clearControls() %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = data_filtered,
          lng = ~Longitude, lat = ~Latitude,
          radius = ~DotSize(),
          color = ~color_pal_map1(var_map1),
          popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var_map1),
          layerId = ~Code
        )
      
      if (isTRUE(CheckBoxInputSecondMap())) {
        req(ColumnSelectForColorMap2(), RegionChoiceMap2())
        region_map2 <- regions[[RegionChoiceMap2()]]
        var_map2 <- data_filtered[[ColumnSelectForColorMap2()]]
        
        color_pal_map2 <- if (is.numeric(var_map2)) {
          colorNumeric(palette = c(Col1_map2(), Col2_map2(), Col3_map2()), domain = var_map2)
        } else {
          qual_levels_map2 <- unique(as.character(var_map2))
          if (length(qual_levels_map2) == 0) {
            message(" No categorical values found, points will not be displayed.")
            return(NULL)
          }
          color_list_map2 <- sapply(qual_levels_map2, function(level) {
            color_input_id_map2 <- paste0("color_", level)
            if (!is.null(input[[color_input_id_map2]])) {
              isolate(input[[color_input_id_map2]])
            } else {
              "#D3D3D3"
            }
          }, simplify = TRUE, USE.NAMES = TRUE)
          color_pal_map2 <- colorFactor(color_list_map2, domain = qual_levels_map2)
        }
        
        leafletProxy(ns("Map2"), session) %>%
          clearGroup("selected_points_map2") %>%
          clearGroup("selected_area_Map2") %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          addCircleMarkers(
            data = data_filtered,
            lng = ~Longitude, lat = ~Latitude,
            radius = ~DotSize(),
            color = ~color_pal_map2(var_map2),
            popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var_map2),
            layerId = ~Code
          )
      }
      
    })
    
    ### Download Handlers for Exporting Maps ###
    # The following download handlers export the maps as SVG, PNG, or HTML.
    
    observeEvent(input$openDownloadModalMap1, {
      req(merged_climdata(), ColumnSelectForColorMap1(), Map1Type(), RegionChoiceMap1(), DotSize())
      data_filtered <- merged_climdata()
      var_map1 <- data_filtered[[ColumnSelectForColorMap1()]]
      color_pal_map1 <- if (is.numeric(var_map1)) {
        req(Col1_map1(), Col2_map1(), Col3_map1())
        colorNumeric(palette = c(Col1_map1(), Col2_map1(), Col3_map1()), domain = var_map1)
      } else {
        qual_levels_map1 <- unique(as.character(var_map1))
        color_list_map1 <- sapply(qual_levels_map1, function(level) {
          input_id <- paste0("color_", level)
          if (!is.null(input[[input_id]])) isolate(input[[input_id]]) else "#D3D3D3"
        }, simplify = TRUE, USE.NAMES = TRUE)
        colorFactor(color_list_map1, domain = qual_levels_map1)
      }
      
      region <- regions[[RegionChoiceMap1()]]
      selected_data <- data_filtered[data_filtered[[CommunColumn()]] %in% selected_points_map1(), ]
      
      Map1_complet <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(Map1Type()) %>%
        setView(lng = region$lng, lat = region$lat, zoom = region$zoom) %>%
        addCircleMarkers(
          data = data_filtered,
          lng = ~Longitude, lat = ~Latitude,
          radius = DotSize(),
          color = ~color_pal_map1(var_map1),
          popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var_map1),
          layerId = ~Code
        )
      
      if (nrow(selected_data) > 0) {
        Map1_complet <- Map1_complet %>%
          addCircleMarkers(
            data = selected_data,
            lng = ~Longitude, lat = ~Latitude,
            radius = ~DotSize(),
            color = "red",
            group = "selected_points_map1",
            popup = ~paste0("<strong>Code:</strong> ", Code),
            layerId = ~paste0("selected_", Code)
          )
      }
      
      Map1_react(Map1_complet)
      showModal(modalDialog(
        title = "Download Map Settings",
        
        selectInput(ns("downloadFormatMap1"), "Select Format", choices = c("PNG", "SVG", "HTML")),
        
        
        downloadButton(ns("downloadCustomMap1"), "Download Map", icon = icon("download")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    
    observeEvent(input$openDownloadModalMap2, {
      
      req(merged_climdata(), ColumnSelectForColorMap2(), Map2Type(), RegionChoiceMap2(), DotSize())
      data_filtered <- merged_climdata()
      var_map2 <- data_filtered[[ColumnSelectForColorMap2()]]
      color_pal_map2 <- if (is.numeric(var_map2)) {
        req(Col1_map2(), Col2_map2(), Col3_map2())
        colorNumeric(palette = c(Col1_map2(), Col2_map2(), Col3_map2()), domain = var_map2)
      } else {
        qual_levels_map2 <- unique(as.character(var_map2))
        color_list_map2 <- sapply(qual_levels_map2, function(level) {
          input_id <- paste0("color_", level)
          if (!is.null(input[[input_id]])) isolate(input[[input_id]]) else "#D3D3D3"
        }, simplify = TRUE, USE.NAMES = TRUE)
        colorFactor(color_list_map2, domain = qual_levels_map2)
      }
      
      region <- regions[[RegionChoiceMap2()]]
      selected_data <- data_filtered[data_filtered[[CommunColumn()]] %in% selected_points_map2(), ]
      
      
      Map2_complet <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(Map2Type()) %>%
        setView(lng = region$lng, lat = region$lat, zoom = region$zoom) %>%
        addCircleMarkers(
          data = data_filtered,
          lng = ~Longitude, lat = ~Latitude,
          radius = DotSize(),
          color = ~color_pal_map2(var_map2),
          popup = ~paste0("<strong>Code:</strong> ", Code, "<br><strong>Valeur:</strong> ", var_map2),
          layerId = ~Code
        )
      
      if (nrow(selected_data) > 0) {
        Map2_complet <- Map2_complet %>%
          addCircleMarkers(
            data = selected_data,
            lng = ~Longitude, lat = ~Latitude,
            radius = ~DotSize(),
            color = "red",
            group = "selected_points_map2",
            popup = ~paste0("<strong>Code:</strong> ", Code),
            layerId = ~paste0("selected_", Code)
          )
      }
      Map2_react(Map2_complet)
      
      showModal(modalDialog(
        title = "Download Map Settings",
        
        selectInput(ns("downloadFormatMap2"), "Select Format", choices = c("PNG", "SVG", "HTML")),
        
        
        downloadButton(ns("downloadCustomMap2"), "Download Map", icon = icon("download")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    output$downloadCustomMap1 <- downloadHandler(
      filename = function() {
        paste0("map-", Sys.Date(), ".", tolower(input$downloadFormatMap1))
      },
      content = function(file) {
        format <- input$downloadFormatMap1
        
        map_obj <- Map1_react()
        req(map_obj)
        
        tmp_html <- tempfile(fileext = ".html")
        saveWidget(map_obj, tmp_html, selfcontained = TRUE)
        
        b <- ChromoteSession$new()
        b$Page$navigate(paste0("file://", tmp_html))
        Sys.sleep(2)
        
        if (format == "PNG") {
          screenshot <- b$Page$captureScreenshot(format = "png", captureBeyondViewport = TRUE)
          writeBin(base64_dec(screenshot$data), file)
        } else if (format == "SVG") {
          tmp_png <- tempfile(fileext = ".png")
          screenshot <- b$Page$captureScreenshot(format = "png")
          writeBin(base64_dec(screenshot$data), tmp_png)
          
          svg_data <- b$Runtime$evaluate("
        (function() {
          var svgElement = document.querySelector('svg');
          if (!svgElement) return 'ERROR: No SVG element found.';
          return new XMLSerializer().serializeToString(svgElement);
        })();
      ")
          
          if (startsWith(svg_data$result$value, "ERROR")) {
            showNotification("Unable to extract SVG", type = "error")
            b$close()
            return(NULL)
          }
          
          svg_final <- paste0(
            '<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n',
            '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"600\">\n',
            '  <image x=\"0\" y=\"0\" width=\"800\" height=\"600\" xlink:href=\"data:image/png;base64,',
            base64_enc(readBin(tmp_png, "raw", file.info(tmp_png)$size)),
            '\"/>\n', svg_data$result$value, '\n</svg>'
          )
          writeLines(svg_final, file)
        } else if (format == "HTML") {
          file.copy(tmp_html, file, overwrite = TRUE)
        }
        
        b$close()
        showNotification(paste("Downloaded", format, "successfully!"), type = "message")
      }
    )
    
    
    output$downloadCustomMap2 <- downloadHandler(
      filename = function() {
        paste0("map-", Sys.Date(), ".", tolower(input$downloadFormatMap2))
      },
      content = function(file) {
        format <- input$downloadFormatMap2
        
        map_obj <- Map2_react()
        req(map_obj)
        
        tmp_html <- tempfile(fileext = ".html")
        saveWidget(map_obj, tmp_html, selfcontained = TRUE)
        
        b <- ChromoteSession$new()
        b$Page$navigate(paste0("file://", tmp_html))
        Sys.sleep(2)
        
        if (format == "PNG") {
          screenshot <- b$Page$captureScreenshot(format = "png", captureBeyondViewport = TRUE)
          writeBin(base64_dec(screenshot$data), file)
        } else if (format == "SVG") {
          tmp_png <- tempfile(fileext = ".png")
          screenshot <- b$Page$captureScreenshot(format = "png")
          writeBin(base64_dec(screenshot$data), tmp_png)
          
          svg_data <- b$Runtime$evaluate("
        (function() {
          var svgElement = document.querySelector('svg');
          if (!svgElement) return 'ERROR: No SVG element found.';
          return new XMLSerializer().serializeToString(svgElement);
        })();
      ")
          
          if (startsWith(svg_data$result$value, "ERROR")) {
            showNotification("Unable to extract SVG", type = "error")
            b$close()
            return(NULL)
          }
          
          svg_final <- paste0(
            '<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n',
            '<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"600\">\n',
            '  <image x=\"0\" y=\"0\" width=\"800\" height=\"600\" xlink:href=\"data:image/png;base64,',
            base64_enc(readBin(tmp_png, "raw", file.info(tmp_png)$size)),
            '\"/>\n', svg_data$result$value, '\n</svg>'
          )
          writeLines(svg_final, file)
        } else if (format == "HTML") {
          file.copy(tmp_html, file, overwrite = TRUE)
        }
        
        b$close()
        showNotification(paste("Downloaded", format, "successfully!"), type = "message")
      }
    )
    
    
  })
}

## To be copied in the UI

## To be copied in the server
# mod_Map_Visualization_server("Map_Visualization_1")

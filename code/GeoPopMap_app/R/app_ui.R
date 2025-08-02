#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
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
#' @import webshot
#' @import chromote
#' @import htmlwidgets
#' @import base64enc
#' @import rsvg
#' @import leaflet.extras
#' @import data.table
#' @import promises
#' @import future
#' @import ape
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(

      #Theme choice
      theme = shinytheme("cosmo"),

      #Permit to use shinyjs package functions
      useShinyjs(),

      # Inclusion du fichier CSS
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
        tags$script(src = "custom.js")
      ),
      # Conteneur pour le logo
      div(class = "logo-img",
          img(src = "www/GeoPopMap_log.png", alt = "Logo", class = "logo-img"), # Use the image from the www file
      ),

      tabsetPanel(
        ## Main sidebar ----
        mod_application_overview_ui("App_overview"),
        tabPanel("Data_input", sidebarLayout(
          sidebarPanel(
            width = 3,
            # Add scroll bar
            style = "height: 90vh; overflow-y: auto;",
            br(), # Space
            ### File inputs ----
            tags$h4(strong("Files")),
            checkboxInput("InputDataPop", "Click here to upload Population data", value = FALSE),
            conditionalPanel(
              condition =  paste0("input['", "InputDataPop", "'] == true"),
              mod_upload_Populations_Data_ui("upload_Populations_Data")
            ),
            checkboxInput("InputDataClim", "Click here to upload Climatic data", value = FALSE),
            conditionalPanel(
              condition =  paste0("input['","InputDataClim", "'] == true"),
              mod_upload_Climatic_Data_ui("upload_Climatic_Data")
            ),
            checkboxInput("InputDataPheno", "Click here to upload Phenotypic data", value = FALSE),
            conditionalPanel(
              condition =  paste0("input['","InputDataPheno", "'] == true"),
              mod_upload_Phenotypic_Data_ui("upload_Phenotypic_Data")
            ),
            checkboxInput("InputDataGeno", "Click here to upload Genotypic data", value = FALSE),
            conditionalPanel(
              condition =  paste0("input['","InputDataGeno", "'] == true"),
              mod_upload_Genotypic_Data_ui("upload_Genotypic_Data")
            ),
            checkboxInput("InputDataStruct", "Click here to upload Structure data", value = FALSE),
            conditionalPanel(
              condition =  paste0("input['", "InputDataStruct", "'] == true"),
              mod_upload_Structure_Information_Data_ui("upload_Structure_Information_Data")
            )
          ),
          mainPanel(
            h4("Preview of Data"),
            tabsetPanel(
              tabPanel("population Data", DT::DTOutput("populationDataTable")),
              tabPanel("Clim Data", DT::DTOutput("ClimdataeDataTable")),
              tabPanel("Phenotypic Data", DT::DTOutput("PhenotypicdataeDataTable")),
              tabPanel("Genotypic Data", DT::DTOutput("GenotypicdataDataTable")),
              tabPanel("Structure Data", DT::DTOutput("StructureDataTable"))

            ),
            br(),
            # The verification box is initially hidden (display: none)
            div(id = "verification_box", style = "display: none;",

                # The box will only be displayed if at least two datasets are uploaded
                conditionalPanel(
                  condition = "input.InputDataPop + input.InputDataClim + input.InputDataStruct + input.InputDataPheno  + input.InputDataGeno >= 2",

                  fluidRow(
                    column(12, align = "center",

                           # Panel to display verification questions
                           wellPanel(
                             h4("Verify Your Input Data"),  # Title of the verification box

                             # Checkbox: User confirms they have checked the input data
                             checkboxInput("check_inputs", "Have you checked the input data?", FALSE),

                             # Checkbox: User confirms they have chosen a common column between datasets
                             checkboxInput("check_column_choice", "Did you choose the common column between tables?", FALSE),

                             # Checkbox: User confirms that the common column has the same naming in all datasets
                             checkboxInput("check_column_naming", "Does this column have the same naming across datasets?", FALSE),

                             # Run Merge button (initially hidden)
                             actionButton("run_merge", "GO to Merge", icon = icon("play"), class = "btn-primary", style = "display: none;")
                           )
                    )
                  )
                )
            )







          )
        )),

        mod_Merge_Data_ui("Merge_Data"),
        mod_Map_Visualization_ui("Map_Visualization"),
        mod_Anova_structure_ui("Anova_structure"),
        mod_Stat_structure_ui("Stat_structure"),
        mod_PCA_ui("PCA"),
        mod_Correlation_ui("Correlation")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "GeoPopMap_app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

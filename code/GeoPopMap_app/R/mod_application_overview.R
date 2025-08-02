#' Application Overview Server Function
#'
#' @description A shiny Module to initialise the application overview module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column h2 p tabsetPanel tabPanel
#' @import shiny
#' @import DT
#' @import visNetwork
#'

# Example Data Tables
# Population (GPS) Data Example
pop_data <- data.frame(
  code = c("Geno_001", "Geno_002", "Geno_003"),
  longitude = c(45.6789, 38.1234, 40.5678),
  latitude = c(-122.4567, -118.9876, -121.3456)
)

# Climatic Data Example
climate_data <- data.frame(
  code = c("Geno_001", "Geno_002", "Geno_003"),
  tmin1 = c(-5.4, -7.1, -4.9),
  tmax1 = c(25.3, 23.9, 26.1),
  prec1 = c(80, 75, 85),
  bio1 = c(12.5, 11.8, 13.2),
  Bio_Quart_Hottest = c("Summer", "Summer", "Spring")
)

# Phenotypic Data Example
pheno_data <- data.frame(
  code = c("Geno_001", "Geno_002", "Geno_003"),
  yield = c(3.2, 2.8, 3.5),
  flowering_date = c(120, 115, 123)
)

# Genotypic Data Example (test.csv-like format)
geno_data <- data.frame(
  code = c("Chromosomes", "Position", "Geno_001", "Geno_002", "Geno_003"),
  SNP1 = c("chr1", 1001, 0.1, 0.2, 0.3),
  SNP2 = c("chr1", 1050, 0.5, 0.4, 0.6),
  SNP3 = c("chr1", 1100, 0.9, 0.8, 0.7),
  stringsAsFactors = FALSE
)

# Structure Data Example
structure_data <- data.frame(
  code = c("Geno_001", "Geno_002", "Geno_003"),
  K1 = c(0.2, 0.1, 0.3),
  K2 = c(0.3, 0.2, 0.1),
  K3 = c(0.1, 0.25, 0.2),
  K4 = c(0.1, 0.15, 0.15),
  K5 = c(0.1, 0.1, 0.05)
)

mod_application_overview_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Application Overview",
           fluidRow(
             column(12,
                    div(class = "background-panel",
                        h2("Welcome to the GeoPopMap Application"),
                        p(style = "text-align: justify;",
                          "GeoPopMap is a modular Shiny application for the integration, statistical analysis, and interactive visualization of climatic, genotypic, phenotypic, and structure data for geolocated varieties. It enables users to cross-reference multiple data sources to explore relationships between environment, genotype, and phenotype, and to perform spatial and statistical analyses."),
                        br(),
                        h3("General Workflow"),
                        visNetworkOutput(ns("workflow_diagram"), height = "400px"),
                        br(),
                        h3("Main Modules of the Application"),
                        tags$ul(
                          tags$li(tags$b("Data Management and Merging:"), " Import, validation, and merging of multiple datasets (population, climate, phenotype, genotype, structure)."),
                          tags$li(tags$b("Map Visualization:"), " Interactive maps to explore the spatial distribution of genotypes and associated variables."),
                          tags$li(tags$b("Descriptive Statistics:"), " Statistical summaries, distributions, histograms, etc."),
                          tags$li(tags$b("Analysis of Variance (ANOVA):"), " Group comparisons based on genetic structure or other factors."),
                          tags$li(tags$b("Principal Component Analysis (PCA/PCoA):"), " Dimensionality reduction and visualization of relationships between individuals and variables."),
                          tags$li(tags$b("Correlation Analysis:"), " Correlation matrices, visualization of relationships between variables."),
                          tags$li(tags$b("Structure Analysis:"), " Exploration of the genetic structuring of populations.")
                        ),
                        br(),
                        h3("Expected Data Types and Recommended Formats"),
                        tags$ul(
                          tags$li(tags$b("Population (GPS):"), " Table with columns: ", tags$code("code"), ", ", tags$code("longitude"), ", ", tags$code("latitude"), ". Each row corresponds to a geolocated genotype. ", tags$em("Required for all analyses.")),
                          tags$li(tags$b("Climatic:"), " Table with column ", tags$code("code"), " (genotype identifier, must match population) and climatic variables (e.g., tmin1-tmin12, tmax1-tmax12, prec1-prec12, bio1-bio19, seasonal qualitative variables). ", tags$em("The user can add any number of additional climatic variables (quantitative or qualitative) as needed.")),
                          tags$li(tags$b("Phenotypic:"), " Table with column ", tags$code("code"), " (genotype identifier) and one or more phenotypic variables (e.g., yield, flowering date, etc.). Format similar to climate."),
                          tags$li(tags$b("Genotypic (allele frequencies):"), " Table in wide format: first column is ", tags$code("code"), " (with values 'Chromosomes', 'Position', then one row per genotype), each other column is a SNP/marker (e.g., SNP1, SNP2, ...), values = chromosome, position, or allele frequency. ", tags$em("The 'Chromosomes' and 'Position' rows are optional."), " This format matches the test.csv example."),
                          tags$li(tags$b("Structure:"), " Table with column ", tags$code("code"), " (genotype identifier) and one column per structure group (K1, K2, ..., Kn). Values are the percentage of membership to each group (sum <= 1).")
                        ),
                        br(),
                        tags$div(style = "color: #b22222; font-weight: bold;", "Important: the 'code' column must be present and identical in all datasets to allow merging and cross-analysis."),
                        br(),
                        h3("Example Data Formats"),
                        tabsetPanel(
                          tabPanel("Population (GPS)",
                                   p("Example of a population table (geolocated genotypes):"),
                                   DTOutput(ns("gps_table"), width = "auto")),
                          tabPanel("Climatic",
                                   p("Example of a climatic table (quantitative and qualitative variables):"),
                                   DTOutput(ns("climate_table"), width = "auto")),
                          tabPanel("Phenotypic",
                                   p("Example of a phenotypic table (measured traits):"),
                                   DTOutput(ns("pheno_table"), width = "auto")),
                          tabPanel("Genotypic",
                                   p("Example of a genotypic table (allele frequencies):"),
                                   DTOutput(ns("geno_table"), width = "auto")),
                          tabPanel("Structure",
                                   p("Example of a structure table (K groups):"),
                                   DTOutput(ns("structure_table"), width = "auto"))
                        )
                    )
             )
           )
  )
}

#' application_overview Server Functions
#'
#' @noRd
mod_application_overview_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Define the workflow diagram data
    nodes_overview <- data.frame(
      id = 1:6,
      label = c(
        "1. Data Input",
        "2. Data Validation",
        "3. Data Merging",
        "4. Analysis Parameters",
        "5. Visualization (Map, PCA, Stat, Correlation, ANOVA)",
        "6. Export & Download"
      ),
      title = c(
        "<div style='font-size:16px; color:black; padding:10px; border-radius:5px; max-width:250px; max-height:150px; overflow-y:auto;'>Upload your data (Population, Climatic, Phenotypic, Genotypic, Structure).</div>",
        "<div style='font-size:16px; color:black; padding:10px; border-radius:5px; max-width:250px; max-height:150px; overflow-y:auto;'>Check format, columns, missing values, and consistency.</div>",
        "<div style='font-size:16px; color:black; padding:10px; border-radius:5px; max-width:250px; max-height:150px; overflow-y:auto;'>Merge datasets using the common 'code' column.</div>",
        "<div style='font-size:16px; color:black; padding:10px; border-radius:5px; max-width:250px; max-height:150px; overflow-y:auto;'>Choose analysis parameters (variables, groups, thresholds).</div>",
        "<div style='font-size:16px; color:black; padding:10px; border-radius:5px; max-width:250px; max-height:150px; overflow-y:auto;'>Explore results: interactive maps, PCA, statistics, correlations, ANOVA, structure.</div>",
        "<div style='font-size:16px; color:black; padding:10px; border-radius:5px; max-width:250px; max-height:150px; overflow-y:auto;'>Export tables, plots, and results for further use.</div>"
      ),
      shape = "box",
      color = list(background = "lightblue", border = "darkblue"),
      font.size = 18,
      widthConstraint = 220,
      heightConstraint = 70
    )

    edges_overview <- data.frame(
      from = c(1, 2, 3, 4, 5),
      to = c(2, 3, 4, 5, 6)
    )

    output$workflow_diagram <- renderVisNetwork({
      visNetwork(nodes_overview, edges_overview) %>%
        visEdges(arrows = "to") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visInteraction(hover = FALSE,  dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
        visLayout(hierarchical = TRUE)  %>%
        visHierarchicalLayout(
          direction = "LR",
          sortMethod = "directed",
          levelSeparation = 300,
          nodeSpacing = 150
        ) %>%
        htmlwidgets::onRender(
          "function(el, x) {document.getElementById(el.id).style.background = 'transparent';}"
        )
    })

    output$gps_table <- renderDT({ datatable(pop_data, options = list(dom = 't')) })
    output$climate_table <- renderDT({ datatable(climate_data, options = list(dom = 't')) })
    output$pheno_table <- renderDT({ datatable(pheno_data, options = list(dom = 't')) })
    output$geno_table <- renderDT({ datatable(geno_data, options = list(dom = 't')) })
    output$structure_table <- renderDT({ datatable(structure_data, options = list(dom = 't')) })

  })
}

## To be copied in the UI
# mod_application_overview_ui("application_overview_1")

## To be copied in the server
# mod_application_overview_server("application_overview_1")

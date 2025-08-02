#' Anova_structure UI Function
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
#' @import shinyjs
#' @import shinyWidgets
#' @import dplyr
#' @import colourpicker
#' @import rcompanion
#' @import multcompView
#' @import multcomp
#' @import car
#' @import svglite
#' @import rsvg
mod_Anova_structure_ui <- function(id) {
  ns <- NS(id)
  tabPanel("AOV", sidebarLayout(
    sidebarPanel(
      width = 3,

      # Add scroll bar
      style = "height: 90vh; overflow-y: auto;",

      br(),

      ### Structure parameters ----
      tags$h4(strong("Structure")),

      #ANOVA plot options
      radioGroupButtons(ns("aovplot"), label = "ANOVA plot", choices = c("Box", "Violin"), status = "primary")),
    mainPanel(

      uiOutput(ns("aov_panel"))

    )
  ))
}

#' Anova_structure Server Functions
#'
#' @noRd
mod_Anova_structure_server <- function(id, cleaned_popdata, merged_climdata,
                                       strdata, ColumnSelectForColor,  AOVPlot){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Automatically save thresholds when inputs change
    observe({
      AOVPlot(input$aovplot)


    })

    output$aov_panel <- renderUI(

      ### Condition var quanti + structure ----
      if(!is.null(cleaned_popdata()) && !is.null(merged_climdata()) && !is.null(strdata()) && !is.null(ColumnSelectForColor()) && is.numeric(merged_climdata()[[ColumnSelectForColor()]])){

        ### Structure ----
        tabPanel("Structure AOV",
                 tags$h3(strong("AOV structure")),
                 # Explanation of ANOVA
                 tags$div(
                   style = "border: 1px solid #ddd; border-radius: 6px; padding: 15px; margin-top: 20px; background-color: #fafafa;",
                   tags$div(
                     style = "display: flex; justify-content: space-between; align-items: center;",
                     tags$h4("About the ANOVA Test", style = "margin: 0;"),
                     actionButton(
                       ns("toggleGraphOptions"),
                       label = NULL,
                       icon = icon("minus"),
                       class = "btn btn-default btn-sm toggle-box-btn",
                       `data-target` = ns("anovaExplanationBox")
                     )
                   ),
                   tags$div(
                     id = ns("anovaExplanationBox"),
                     style = "display:none;",
                     p("The ANOVA (Analysis of Variance) test helps determine whether the mean of a quantitative variable is significantly different between multiple groups."),
                     p("In this application, the groups are defined by the \"Assignation\" variable, typically coming from a structure analysis (e.g., clustering or genetic groups)."),
                     p("If the p-value from the ANOVA test is below a significance threshold (e.g., 0.05), it indicates that at least one group mean is statistically different from the others."),
                     p("A post-hoc Tukey HSD test is then applied to detect which specific group means differ. Groups sharing the same letter in the plot or table are statistically similar."),
                     tags$h4("Check the validity of the ANOVA assumptions"),
                     p("To trust the ANOVA results, we need to check two main assumptions:"),
                     tags$ul(
                       tags$li(strong("1. Homogeneity of variance"), ": The 'Residuals vs Fitted' plot should show a random scatter of points around the zero line. Patterns like funnels or curves suggest problems."),
                       tags$li(strong("2. Normality of residuals"), ": The 'Normal Q-Q Plot' should have points close to the diagonal. If they deviate strongly, the residuals may not be normally distributed.")
                     ),
                     p("If these assumptions are not respected, results may not be reliable, and alternative methods such as Kruskal-Wallis may be considered."),
                     tags$h4("How to interpret the results"),
                     p("Each group is assigned a letter. Groups that share the same letter are not significantly different from each other. Groups with different letters are statistically distinct. For example, if Group A has letter 'a' and Group B has letter 'b', they are significantly different. If both have 'a', they are not."),
                     tags$h4("Pairwise Comparison Table"),
                     p("This table displays the result of Tukey's test comparing each pair of groups. It includes the estimated difference between means, standard error, t value, and adjusted p-value. Comparisons with p < 0.05 are considered statistically significant.")
                   )
                 ),


                 br(),

                 tags$h3(strong("Model Diagnostics")),
                 plotOutput(ns("residualPlot"), height = "300px"),

                 br(),

                 column(12, downloadButton(ns("downloadAOVSVG"), "Download plot as SVG"),
                        downloadButton(ns("downloadAOVPNG"), "Download plot as PNG")),
                 br(),
                 br(),
                 plotOutput(ns("aov_structure"), width = "800px", height = "700px"),
                 tags$h3(strong("Structure ANOVA summary")),
                 verbatimTextOutput(ns("aov_tab")),
                 tags$h3(strong("Structure Posthoc tests (TukeyHSD) summary")),
                 DT::dataTableOutput(ns("posthoc")),
                 tags$h3(strong("Pairwise group comparisons")),
                 DT::dataTableOutput(ns("pairwise_table"))
                 )
      }

      ### Condition var quanti only ----
      else if(!is.null(cleaned_popdata()) && !is.null(merged_climdata()) && !is.null(ColumnSelectForColor()) && is.numeric(merged_climdata()[[ColumnSelectForColor()]])){
        tabPanel("Structure AOV",
                 tags$h3(strong("AOV structure")),
                 p(style = "text-align: justify;",
                   "Quantitative variable only, We can't do ANOVA") ,
                 br())
      }
      ### Condition var quali ----
      else {
        tabPanel("Structure AOV",
                 tags$h3(strong("AOV structure")),
                 p(style = "text-align: justify;",
                   "Quanlitative variable only, We can't do ANOVA") ,
                 br())
      }
    )


    ## Structure ----

    ### ANOVA Tukey ----

    output$residualPlot <- renderPlot({
      if (!is.null(strdata())){
        pop_clim <- merged_climdata()
        model <- lm(pop_clim[[ColumnSelectForColor()]] ~ Assignation, data = pop_clim)
        par(mfrow = c(1,2))
        plot(model$fitted.values, model$residuals, main = "Residuals vs Fitted")
        qqnorm(model$residuals); qqline(model$residuals)
      }

    })


    #### ANOVA tab ----
    aov_tab <- reactive({
      if (!is.null(strdata())){
        pop_clim <- merged_climdata()
        model <- lm(pop_clim[[ColumnSelectForColor()]]~Assignation, data = pop_clim)
        aov <- Anova(model, type = "II")
        return(print(aov))
      }
    })

    output$aov_tab <- renderPrint({aov_tab()})

    #### Posthoc tab ----
    posthoc <- reactive({
      req(strdata(),  merged_climdata() )  # Ensure structure data and merged dataset exist
      pop_clim <- merged_climdata()  # Get merged data

      # Check if the selected column exists and is numeric
      if (!ColumnSelectForColor() %in% colnames(pop_clim) || !is.numeric(pop_clim[[ColumnSelectForColor()]])) {
        showNotification("Selected variable is not numeric or missing!", type = "error")
        return(NULL)
      }

      # Ensure Assignation column exists
      if (!"Assignation" %in% colnames(pop_clim)) {
        showNotification("Column 'Assignation' not found! Check if structure data was merged properly.", type = "error")
        return(NULL)
      }


      pop_clim$Assignation <- as.factor(pop_clim$Assignation) # Ensure Assignation is a factor
      model <- lm(pop_clim[[ColumnSelectForColor()]]~Assignation, data = pop_clim) # Linear model


      # Run post-hoc test with error handling
      tryCatch({
        mc <- glht(model, mcp(Assignation = "Tukey")) #not actually a Tukey procedure
        mcs <- summary(mc, test=adjusted("single-step"))
        cld <- cld(mcs, level=0.05, decreasing=F)
        cld_let <- cld$mcletters$Letters # Extract compact letter display

        # Summarize results
        x <- group_by(pop_clim, Assignation) %>%
          summarise(
            m = mean(!!sym(ColumnSelectForColor()), na.rm = TRUE),
            sd = sd(!!sym(ColumnSelectForColor()), na.rm = TRUE),
            max = max(!!sym(ColumnSelectForColor()), na.rm = TRUE)
          ) %>%
          arrange(m)

        x$cld<- sort(cld_let) # Assign letters to groups
        colnames(x) <- c("Group", "Mean", "SD", "Max","Letters")
        return(x)
      }, error = function(e) {
        showNotification("Error in post-hoc test: Check data grouping.", type = "error")
        return(NULL)
      })

    })

    #output$posthoc <- renderTable({posthoc()})


    output$posthoc <- DT::renderDataTable({

      DT::datatable(
        posthoc(),
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

    #### Boxplot or Violin ----
    aov_structure <- reactive({
      req(strdata())  # Ensure structure data is present

      if (!is.null(strdata())){
        pop_clim <- merged_climdata()
        clim <- pop_clim[[ColumnSelectForColor()]] # Extract selected column
        posthoc <- posthoc()  # Get posthoc results

        max_var <- max(clim, na.rm = T)
        min_var <- min(clim, na.rm = T)

        if(AOVPlot() == "Box"){  # Boxplot option
          gp <- ggplot(pop_clim, aes(x = Assignation, y = clim, fill = Assignation)) +
            geom_boxplot(alpha = 0.5, outlier.alpha = 0.7)+
            geom_jitter(inherit.aes = T, size = 1, alpha = 1)+
            geom_text(data = posthoc, aes(label = Letters, x = Group, y = Max, fill = Group),
                      vjust = -0.5, size = 5, fontface = "bold")+
            ylim(c(min(0, min_var), max_var+(1/4)*abs(max_var)))+
            theme_classic()
        }

        else if (AOVPlot() == "Violin"){ # Violin plot option
          gp <- ggplot(pop_clim, aes(x = Assignation, y = clim, fill = Assignation)) +
            geom_violin(alpha = 0.5)+
            geom_jitter(inherit.aes = T, size = 1, alpha = 1)+
            geom_text(data = posthoc, aes(label = Letters, x = Group, y = Max, fill = Group),
                      vjust = -0.5, size = 5, fontface = "bold")+
            ylim(c(min(0, min_var), max_var+(1/4)*abs(max_var)))+
            theme_classic()
        }
      }
      return(gp)
    })

    output$aov_structure <- renderPlot({aov_structure()}, width = 800, height = 600)

    ### Download plot as svg ----
    output$downloadAOVSVG <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".svg", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = aov_structure(), device = "svg", dpi = 600)

      },
      contentType = "image/svg+xml"
    )

    ### Download plot as png ----
    output$downloadAOVPNG <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = aov_structure(), device = "png", dpi = 600)

      },
      contentType = "image/png"
    )


    output$pairwise_table <- DT::renderDataTable({
      req(strdata(), merged_climdata())
      pop_clim <- merged_climdata()
      model <- lm(pop_clim[[ColumnSelectForColor()]] ~ Assignation, data = pop_clim)
      mc <- multcomp::glht(model, linfct = multcomp::mcp(Assignation = "Tukey"))
      tbl <- as.data.frame(summary(mc)$test[c("coefficients", "sigma", "tstat", "pvalues")])
      tbl$Comparison <- rownames(tbl)
      tbl <- tbl[, c("Comparison", "coefficients", "sigma", "tstat", "pvalues")]
      names(tbl) <- c("Comparison", "Estimate", "Std. Error", "t value", "Adjusted p-value")
      tbl$`Adjusted p-value` <- format(tbl$`Adjusted p-value`, scientific = TRUE, digits = 3)

      DT::datatable(
        tbl,
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
# mod_Anova_structure_ui("Anova_structure_1")

## To be copied in the server
# mod_Anova_structure_server("Anova_structure_1")

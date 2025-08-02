# Load or install BiocManager if needed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}


# Define required packages, specifying which ones are from Bioconductor
cran_packages <- c("shiny", "ggplot2", "shinyjs", "shinyWidgets", "maps", "shinythemes", "devtools", "rsvg", "R6",
                   "dplyr", "RColorBrewer", "colourpicker", "htmlwidgets",
                   "DT", "plotly", "knitr", "kableExtra", "shinyFiles", "jsonlite",
                   "shinydashboard", "shinycssloaders", "DiagrammeR", "visNetwork", "readxl", "leaflet", "leaflet.extras",
                   "rcompanion", "multcompView", "multcomp", "car", "svglite", "golem", "chromote",
                   "data.table", "promises", "future", "FactoMineR", "factoextra", "sf", "ape")


bioc_packages <- c()  # Add any required Bioconductor packages here

# Function to check and install CRAN packages
check_and_install_cran <- function(packages) {
  missing_packages <- setdiff(packages, installed.packages()[, "Package"])
  if (length(missing_packages) > 0) {
    message("Installing missing CRAN packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    message("All CRAN packages are already installed.")
  }
}

# Function to check and install Bioconductor packages
check_and_install_bioc <- function(packages) {
  missing_packages <- setdiff(packages, installed.packages()[, "Package"])
  if (length(missing_packages) > 0) {
    message("Installing missing Bioconductor packages: ", paste(missing_packages, collapse = ", "))
    BiocManager::install(missing_packages, dependencies = TRUE, force = TRUE)
  } else {
    message("All Bioconductor packages are already installed.")
  }
}

# Check and install CRAN and Bioconductor packages
check_and_install_cran(cran_packages)
check_and_install_bioc(bioc_packages)

# Load all CRAN packages
for (pkg in cran_packages) {
  suppressWarnings(suppressMessages(library(pkg, character.only = TRUE)))
}

# Load all Bioconductor packages
for (pkg in bioc_packages) {
  suppressWarnings(suppressMessages(library(pkg, character.only = TRUE)))
}

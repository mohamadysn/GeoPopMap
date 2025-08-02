### ============================================
###      Launch Script for GeoPopMap App
### ============================================

# Automatically set the working directory to the script's location
get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  match <- grep("--file=", args, value = TRUE)
  if (length(match) > 0) {
    return(sub("--file=", "", match))
  } else {
    stop("ERROR: Unable to determine the script location. Please manually set the working directory.")
  }
}

# Set the working directory
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Mode RStudio
} else {
  setwd(dirname(get_script_path()))  # Mode R normal
}


# Confirm the working directory
message("Working directory set to: ", getwd())

# Ensure we are inside the "GeoPopMap_app" directory
if (!grepl("GeoPopMap_app", getwd())) {
  stop("ERROR: Please run this script from inside the 'GeoPopMap_app' folder.")
}


# Install and load all required packages from the "Needed_packages.R" script
source("Needed_packages.R")

# Load the necessary package
library("golem")

# Run the application
message("Launching GeoPopMap App...")
golem::run_dev()

# or
run_app()


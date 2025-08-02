#' Run the GeoPopMap app inside Docker
#'
#' This function runs the Docker container that hosts the app.
#' It will open the app in the default browser.
#' Requires Docker to be installed.
#' @param port The port to expose (default: 3838).
#' @param image The name of the Docker image to run.
#' @importFrom utils browseURL
#' @export
run_docker_app <- function(port = 3838, image = "geopopmap_docker_img") {
  if (Sys.which("docker") == "") {
    stop("Docker is not installed or not in the PATH.")
  }
  
  # Run Docker container
  message("Launching Docker container...")
  system2("docker", c("run", "--rm", "-d", "-p", sprintf("%d:3838", port), image))
  
  # Open the browser
  browseURL(sprintf("http://localhost:%d", port))
}

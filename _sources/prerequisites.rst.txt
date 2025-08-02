
.. _prerequisites:

Prerequisites
==============

Before you can start using GeoPopMap, you need to set up your environment. This section will guide you through installing the required software and dependencies, with detailed explanations for each step.

Required Software
-----------------

1. **R and RStudio**

   You will need R to run the application and RStudio for a better user experience.

   - **Download R**: Go to the official website (https://cran.r-project.org/) and download the version suitable for your operating system (Windows, macOS, or Linux). Follow the installation instructions provided on the site.
   - **Download RStudio**: Visit https://www.rstudio.com/products/rstudio/download/ and download RStudio Desktop. Install it after installing R.

   After installation, you should be able to open RStudio and see its interface. If you encounter any issues, refer to the official documentation or check your installation path.

2. **Git**

   Git is required for cloning the repository.

   - **Install Git**: Go to https://git-scm.com/book/en/v2/Getting-Started-Installing-Git and follow the instructions for your operating system.
   - After installation, open a terminal (or Command Prompt on Windows) and type `git --version` to verify that Git is correctly installed. You should see the installed version number.

3. **Docker**
   
   Docker is required to run the application in a containerized environment, ensuring consistency across different operating systems.

   - **Linux (Debian/Ubuntu)**:
     - Open a terminal and run the following commands:

       .. code-block:: bash

          sudo install -m 0755 -d /etc/apt/keyrings
          curl -fsSL https://download.docker.com/linux/ubuntu/gpg | \
            sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
          sudo chmod a+r /etc/apt/keyrings/docker.gpg

          echo \
            "deb [arch=$(dpkg --print-architecture) \
            signed-by=/etc/apt/keyrings/docker.gpg] \
            https://download.docker.com/linux/ubuntu \
            $(lsb_release -cs) stable" | \
            sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

          sudo apt-get install -y docker-ce docker-ce-cli containerd.io \
                                docker-buildx-plugin docker-compose-plugin

          sudo systemctl start docker
          sudo systemctl enable docker

     - After installation, you can check Docker is running with `docker --version`.

   - **Windows/macOS**:
     - Download Docker Desktop from https://www.docker.com/products/docker-desktop and follow the installation instructions for your OS.
     - After installation, launch Docker Desktop and ensure it is running. You can check Docker is available by opening a terminal (or PowerShell) and running `docker --version`.

System Dependencies
-------------------

For **Linux** users, you may need to install additional system libraries. Run the following command in your terminal:

   sudo apt-get update && sudo apt-get install -y \
     build-essential cmake make g++ \
     libcurl4-openssl-dev libssl-dev libxml2-dev \
     libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
     librsvg2-dev libharfbuzz-dev libfribidi-dev libgit2-dev \
     libglpk-dev libgmp-dev libmpfr-dev libgsl-dev libv8-dev \
     libicu-dev libx11-dev libxt-dev libmagick++-dev \
     libnode-dev libsqlite3-dev \
     libudunits2-dev libgdal-dev gdal-bin libgeos-dev libproj-dev \
     libabsl-dev graphviz pandoc chromium-browser chromium

This will install all the necessary libraries for R packages and spatial data processing. If you are on Windows or macOS, most dependencies are handled by Docker.

Required R Packages
-------------------

The application will automatically handle the installation of required R packages. If you encounter issues, you can install them manually in R or RStudio:

   install.packages(c("devtools", "remotes"))

If you have trouble installing packages, check your internet connection, ensure you have write permissions to your R library, and verify that all system dependencies are installed (especially on Linux).

Next Steps
----------

Now that you have all the prerequisites, you are ready to :doc:`install and run the application <usage>`. If you encounter any issues, consult the official documentation for each tool or seek help from the GeoPopMap community.



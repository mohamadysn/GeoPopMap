.. _usage:

Usage
=====

This section guides you through installing and running the GeoPopMap application. Follow these steps carefully to ensure a smooth setup. Each step is explained in detail so you can follow along without screenshots.

Installation Steps
------------------

1. **Download the Application**

   You can either download the project as a ZIP file or clone it using Git.

   - **As ZIP**: Go to the GitHub repository page (https://github.com/your-username/GeoPopMap), click on the "Code" button, and select "Download ZIP". Once downloaded, extract the contents to your desired location.
   - **With Git**: Open a terminal (or Command Prompt on Windows) and run:

        git clone https://github.com/your-username/GeoPopMap.git
        
        This will create a folder named `GeoPopMap` in your current directory containing all the project files.

2. **Install R Packages**

   Open RStudio and set the working directory to the `GeoPopMap` folder (you can use the menu or the command `setwd()` in R). Then, run the following command in the R console to install the required R package:

        devtools::install("code/GeoPopMap_app")

   This command will install the GeoPopMap application and its dependencies. If you see any error messages, read them carefully; they often indicate missing system libraries or permissions issues. Make sure you have installed all prerequisites as described in the previous section.

3. **Load the Docker Image**

   - Download the Docker image file from the GitHub repository:

     1. Go to the GitHub project page
     2. Navigate to "Actions" tab
     3. Find the latest successful workflow run named "build-docker"
     4. Click on the workflow run to open it
     5. Scroll down to "Artifacts" section and click "geopopmap-docker-image" to download
     6. Extract the downloaded file to get the Docker image file (e.g., `geopopmap_docker_img.tar`)

   - Place the downloaded Docker image file in the root of the `GeoPopMap_app` folder.
   - Open a terminal (or PowerShell/Command Prompt on Windows) and run:

        docker load -i geopopmap_docker_img.tar

   - Wait for the command to finish. You should see a message indicating the image has been loaded. If you get an error, make sure Docker Desktop (on Windows/macOS) or the Docker service (on Linux) is running, and that the file name matches exactly.

   **Note for updates**: If you already have a previous version of the GeoPopMap Docker image installed and want to install a new version, you need to remove the old image first:

   1. **Check existing images**: Run `docker images -a` to see all installed Docker images
   2. **Identify the old GeoPopMap image**: Look for the GeoPopMap image in the list
   3. **Remove the old image**: Use `docker rmi IMAGE_ID` where IMAGE_ID is the ID of the old GeoPopMap image
       4. **Then proceed** with loading the new image using `docker load -i geopopmap_docker_img.tar`

   **Troubleshooting Docker permissions**: If you get a "permission denied" error when running Docker commands, you need to add your user to the docker group:

   1. **Add your user to the docker group**: Run `sudo usermod -aG docker $USER`
   2. **Log out and log back in** (or restart your system) for the changes to take effect
   3. **Verify the fix**: Run `docker --version` to confirm Docker is accessible
       4. **Alternative**: You can also run Docker commands with `sudo` (e.g., `sudo docker load -i geopopmap_docker_img.tar`)

Running the Application
-----------------------

Once everything is installed, you can run the application from RStudio or the R console. In R, type:

    GeoPopMap::run_docker_app()

This command will start the application inside the Docker container. After a few moments, you should see a message in the R console with a local web address (usually starting with `http://127.0.0.1:xxxx`). Open this address in your web browser to access the GeoPopMap interface.

**Important**: After the first successful launch, the Docker container will remain running. For subsequent uses, you can directly access the application at `http://localhost:3838/` without needing to run the command again. The container will continue running until you stop it manually or restart your system.

If you encounter any issues, check the R console and terminal for error messages. Common problems include missing Docker images, Docker not running, or port conflicts.

Test Data
---------

To help you get started with GeoPopMap, we provide a set of example datasets in the `code/Data_test/` directory. These files contain realistic data that you can use to test all the application features.

Available Test Files
~~~~~~~~~~~~~~~~~~~

**Population Data:**

- `populations_test.csv` - Population data with geographic coordinates (100 individuals)
- `populations_test_with_NA.csv` - Same data with missing values for testing NA handling

**Climatic Data:**

- `climatic_test.csv` - Climatic variables including temperature, precipitation, and bioclimatic indices (100 individuals)
- `climatic_test_with_NA.csv` - Same data with missing values

**Phenotypic Data:**

- `phenotypic_test.csv` - Phenotypic measurements (100 individuals)
- `phenotypic_test_with_NA.csv` - Same data with missing values

**Genotypic Data:**

- `genotypic_test.csv` - Genotypic markers with chromosome information (100 individuals)
- `genotypic_test_without_chrs.csv` - Same data without chromosome information

**Structure Data:**

- `structure_test.csv` - Population structure information (100 individuals)

Data Format
~~~~~~~~~~~

All test files are in CSV format and contain 100 individuals (IND_001 to IND_099) with the following structure:

- **Population data**: Contains `code`, `Longitude`, and `Latitude` columns
- **Climatic data**: Contains temperature (tmin1-3, tmax1-3), precipitation (prec1-3), bioclimatic indices (bio1), climate zones, and land cover
- **Phenotypic data**: Contains various phenotypic measurements
- **Genotypic data**: Contains marker data with individual codes and marker names
- **Structure data**: Contains population structure assignments

Using Test Data
~~~~~~~~~~~~~~

1. **Upload the test files** in the "Data_input" tab of the application
2. **Use the files with "_with_NA" suffix** to test the missing value handling features
3. **Try different combinations** of datasets to test the merging functionality
4. **Explore all visualization features** with these realistic datasets

The test data is designed to demonstrate all application features and provide a realistic example of the data types you might work with in your research.

Next Steps
----------

Now that the application is running and you have access to test data, let's proceed to the :doc:`data_management_and_merging` section to learn how to upload and manage your data.

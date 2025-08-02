# GeoPopMap

**GeoPopMap** is a modular Shiny application built using the `{golem}` framework. It allows users to upload, validate, merge, and visualize biological and environmental datasets, including climatic, genotypic, phenotypic, and structure data.

## Project Contributors

- **Developer & Tutorial Author**:
  - Mohamad Yassine (mohamad.a.ysn@gmail.com)
  
- **Collaborators**:

  - Sarah Ben Sadoun (sarah.ben-sadoun@inrae.fr)
  - Yannick Olivier (yannick.olivier@inrae.fr)
  - Stephane Nicolas (stephane.nicolas@inrae.fr)
  - Franck Gauthier (franck.gauthier@inrae.fr)
  - Agustin-Oscar Galaretto (agustin-oscar.galaretto@inrae.fr)
  - Valentin Bourdin (valentin.bourdin@inrae.fr)

---

## Features

- Modular UI and server architecture using Shiny modules
- Upload and preview multiple dataset types
- Merge datasets based on user-defined common columns
- Interactive maps with leaflet
- PCA and correlation plots
- ANOVA and statistical analysis
- Customizable color inputs and tooltips
- Designed to be easily extensible

---

## Installation and Requirements

### Prerequisites

You will need **R**, **RStudio**, **Git**, and **Docker**. For detailed installation instructions, please refer to our full documentation.

### Installation

1.  **Clone the repository**:
    `git clone https://github.com/your-username/GeoPopMap.git`
2.  **Install the R package**:
    `devtools::install("code/GeoPopMap_app")`
3.  **Load the Docker image**:
    `docker load -i /path/to/image.tar`

For detailed, step-by-step instructions, please see the [Usage Guide](https://your-username.github.io/GeoPopMap/usage.html).

---

## Run the App

Once installed, launch the application from R with:

```r
GeoPopMap::run_docker()
```

This will launch the application inside a Docker container. Open the URL provided in the R console in your browser.

---

## Documentation

For a complete, step-by-step tutorial, please visit our documentation website:

[**Full Documentation Tutorial**](https://your-username.github.io/GeoPopMap/index.html)

---

## License

GPL-3 © [Mohamad Yassine](mailto:mohamad.a.ysn@gmail.com)

---

## Contact

For questions or contributions, please contact [Mohamad Yassine](mailto:mohamad.a.ysn@gmail.com).
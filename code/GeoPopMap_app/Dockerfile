FROM rocker/shiny:4.2.0

# System dependencies
RUN sed -i 's|http://archive.ubuntu.com/ubuntu|http://mirror.init7.net/ubuntu|g' /etc/apt/sources.list && \
    sed -i 's|http://security.ubuntu.com/ubuntu|http://mirror.init7.net/ubuntu|g' /etc/apt/sources.list && \
    apt-get update && apt-get install -y \
    build-essential \
    cmake \
    curl \
    g++ \
    gdal-bin \
    git \
    graphviz \
    libappindicator3-1 \
    libasound2 \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libcairo2-dev \
    libcups2 \
    libcurl4-openssl-dev \
    libdbus-1-3 \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgdk-pixbuf2.0-0 \
    libgeos-dev \
    libgit2-dev \
    libglpk-dev \
    libgmp-dev \
    libgsl-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libmagick++-dev \
    libmpfr-dev \
    libnode-dev \
    libnspr4 \
    libnss3 \
    libpng-dev \
    libproj-dev \
    librsvg2-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff-dev \
    libtiff5-dev \
    libudunits2-dev \
    libv8-dev \
    libx11-dev \
    libxcomposite1 \
    libxdamage1 \
    libxrandr2 \
    libxss1 \
    libxml2-dev \
    libxt-dev \
    make \
    pandoc \
    pandoc-citeproc \
    wget \
    xdg-utils \
    --no-install-recommends && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*



RUN wget -O /tmp/chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt-get update \
    && apt-get install -y /tmp/chrome.deb \
    && rm /tmp/chrome.deb
    
# Copy your golem app
COPY . /build

WORKDIR /build

# Install devtools (for golem) and build dependencies
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org')"
# Install dependencies (and dev) from DESCRIPTION
RUN R -e "devtools::install_deps(dependencies = TRUE)"

# Install the package (your golem app)
RUN R -e "devtools::install('.', upgrade = 'never')"

# Set app launch location
WORKDIR /srv/shiny-server

# Launch script
COPY Launch_app.R /srv/shiny-server/app.R

# Set Chrome args for Chromote (important for Docker)
ENV CHROMOTE_CHROME_ARGS="--no-sandbox"

# Expose Shiny port
EXPOSE 3838

# Run the app
CMD ["/usr/bin/shiny-server"]

FROM rocker/r-ver:latest

LABEL maintainer="Anthony Raborn"
LABEL description="Reproducible R environment for building and checking the ShortForm package"

# Install system dependencies commonly needed by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    qpdf \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R package tooling
RUN R -e "install.packages(c('remotes', 'rcmdcheck'), repos = 'https://cloud.r-project.org')"

# Set working directory
WORKDIR /pkg

# Copy package source into container
COPY . /pkg

# Install package dependencies from DESCRIPTION
RUN R -e "remotes::install_deps(dependencies = TRUE)"

# Build package tarball
RUN R CMD build .

# Check the built package tarball
RUN R -e "tarball <- list.files(pattern = '[.]tar[.]gz$')[1]; rcmdcheck::rcmdcheck(tarball, args = c('--no-manual', '--as-cran'), error_on = 'warning')"

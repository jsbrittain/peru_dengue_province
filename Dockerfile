FROM rocker/tidyverse:4.4.2

# Install system dependencies (R package deps, python, snakemake)
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglpk40 \
    snakemake \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error \
    sf \
    s2 \
    units \
    data.table \
    sf \
    raster \
    ggdist \
    scoringRules \
    caret \
    logger \
    tidymodels \
    ISOweek \
    pec \
    Metrics \
    drf \
    ggpubr \
    rlang \
    forecast \
    plyr \
    dplyr \
    tidyr \
    lubridate \
    SpecsVerification \
    xtable \
    epiR \
    pROC \
    spdep \
    data.table \
    scoringRules \
    scoringutils \
    plyr \
    dplyr \
    ggplot2 \
    geosphere \
    gridExtra \
    mgcv \
    ggh4x \
    cowplot \
    ggnewscale \
    mgcv \
    MuMIn \
    texreg \
    WaveletComp \
    plyr \
    RColorBrewer \
    tidyverse \
    viridis \
    covidcast \
    dlnm \
    tsModel \
    RColorBrewer \
    quantmod \
    maps \
    caret \
    leafem \
    htmlwidgets \
    PropCIs \
    mapview \
    rstan \
    readxl \
    remotes \
    mapsPERU \
    ISOweek \
    ggpp \
    gghighlight \
    conflicted \
    stringdist \
    hoardr \
    argparse

RUN R -e 'install.packages(pkgs = "https://cran.r-project.org/package=scoringutils&version=1.2.2", repos = NULL)'
RUN R -e 'install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)'
RUN R -e 'library(remotes); remotes::install_github("reichlab/kcde", dependencies = TRUE, quiet = TRUE)'
RUN R -e 'library(remotes); remotes::install_github("reichlab/xgbstack", dependencies = TRUE, quiet = TRUE)'
RUN R -e 'library(remotes); remotes::install_github("reichlab/densitystackr", dependencies = TRUE, quiet = TRUE)'
RUN R -e 'library(remotes); remotes::install_github("jmcastagnetto/ubigeo", dependencies = TRUE, quiet = TRUE)'
RUN R -e 'library(remotes); remotes::install_github("wmgeolab/rgeoboundaries", dependencies = TRUE, quiet = TRUE)'
RUN R -e 'library(remotes); remotes::install_github("zdk123/SpiecEasi", dependencies = TRUE, quiet = TRUE)'

RUN apt-get update && apt-get install -y python3 python3-pip python3-venv
RUN python3 -m venv /venv
RUN /venv/bin/pip install --upgrade pip
RUN /venv/bin/pip install grapevne snakemake
ENV PATH="/venv/bin:$PATH"

# Set working directory
WORKDIR /app

# Copy analysis scripts
# COPY scripts ./scripts

# The main script to run on container start
CMD ["/app/scripts/run.sh"]

FROM rocker/r-ver:4.4.2

# Install system dependencies for 'sf' and its dependencies (s2, etc.)
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
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
    caret

# Set working directory
WORKDIR /app

# Copy analysis scripts
COPY . .

# The main script to run on container start
CMD ["/app/run.sh"]

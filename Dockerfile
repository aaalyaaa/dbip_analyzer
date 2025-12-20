# Use R 4.3.2 as base image
FROM r-base:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    ca-certificates \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto CLI
RUN wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.553/quarto-1.4.553-linux-amd64.deb && \
    dpkg -i quarto-1.4.553-linux-amd64.deb && \
    rm quarto-1.4.553-linux-amd64.deb

# Create and set working directory
WORKDIR /app

# Copy package files
COPY DESCRIPTION NAMESPACE ./
COPY R/ ./R/
COPY inst/ ./inst/
COPY man/ ./man/

# Install R packages
RUN R -e "install.packages(c('remotes', 'rvest', 'dplyr', 'data.table', 'R.utils', 'leaflet', 'ggplot2', 'DT', 'arrow', 'httpuv', 'quarto'), repos='https://cloud.r-project.org')"

# Install the package using remotes
RUN R -e "remotes::install_local('.', dependencies = FALSE, upgrade = 'never')"

# Expose port
EXPOSE 8080

# Run the application
CMD ["R", "-e", "dbipAnalyzer::run_app(port = 8080)"]
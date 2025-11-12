FROM rocker/shiny:latest

# Install system libraries needed for R packages (e.g., flextable, officer, ragg)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# Install core R packages with dependencies explicitly
RUN R -e "install.packages(c('ragg', 'officer'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('flextable'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('zip', 'readxl', 'dplyr', 'ggplot2', 'tidyr', 'rlang', 'ggrepel', 'quantmod', 'reshape2', 'magick'), repos='https://cran.rstudio.com/')"

# Copy app files to the Shiny server directory
COPY . /srv/shiny-server/

# Expose Shiny app port
EXPOSE 8080

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=8080)"]







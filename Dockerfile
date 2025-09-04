FROM rocker/shiny:latest

# Install needed packages

RUN R -e "install.packages(c('readxl', 'dplyr', 'ggplot2', 'tidyr','rlang','ggrepel','flextable','officer','quantmod', 'reshape2'), repos='https://cran.rstudio.com/')"

# Copy app files
COPY . /srv/shiny-server/

# Expose port
EXPOSE 8080

# Run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=8080)"]


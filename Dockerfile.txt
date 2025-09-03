FROM rocker/shiny:latest
COPY . /srv/shiny-server/
EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=8080)"]

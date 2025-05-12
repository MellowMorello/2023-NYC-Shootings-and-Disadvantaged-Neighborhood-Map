FROM rocker/shiny:latest

# Install needed R packages
RUN install2.r --error \
    shiny \
    leaflet \
    dplyr \
    lubridate \
    sf \
    htmltools \
    readr

# Copy app files
COPY . /srv/shiny-server/

# Set correct ownership
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]


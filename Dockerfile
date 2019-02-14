FROM quay.io/workflow4metabolomics/gie-shiny:latest

# Installing packages needed for check traffic on the container and kill if none
RUN apt-get update && \
    apt-get install --no-install-recommends -y r-cran-car

# Install R packages
COPY ./packages.R /tmp/packages.R
RUN Rscript /tmp/packages.R

# Build the app
RUN rm -rf /srv/shiny-server/sample-apps && \
    rm /srv/shiny-server/index.html && \
    mkdir -p /srv/shiny-server/samples/nmr && \
    chmod -R 755 /srv/shiny-server/samples/nmr && \
    chown shiny.shiny /srv/shiny-server/samples/nmr

COPY ./app.R /srv/shiny-server/samples/nmr/app.R
COPY ./static/css/styles.css /srv/shiny-server/samples/nmr/styles.css
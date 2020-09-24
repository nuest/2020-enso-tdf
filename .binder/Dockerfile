# Manually created based on https://github.com/karthik/binder-test/blob/master/.binder/Dockerfile
# and https://github.com/rocker-org/rocker-versioned2/blob/master/partials/Dockerfile.binder.partial
# and https://hub.docker.com/r/rocker/binder
FROM rocker/binder:latest
LABEL maintainer = "Daniel Nüst <daniel.nuest@uni-muenster.de>"

ARG NB_USER
ARG NB_UID

# Image uses RStudio Package Manager, frozen to June 25, 2020, see https://packagemanager.rstudio.com/client/#/repos/1/overview
RUN R -q -e "options('repos')"

# Install renv globally and use the lockfile to restore the packages into the home directory
#USER root
#RUN install2.r --error -r $CRAN --skipinstalled renv
#USER ${NB_USER}

# Manually set workdir
#WORKDIR /home/rstudio

# Copy only lockfile and restore first
#COPY renv.lock renv.lock
#RUN R -e "renv::restore()"

# Explicitly copy what we want to have in the Binder
#COPY code code
#COPY data data
#COPY docs docs
#COPY figures figures
#COPY README.md ./

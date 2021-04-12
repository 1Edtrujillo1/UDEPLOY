# Docker Image AImagination

## TAKE CREATED DOCKER IMAGE
FROM rocker/shiny-verse:latest

RUN apt-get update -qq \
    && apt-get -y --no-install-recommends install \
        libsodium-dev \
    && install2.r --error --deps TRUE \
        sodium

##CREATE FOLDER
RUN mkdir -p /docker_r_packages

## COPY FILE
COPY docker_r_packages/install_packages.R /install_packages.R

## INSTALL PACKAGES
RUN Rscript /install_packages.R

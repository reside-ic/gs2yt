FROM rocker/r-ver:3.6.1

RUN apt-get update && apt-get -y install \
        libcurl4-openssl-dev \
        libssl-dev

RUN sed  -i'' '/mran.microsoft.com/d' /usr/local/lib/R/etc/Rprofile.site && \
        Rscript -e 'install.packages("docopt")'

COPY docker/bin /usr/local/bin

RUN install_packages \
        R6 \
        glue \
        googlesheets4 \
        httr \
        jsonlite \
        vaultr

COPY . /gs2yt
WORKDIR /gs2yt

ENTRYPOINT ["/gs2yt/main.R"]

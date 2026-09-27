# Reproducible computational environment for the compendium.
#
# rocker/r-ver images pin R and point CRAN at a dated Posit Package Manager
# snapshot, so every build installs identical package versions.
#
#   docker build -f docker/Dockerfile -t rdsc .
#   docker run --rm rdsc                      # R CMD check
#   docker run --rm -v "$PWD/analysis:/project/analysis" rdsc make analysis

FROM rocker/r-ver:4.5.2

LABEL org.opencontainers.image.title="RDataScienceCompendium" \
      org.opencontainers.image.source="https://github.com/SatvikPraveen/R-Data-Science-Compendium" \
      org.opencontainers.image.licenses="MIT"

RUN /rocker_scripts/install_pandoc.sh \
    && apt-get update \
    && apt-get install -y --no-install-recommends make qpdf \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /project

# Install dependencies first so this layer is cached across code changes.
COPY DESCRIPTION setup.R ./
RUN Rscript setup.R

COPY . .
RUN R CMD INSTALL --no-multiarch .

CMD ["make", "check"]

FROM rocker/verse:4.4.0

WORKDIR /project

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    libpng-dev \
    libjpeg-dev \
    libcairo2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy renv files
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R

# Install renv and restore packages
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# Copy project files
COPY . .

# Expose ports
EXPOSE 8787 3838

CMD ["R"]

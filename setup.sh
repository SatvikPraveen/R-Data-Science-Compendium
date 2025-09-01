#!/bin/bash

# =============================================================================
# R Data Science Portfolio Project - Complete Setup Script
# =============================================================================

echo "🚀 Creating R Data Science Portfolio Project..."

PROJECT_NAME="r-data-science-compendium"
mkdir -p $PROJECT_NAME
cd $PROJECT_NAME

# Initialize git repository
git init
echo "✅ Git repository initialized"

# =============================================================================
# VS Code Configuration
# =============================================================================

mkdir -p .vscode

# VS Code settings.json
cat > .vscode/settings.json << 'EOF'
{
    "r.rterm.mac": "/usr/local/bin/R",
    "r.rpath.mac": "/usr/local/bin/R",
    "r.rterm.option": ["--no-save", "--no-restore", "--quiet"],
    "r.source.focus": "terminal",
    "r.source.encoding": "UTF-8",
    "r.lsp.enabled": true,
    "r.plot.useHttpgd": true,
    "r.sessionWatcher": true,
    "r.bracketedPaste": true,
    "files.associations": {
        "*.R": "r",
        "*.Rmd": "rmd",
        "*.qmd": "quarto"
    },
    "editor.formatOnSave": true,
    "editor.codeActionsOnSave": {
        "source.organizeImports": true
    },
    "editor.rulers": [80, 120],
    "editor.wordWrap": "wordWrapColumn",
    "editor.wordWrapColumn": 80,
    "files.trimTrailingWhitespace": true,
    "files.insertFinalNewline": true,
    "git.enableSmartCommit": true,
    "git.confirmSync": false,
    "terminal.integrated.defaultProfile.osx": "zsh"
}
EOF

# VS Code tasks.json
cat > .vscode/tasks.json << 'EOF'
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Run R Script",
            "type": "shell",
            "command": "Rscript",
            "args": ["${file}"],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "new"
            }
        },
        {
            "label": "Run Tests",
            "type": "shell",
            "command": "Rscript",
            "args": ["-e", "testthat::test_dir('tests/')"],
            "group": "test"
        },
        {
            "label": "Build Documentation",
            "type": "shell",
            "command": "Rscript",
            "args": ["-e", "pkgdown::build_site()"],
            "group": "build"
        },
        {
            "label": "Check Package",
            "type": "shell",
            "command": "R",
            "args": ["CMD", "check", "."],
            "group": "test"
        }
    ]
}
EOF

# Extensions recommendations
cat > .vscode/extensions.json << 'EOF'
{
    "recommendations": [
        "reditorsupport.r",
        "quarto-dev.quarto",
        "ms-vscode.vscode-json",
        "redhat.vscode-yaml",
        "streetsidesoftware.code-spell-checker",
        "github.copilot"
    ]
}
EOF

echo "✅ VS Code configuration created"

# =============================================================================
# Project Structure
# =============================================================================

# Create main directories
mkdir -p R/{01-basics,02-data-manipulation,03-visualization,04-statistical-analysis,05-machine-learning,06-advanced-topics,utils}
mkdir -p data/{raw,processed,external}
mkdir -p analysis
mkdir -p shiny-apps/{data-dashboard,statistical-calculator,ml-model-demo}
mkdir -p tests/testthat
mkdir -p outputs/{plots,reports,models}
mkdir -p docs/{syntax-guide,advanced-topics,case-studies}
mkdir -p presentations
mkdir -p .github/workflows
mkdir -p docker

# Create placeholder files
touch data/raw/.gitkeep
touch data/processed/.gitkeep
touch data/external/.gitkeep

echo "✅ Project directory structure created"

# =============================================================================
# Configuration Files
# =============================================================================

# .gitignore
cat > .gitignore << 'EOF'
# R specific
.Rhistory
.RData
.Ruserdata
*.Rproj
.Rproj.user/

# renv
renv/library/
renv/python/
renv/staging/

# OS specific
.DS_Store
Thumbs.db
*~

# IDE
.idea/

# Data files
data/raw/*.csv
data/raw/*.xlsx
data/raw/*.json
!data/raw/.gitkeep

# Outputs
outputs/plots/*.png
outputs/plots/*.pdf
outputs/reports/*.html
outputs/models/*.rds

# Temporary files
*.tmp
*.temp
.httr-oauth

# Logs
*.log

# Environment
.env
.Renviron

# Docker
.dockerignore

# Shiny
rsconnect/
EOF

# DESCRIPTION file
cat > DESCRIPTION << 'EOF'
Package: RDataScienceCompendium
Title: Comprehensive R Data Science Portfolio
Version: 1.0.0
Authors@R: 
    person("Portfolio", "Developer", , "developer@example.com", role = c("aut", "cre"))
Description: A comprehensive showcase of R programming skills through practical 
    data science applications, covering everything from basic syntax to advanced 
    statistical modeling and machine learning.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.2.3
Depends: 
    R (>= 4.2.0)
Imports:
    dplyr,
    ggplot2,
    readr,
    tidyr,
    caret,
    shiny,
    plotly,
    DT
Suggests: 
    testthat (>= 3.0.0),
    knitr,
    rmarkdown
Config/testthat/edition: 3
VignetteBuilder: knitr
EOF

# LICENSE
cat > LICENSE << 'EOF'
MIT License

Copyright (c) 2025 R Data Science Portfolio

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOF

echo "✅ Configuration files created"

# =============================================================================
# Main Setup Script
# =============================================================================

cat > setup.R << 'EOF'
#!/usr/bin/env Rscript

# =============================================================================
# R Data Science Portfolio Project Setup
# =============================================================================

cat("🎯 Initializing R Data Science Portfolio Project\n\n")

# Check R version
r_version <- getRversion()
if (r_version < "4.2.0") {
  warning("R version 4.2.0 or higher recommended. Current version: ", r_version)
}

# Essential packages for the project
essential_packages <- c(
  # Core tidyverse
  "dplyr", "ggplot2", "readr", "tidyr", "stringr", "purrr", "tibble", "forcats",
  
  # Data manipulation and import
  "data.table", "readxl", "haven", "jsonlite", "DBI", "RSQLite",
  
  # Visualization
  "plotly", "ggthemes", "RColorBrewer", "viridis", "scales", "patchwork",
  
  # Statistical analysis
  "broom", "modelr", "car", "lme4", "survival", "forecast", "tseries",
  
  # Machine learning
  "caret", "randomForest", "xgboost", "e1071", "cluster", "factoextra",
  
  # Interactive applications
  "shiny", "shinydashboard", "DT", "leaflet", "crosstalk",
  
  # Development tools
  "renv", "devtools", "usethis", "testthat", "roxygen2", "pkgdown",
  "lintr", "styler", "here", "fs", "glue",
  
  # Documentation and reporting
  "rmarkdown", "knitr", "bookdown", "flexdashboard",
  
  # Advanced topics
  "R6", "future", "parallel", "Rcpp", "profvis"
)

# Function to install packages if missing
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("📦 Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, repos = "https://cran.rstudio.com/", dependencies = TRUE)
  } else {
    cat("✅ All essential packages already installed\n")
  }
}

# Install missing packages
install_if_missing(essential_packages)

# Initialize renv for reproducibility
if (!file.exists("renv.lock")) {
  cat("🔧 Initializing renv for dependency management...\n")
  renv::init()
} else {
  cat("✅ renv already initialized\n")
}

cat("✅ Setup completed successfully!\n\n")
cat("🚀 Next steps:\n")
cat("1. Open VS Code: code .\n")
cat("2. Install recommended extensions when prompted\n") 
cat("3. Start exploring R/ modules\n")
cat("4. Run tests: Rscript -e \"testthat::test_dir('tests/')\"\n")
cat("5. Commit your changes: git add . && git commit -m 'Initial setup'\n\n")

cat("📚 Happy coding! Your R portfolio project is ready.\n")
EOF

chmod +x setup.R

echo "✅ Setup script created"

# =============================================================================
# Development Helper Scripts
# =============================================================================

cat > dev-helpers.sh << 'EOF'
#!/bin/bash

case "$1" in
  "test")
    echo "🧪 Running tests..."
    Rscript -e "testthat::test_dir('tests/')"
    ;;
  "lint")
    echo "🔍 Checking code style..."
    Rscript -e "lintr::lint_package()"
    ;;
  "docs")
    echo "📚 Building documentation..."
    Rscript -e "pkgdown::build_site()"
    ;;
  "style")
    echo "✨ Formatting code..."
    Rscript -e "styler::style_pkg()"
    ;;
  "check")
    echo "🔍 Checking package..."
    R CMD check .
    ;;
  "shiny")
    echo "🌟 Starting Shiny app..."
    Rscript -e "shiny::runApp('shiny-apps/data-dashboard')"
    ;;
  *)
    echo "Usage: $0 {test|lint|docs|style|check|shiny}"
    ;;
esac
EOF

chmod +x dev-helpers.sh

echo "✅ Development helper scripts created"

# =============================================================================
# Docker Configuration
# =============================================================================

cat > docker/Dockerfile << 'EOF'
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
EOF

cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  r-dev:
    build:
      context: .
      dockerfile: docker/Dockerfile
    ports:
      - "8787:8787"
      - "3838:3838"
    volumes:
      - .:/project
    environment:
      - DISABLE_AUTH=true
      - ROOT=TRUE
EOF

echo "✅ Docker configuration created"

# =============================================================================
# GitHub Actions CI/CD
# =============================================================================

cat > .github/workflows/ci-cd.yml << 'EOF'
name: R Package CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macOS-latest]
        r-version: ['4.3', '4.4']
        
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes
      
    steps:
    - uses: actions/checkout@v4
    
    - name: Set up R ${{ matrix.r-version }}
      uses: r-lib/actions/setup-r@v2
      with:
        r-version: ${{ matrix.r-version }}
        
    - name: Install system dependencies (Ubuntu)
      if: runner.os == 'Linux'
      run: |
        sudo apt-get update
        sudo apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev libgit2-dev
        
    - name: Install R dependencies
      uses: r-lib/actions/setup-r-dependencies@v2
      with:
        extra-packages: any::rcmdcheck, any::covr, any::lintr
        needs: check
        
    - name: Run tests
      run: |
        library(testthat)
        test_dir("tests/")
      shell: Rscript {0}
      
    - name: Check package
      uses: r-lib/actions/check-r-package@v2

  deploy-docs:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Set up R
      uses: r-lib/actions/setup-r@v2
      
    - name: Install dependencies
      run: |
        install.packages(c("pkgdown", "rmarkdown"))
      shell: Rscript {0}
      
    - name: Build documentation
      run: |
        pkgdown::build_site()
      shell: Rscript {0}
      
    - name: Deploy to GitHub Pages
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{ secrets.GITHUB_TOKEN }}
        publish_dir: ./docs
EOF

echo "✅ GitHub Actions workflow created"

echo ""
echo "🎉 Complete R Data Science Portfolio Project Setup Finished!"
echo ""
echo "Project structure created with:"
echo "✅ Professional VS Code configuration"
echo "✅ Complete directory structure"
echo "✅ Package configuration (DESCRIPTION, LICENSE)"
echo "✅ renv setup for reproducibility"
echo "✅ Docker containerization"
echo "✅ GitHub Actions CI/CD pipeline"
echo "✅ Development helper scripts"
echo ""
echo "Next: Run './setup.R' to install packages and initialize the project"
echo "Then: Open VS Code with 'code .' to start development"
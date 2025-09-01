# R Data Science Portfolio - Build Automation
# ==========================================
# Makefile for automating common development tasks

# Variables
R := R
RSCRIPT := Rscript
PACKAGE_NAME := RDataSciencePortfolio
VERSION := $(shell grep "Version:" DESCRIPTION | sed 's/Version: //')

# Default target
.PHONY: all
all: check

# Help target
.PHONY: help
help:
	@echo "R Data Science Portfolio - Available Make Targets"
	@echo "================================================="
	@echo ""
	@echo "Development:"
	@echo "  install        Install package dependencies"
	@echo "  check          Run R CMD check"
	@echo "  test           Run unit tests"
	@echo "  lint           Run code linting"
	@echo "  coverage       Generate test coverage report"
	@echo "  document       Generate documentation"
	@echo ""
	@echo "Analysis:"
	@echo "  notebooks      Render all analysis notebooks"
	@echo "  demos          Run all demonstration scripts"
	@echo "  shiny-apps     Test all Shiny applications"
	@echo ""
	@echo "Deployment:"
	@echo "  deploy-apps    Deploy Shiny apps to shinyapps.io"
	@echo "  build-site     Build documentation website"
	@echo ""
	@echo "Maintenance:"
	@echo "  clean          Clean build artifacts"
	@echo "  update-deps    Update package dependencies"
	@echo "  backup         Create project backup"
	@echo ""

# Package installation and setup
.PHONY: install
install:
	@echo "Installing package dependencies..."
	$(RSCRIPT) -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"
	$(RSCRIPT) -e "renv::restore()"
	@echo "✅ Dependencies installed successfully"

.PHONY: install-dev
install-dev: install
	@echo "Installing development dependencies..."
	$(RSCRIPT) -e "install.packages(c('devtools', 'testthat', 'covr', 'lintr', 'styler', 'pkgdown'))"
	@echo "✅ Development dependencies installed"

# Package checking and testing
.PHONY: check
check:
	@echo "Running R CMD check..."
	$(R) CMD check . --as-cran --no-manual
	@echo "✅ Package check completed"

.PHONY: test
test:
	@echo "Running unit tests..."
	$(RSCRIPT) -e "devtools::test()"
	@echo "✅ Tests completed"

.PHONY: test-coverage
test-coverage:
	@echo "Generating test coverage report..."
	$(RSCRIPT) -e "covr::report(covr::package_coverage(), file = 'coverage-report.html')"
	@echo "✅ Coverage report generated: coverage-report.html"

# Code quality
.PHONY: lint
lint:
	@echo "Running code linting..."
	$(RSCRIPT) -e "lintr::lint_package()"
	@echo "✅ Linting completed"

.PHONY: style
style:
	@echo "Checking and fixing code style..."
	$(RSCRIPT) -e "styler::style_pkg()"
	@echo "✅ Code styling completed"

# Documentation
.PHONY: document
document:
	@echo "Generating documentation..."
	$(RSCRIPT) -e "devtools::document()"
	@echo "✅ Documentation generated"

.PHONY: readme
readme:
	@echo "Rendering README..."
	$(RSCRIPT) -e "if (file.exists('README.Rmd')) rmarkdown::render('README.Rmd')"
	@echo "✅ README rendered"

.PHONY: build-site
build-site: document
	@echo "Building documentation website..."
	$(RSCRIPT) -e "pkgdown::build_site()"
	@echo "✅ Documentation website built"

# Analysis and demonstrations
.PHONY: notebooks
notebooks:
	@echo "Rendering analysis notebooks..."
	@for notebook in analysis/*.Rmd; do \
		if [ -f "$$notebook" ]; then \
			echo "Rendering $$notebook..."; \
			$(RSCRIPT) -e "rmarkdown::render('$$notebook')"; \
		fi \
	done
	@echo "✅ All notebooks rendered"

.PHONY: demos
demos:
	@echo "Running demonstration scripts..."
	@for script in R/*/; do \
		demo_script="$$script/demo.R"; \
		if [ -f "$$demo_script" ]; then \
			echo "Running $$demo_script..."; \
			$(RSCRIPT) "$$demo_script"; \
		fi \
	done
	@echo "✅ All demonstrations completed"

.PHONY: shiny-apps
shiny-apps:
	@echo "Testing Shiny applications..."
	@for app in shiny-apps/*/; do \
		if [ -f "$$app/app.R" ] || ([ -f "$$app/ui.R" ] && [ -f "$$app/server.R" ]); then \
			echo "Testing $$app..."; \
			$(RSCRIPT) -e "shiny::runApp('$$app', launch.browser = FALSE, port = 3838, host = '127.0.0.1')" & \
			sleep 5; \
			curl -f http://127.0.0.1:3838 > /dev/null 2>&1 && echo "✅ $$app is working" || echo "❌ $$app failed"; \
			pkill -f "shiny::runApp"; \
		fi \
	done
	@echo "✅ Shiny app testing completed"

# Build and packaging
.PHONY: build
build:
	@echo "Building package..."
	$(R) CMD build .
	@echo "✅ Package built: $(PACKAGE_NAME)_$(VERSION).tar.gz"

.PHONY: install-local
install-local: build
	@echo "Installing package locally..."
	$(R) CMD INSTALL $(PACKAGE_NAME)_$(VERSION).tar.gz
	@echo "✅ Package installed locally"

# Deployment
.PHONY: deploy-apps
deploy-apps:
	@echo "Deploying Shiny applications..."
	$(RSCRIPT) -e "source('scripts/deploy-shiny-apps.R')"
	@echo "✅ Shiny apps deployed"

# Data processing
.PHONY: process-data
process-data:
	@echo "Processing raw data..."
	@if [ -f "scripts/process-data.R" ]; then \
		$(RSCRIPT) scripts/process-data.R; \
	else \
		echo "No data processing script found"; \
	fi
	@echo "✅ Data processing completed"

# Maintenance tasks
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	rm -rf *.Rcheck/
	rm -f *.tar.gz
	rm -rf docs/
	rm -f coverage-report.html
	rm -f Rplots.pdf
	rm -rf analysis/*_files/
	rm -f analysis/*.html
	find . -name ".Rhistory" -delete
	find . -name ".RData" -delete
	@echo "✅ Clean completed"

.PHONY: deep-clean
deep-clean: clean
	@echo "Deep cleaning (including renv cache)..."
	rm -rf renv/library/
	rm -rf renv/staging/
	@echo "✅ Deep clean completed"

.PHONY: update-deps
update-deps:
	@echo "Updating package dependencies..."
	$(RSCRIPT) -e "renv::update()"
	@echo "✅ Dependencies updated"

.PHONY: snapshot-deps
snapshot-deps:
	@echo "Snapshotting current dependencies..."
	$(RSCRIPT) -e "renv::snapshot()"
	@echo "✅ Dependencies snapshot created"

# Backup and archiving
.PHONY: backup
backup:
	@echo "Creating project backup..."
	@BACKUP_NAME="r-data-science-portfolio-backup-$$(date +%Y%m%d-%H%M%S)"; \
	tar -czf "$$BACKUP_NAME.tar.gz" \
		--exclude='.git' \
		--exclude='renv/library' \
		--exclude='*.Rcheck' \
		--exclude='*.tar.gz' \
		--exclude='docs/' \
		.; \
	echo "✅ Backup created: $$BACKUP_NAME.tar.gz"

# Development workflow shortcuts
.PHONY: dev-setup
dev-setup: install-dev document readme
	@echo "✅ Development environment setup completed"

.PHONY: pre-commit
pre-commit: lint test check
	@echo "✅ Pre-commit checks completed"

.PHONY: release-prep
release-prep: clean document readme test check build
	@echo "✅ Release preparation completed"

# CI/CD simulation
.PHONY: ci-check
ci-check: install lint test check coverage
	@echo "✅ CI pipeline simulation completed"

# Performance benchmarking
.PHONY: benchmark
benchmark:
	@echo "Running performance benchmarks..."
	@if [ -f "benchmarks/run-benchmarks.R" ]; then \
		$(RSCRIPT) benchmarks/run-benchmarks.R; \
	else \
		echo "No benchmark script found"; \
	fi
	@echo "✅ Benchmarking completed"

# Project validation
.PHONY: validate
validate:
	@echo "Validating project structure..."
	$(RSCRIPT) -e "source('.Rprofile'); validate_project()"
	@echo "✅ Project validation completed"

# Generate project statistics
.PHONY: stats
stats:
	@echo "Generating project statistics..."
	@echo "Lines of R code:"
	@find R/ -name "*.R" -exec wc -l {} + | tail -1
	@echo "Number of functions:"
	@grep -r "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]*<-[[:space:]]*function" R/ | wc -l
	@echo "Number of test files:"
	@find tests/ -name "*.R" 2>/dev/null | wc -l || echo "0"
	@echo "✅ Statistics generated"
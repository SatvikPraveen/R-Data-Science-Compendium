# Build automation for the RDataScienceCompendium research compendium.
#
#   make deps       install all package and analysis dependencies
#   make check      R CMD check (as CRAN)
#   make analysis   regenerate every result, figure and the paper
#
# Run `make help` for the full list.

R       ?= R
RSCRIPT ?= Rscript
PKG     := $(shell sed -n 's/^Package: //p' DESCRIPTION)
VERSION := $(shell sed -n 's/^Version: //p' DESCRIPTION)
TARBALL := $(PKG)_$(VERSION).tar.gz

RESULTS := analysis/results/coverage-performance.csv \
           analysis/results/cv-selection-performance.csv
FIGURES := analysis/figures/coverage.png analysis/figures/cv-bias.png
PAPER   := analysis/paper/paper.html

.PHONY: help deps document test lint check coverage site install \
        analysis analysis-quick paper clean docker-build docker-analysis

help:
	@echo "Package development"
	@echo "  deps             Install package, development and analysis dependencies"
	@echo "  document         Regenerate NAMESPACE and man/ with roxygen2"
	@echo "  test             Run the testthat suite"
	@echo "  lint             Run lintr on the package code"
	@echo "  check            Build and R CMD check --as-cran"
	@echo "  coverage         Report test coverage (requires covr)"
	@echo "  site             Build the pkgdown site into docs/"
	@echo "  install          Install the package"
	@echo ""
	@echo "Research compendium"
	@echo "  analysis         Run both simulation studies, draw figures, render the paper"
	@echo "  analysis-quick   Smoke-test the pipeline with few replicates (RDSC_QUICK=1)"
	@echo "  paper            Re-render the paper from existing results"
	@echo ""
	@echo "Containers"
	@echo "  docker-build     Build the reproducible Docker image"
	@echo "  docker-analysis  Run the full analysis inside the container"
	@echo ""
	@echo "  clean            Remove build artefacts"

deps:
	$(RSCRIPT) setup.R

document:
	$(RSCRIPT) -e 'roxygen2::roxygenise()'

test:
	$(RSCRIPT) -e 'testthat::test_local(stop_on_failure = TRUE)'

lint:
	$(RSCRIPT) -e 'l <- lintr::lint_package(); print(l); if (length(l)) quit(status = 1)'

$(TARBALL): DESCRIPTION NAMESPACE $(wildcard R/*.R) $(wildcard man/*.Rd) \
            $(wildcard vignettes/*.Rmd) $(wildcard tests/testthat/*.R)
	$(R) CMD build .

check: $(TARBALL)
	_R_CHECK_CRAN_INCOMING_REMOTE_=false $(R) CMD check --as-cran --no-manual $(TARBALL)

coverage:
	$(RSCRIPT) -e 'cov <- covr::package_coverage(); print(cov); covr::report(cov, file = "coverage.html", browse = FALSE)'

site:
	$(RSCRIPT) -e 'pkgdown::build_site()'

install:
	$(R) CMD INSTALL --no-multiarch .

analysis: install $(RESULTS) $(FIGURES) $(PAPER)

analysis/results/coverage-performance.csv: analysis/scripts/00-setup.R analysis/scripts/01-coverage-study.R
	$(RSCRIPT) analysis/scripts/01-coverage-study.R

analysis/results/cv-selection-performance.csv: analysis/scripts/00-setup.R analysis/scripts/02-cv-selection-study.R
	$(RSCRIPT) analysis/scripts/02-cv-selection-study.R

$(FIGURES): $(RESULTS) analysis/scripts/03-figures.R
	$(RSCRIPT) analysis/scripts/03-figures.R

$(PAPER): analysis/paper/paper.Rmd analysis/paper/references.bib $(RESULTS) $(FIGURES)
	$(RSCRIPT) -e 'rmarkdown::render("analysis/paper/paper.Rmd")'

paper:
	$(RSCRIPT) -e 'rmarkdown::render("analysis/paper/paper.Rmd")'

analysis-quick: install
	RDSC_QUICK=1 $(RSCRIPT) analysis/scripts/01-coverage-study.R
	RDSC_QUICK=1 $(RSCRIPT) analysis/scripts/02-cv-selection-study.R
	$(RSCRIPT) analysis/scripts/03-figures.R
	@echo "Quick run complete. Restore the committed results with: git checkout analysis/"

docker-build:
	docker build -f docker/Dockerfile -t rdsc:$(VERSION) .

docker-analysis: docker-build
	docker run --rm -v "$(CURDIR)/analysis:/project/analysis" rdsc:$(VERSION) make analysis

clean:
	rm -rf $(PKG)_*.tar.gz $(PKG).Rcheck docs coverage.html lib

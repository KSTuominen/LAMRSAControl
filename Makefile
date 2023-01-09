# Determine package name and version from DESCRIPTION file
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Name of built package
PKG_TAR=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Install package
install:
	cd .. && R CMD INSTALL $(PKG_NAME)

# Install package with vignette
install_full:build
	cd .. && R CMD INSTALL $(PKG_TAR)

# Build documentation with roxygen
roxygen:
	Rscript -e 'roxygen2::roxygenize(clean = TRUE)'

# Generate PDF output from the Rd sources
# 1) Rebuild documentation with roxygen
# 2) Generate pdf, overwrites output file if it exists
pdf: roxygen
	cd .. && R CMD Rd2pdf --force $(PKG_NAME)

# Build and check package
check: clean
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && R CMD check --no-manual --no-vignettes --no-build-vignettes $(PKG_TAR)

# Build package
build: clean
	cd .. && R CMD build $(PKG_NAME)

# Build and check package
check_full: clean
	cd .. && R CMD build $(PKG_NAME) --compact-vignettes
	cd .. && R CMD check $(PKG_TAR) --as-cran

# Build vignette
vignette:
	mkdir -p inst/doc
	cd vignettes && Rscript -e "rmarkdown::render(input = 'LAMRSAControl.Rmd', output_file = '../inst/doc/LAMRSAControl.html')"

.PHONY: install install_full roxygen pdf check build check_full clean

# SolveBio for R

**This version of SolveBio for R is compatible with Vault-based datasets only (released on July 28th, 2017).**


[![Build Status](https://travis-ci.org/solvebio/solvebio-r.png?branch=master)](https://travis-ci.org/solvebio/solvebio-r)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/solvebio)](https://cran.r-project.org/package=solvebio)

This package contains the SolveBio R language bindings. SolveBio makes it easy
to access genomic reference data.

Features of this package include:

* Authentication with SolveBio's API
* REST API query support
* S3 object system for SolveBio API resources
* Portability between most platforms: Linux, Windows, OS X.

Please see the SolveBio [documentation](https://docs.solvebio.com) for more
information about the platform.


## Installation

Installing this package requires an installed [R environment](https://www.r-project.org).

```R
install.packages("solvebio")
library(solvebio)
```

## Usage

```R
# By default it will look for a key in the $SOLVEBIO_API_KEY environment variable.
library(solvebio)

# You may also supply an API key in your code
login(api_key="<Your API key>")
# RStudio users can put the following line in ~/.Rprofile
# Sys.setenv(SOLVEBIO_API_KEY="<Your API key>")

# Retrieve a list of all datasets
datasets <- Dataset.all()

# Retrieve a specific dataset (metadata)
ClinVar <- Dataset.get_by_full_path("solvebio:public:/ClinVar/3.7.4-2017-01-30/Variants-GRCh37")

# Query a dataset with filters as JSON:
filters <- '[["gene_symbol", "BRCA1"]]'
# or, filters as R code:
filters <- list(list('gene_symbol', 'BRCA1'), list('clinical_significance',
'Benign'))

# Execute the queries
# NOTE: paginate=TRUE may issue multiple requests, depending on the dataset and filters
results <- Dataset.query(id = ClinVar$id, filters = filters, limit = 1000, paginate = TRUE)

# Access the results (flattened by default)
results

```


## Shiny

To use SolveBio in your Shiny app, refer to the docs on [Developing Applications with R Shiny and SolveBio](https://docs.solvebio.com/applications/developing/#r-shiny-and-solvebio).

This package provides a Shiny server wrapper called `solvebio::protectedServer()` which requires users to authenticate with SolveBio and authorize the app before proceeding. In addition, you may enable token cookie storage by installing [ShinyJS](https://deanattali.com/shinyjs/) and adding JS code (`solvebio::protectedServerJS()`) to your Shiny UI.

An example app is available in the [solvebio-shiny-example](https://github.com/solvebio/solvebio-shiny-example) GitHub repository.


## Developers

To install the development version of this package from GitHub, you will need the `devtools` package.

```R
install.packages(c("devtools", "httr", "jsonlite"))
library(devtools)
devtools::install_github("solvebio/solvebio-r", ref="master")
library(solvebio)
```

To run the test suite:

```bash
make test
```


## Packaging and Releasing

1. Bump the version using the `bumpversion` command (pip install bumpversion).
2. Update the NEWS.md with changes.
3. Update the DESCRIPTION file with the latest date.
4. Regenerate roxygen2 and build/check the tarball:

    make clean
    make
    make check

5. Submit to CRAN.

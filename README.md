# SolveBio for R

**PLEASE NOTE: The SolveBio R Bindings are currently in alpha. The bindings may change frequently so please check back for updates.**

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
require(solvebio)
# You may also supply an API key in your code
login(api_key="<Your API key>")
# RStudio users can put the following line in ~/.Rprofile
# Sys.setenv(SOLVEBIO_API_KEY="<Your API key>")

# Retrieve a list of all datasets
datasets = Dataset.all()

# Retrieve a specific dataset (metadata)
dataset = Dataset.retrieve('ClinVar/Variants')

# Query a dataset with filters as JSON:
filters = '[["gene_symbol", "BRCA1"]]'
# or, filters as R code:
filters = list(list('gene_symbol', 'BRCA1'), list('clinical_significance',
'Benign'))

# Execute the queries
# NOTE: paginate=TRUE may issue multiple requests, depending on the dataset and filters
results = Dataset.query('ClinVar/3.7.2-2016-08-02/Variants', filters=filters, limit=1000, paginate=TRUE)
# Access the results (flattened by default)
results

```


## Developers

To install the development version of this package from GitHub, you will need the `devtools` package.

```R
install.packages(c("devtools", "httr", "jsonlite"))
library(devtools)
devtools::install_github("solvebio/solvebio-r")
library(solvebio)
```


# SolveBio for R

This package contains the SolveBio R language bindings. SolveBio makes it easy
to access genomic reference data.

Features of this package include:

* Authentication with SolveBio's API
* REST API query support
* Object-oriented S4 class system for all SolveBio resources
* Portability between most platforms: Linux, Windows, OS X.

Please see the SolveBio [documentation](https://docs.solvebio.com) for more
information about the platform.


## Installation

Installing this package requires an installed [R environment](http://www.r-project.org).

````R
install.packages("solvebio")
library(solvebio)
````


## Developers

To install the development version of this package, you will need the `devtools` package.

````R
install.packages("devtools")
library(devtools)
devtools::install_github("solvebio/solvebio-r")
library(solvebio)
````


## Usage

````R
# Create the client
sb = SolveBioClient(api_key="$SOLVEBIO_API_KEY")

# Query a dataset
res = query(sb, dataset='ClinVar/Variants', query='{"filters": {}}')
````

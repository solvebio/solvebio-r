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

```R
install.packages("solvebio")
library(solvebio)
```


## Developers

To install the development version of this package, you will need the `devtools` package.

```R
install.packages("devtools")
library(devtools)
devtools::install_github("solvebio/solvebio-r")
library(solvebio)
```


## Usage

```R
# Create the client
# By default it will look for a key in the $SOLVEBIO_API_KEY environment variable.
require(solvebio)
login(api_key="<Your SolveBio API key">)

# Retrieve a list of all datasets
datasets = Dataset.all()

# Retrieve a specific dataset (metadata)
dataset = Dataset.retrieve('ClinVar/Variants')

# Query a dataset
query = '{
    "filters": [
        ["gene_symbol", "BRCA1"]
    ]
}'
results = Dataset.query('ClinVar/Variants', query=query)
# Load the next page of results
results.next()
```


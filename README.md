# SolveBio for R

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

Installing this package requires an installed [R environment](http://www.r-project.org).

```R
install.packages("solvebio")
library(solvebio)
```


## Developers

To install the development version of this package from GitHub, you will need the `devtools` package.

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
# You may also manually supply an API key in your code
# login(api_key="<Your SolveBio API key">)

# Retrieve a list of all datasets
datasets = Dataset.all()

# Retrieve a specific dataset (metadata)
dataset = Dataset.retrieve('ClinVar/Variants')

# Query a dataset with filters as JSON:
query = '[["gene_symbol", "BRCA1"]]'
# or, filters as R code:
query = matrix(c("gene_symbol", "BRCA2"), nrow=1, ncol=2)
response = Dataset.query('ClinVar/Variants', query=query, offset=0, limit=50)
# Access the results
response$results

# Load the next page of results
# reponse$offset will be NULL if there are no more results.
if (!is.null(response$offset)) {
    response = Dataset.query('ClinVar/Variants', query=query, offset=response$offset)
}
```

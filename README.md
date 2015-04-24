# SolveBio for R (PRE-ALPHA)

This package contains SolveBio R language bindings.


## Installation

You will need the `devtools` package in order to install this package.

````R
install.packages("devtools")
library(devtools)
devtools::install_github("solvebio/solvebio-r")
library(solvebio)
````

## Installation from a private repo

Generate YOUR_GITHUB_PAT from https://github.com/settings/applications#personal-access-tokens

````R
install.packages("devtools")
library(devtools)
devtools::install_github("solvebio/solvebio-r",auth_token="YOUR_GITHUB_PAT")
library(solvebio)
````

## Examples

````R
# Create the client
sb = SolveBioClient(api_key="$SOLVEBIO_API_KEY")

# Query a dataset
res = query(sb, dataset='ClinVar/Variants', query='{"filters": {}}')
````

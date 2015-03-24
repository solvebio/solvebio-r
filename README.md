# SolveBio for R

## Installation

You will need the `devtools` package in order to install this package.

````R
install.packages("devtools")
library(devtools)
install_github("solvebio/solvebio-r")
library(solvebio)
````

## Examples

````R
# Create the client
sb = SolveBioClient(api_key="{$SOLVEBIO_API_KEY}")

# Query a dataset
res = query(sb, dataset='ClinVar/Variants', query='{"filters": {}}')
````

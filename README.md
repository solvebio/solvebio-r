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


## Developers

To install the development version of this package from GitHub, you will need the `devtools` package.

```R
install.packages(c("devtools", "httr", "jsonlite"))
library(devtools)
devtools::install_github("solvebio/solvebio-r")
library(solvebio)
```


## Migrating to Version 2

Version 2 of the R client removes support for the `Depository` and
`DepositoryVersion` methods, and adds support for the `Vault` and `Object` methods.

A vault is similar to a filesystem in that it provides a folder-based
hierarchy in which additional folders, files, and SolveBio Datasets can be
stored. The folders, files, and SolveBio Datasets in a vault are
collectively referred to as "objects" and can be accessed using the
`Vault`, `Object`, and `Dataset` methods.

Vaults have an advanced permission model that provides for three different
levels of access: read, write, and admin. Permissions are settable through
the SolveBio UI. For detailed information on the permission model, please
visit this link:

https://support.solvebio.com/hc/en-us/articles/227732207

As part of the migration onto Version 2, SolveBio has automatically applied
the permissions set on existing Depositories to new Vaults which we have created to
replace them.

It is likely that any scripts you have written which utilize the
R client will need to be modified to be compatible with Version 2.
Below is an exhaustive list of all the things that have changed in the
user-facing methods of the client. If you encounter any issues migrating
your code, please submit a support ticket and we would be happy to assist you.


### Naming Conventions

It is useful to know the different names for the various entities (or combined
entities) that are available via the Client.  The naming conventions are
as follows:

```

solvebio:public:/ClinVar/3.7.0-2015-12-06/Variants-GRCh37
+------+
(1)
         +----+
         (2)
+-------------+
(3)
                +---------------------------------------+
                (4)
                                          +-------------+
                                          (5)
+-------------------------------------------------------+
(6)
```
```
(1) - Account Domain
(2) - Vault Name
(3) - Vault Full Path
(4) - Object Path
(5) - Object Filename
(6) - Object Full Path

```

### Changes in Version 2

1. Dataset creation changes

```
Old: Dataset.get_or_create_by_full_name(<full_name>)
New: Dataset.get_or_create_by_full_path(account_domain:vault_name:/parent/path/dataset_name)
```

For example, if you belong to the "acme" domain, then to create a dataset
named named "EGFR_analysis" in the "/July-2017" folder of the "Research" vault,
make the following call:

```
Dataset.get_or_create_by_full_path('Acme:Research:/July_2017/EGFR_analysis')
```

You can optionally leave off the account domain in front, but note that this
will not work if your object path includes a colon:

```
Dataset.get_or_create_by_full_path('Research:/July_2017/EGFR_analysis')
```


2. Dataset retrieval changes

A dataset's "full_path" is a triplet consisting of account domain, vault
name, and the dataset's path in the vault (see above).  Retrieval of a dataset
by its full path can be performed in a single call:

```
Dataset.get_by_full_path("account_domain:vault_name:object_path")
Dataset.get_by_full_path("solvebio:public:/ICGC/3.0.0-23/Donor")
```

In order to get the full path of an existing dataset, search for datasets
within a vault.

```
# Get all of the Clinvar datasets that are version 3 and above
public <- Vault.get_by_full_path('solvebio:public')
Vault.datasets(public$id, query='/ClinVar/3')
```

4. Removal of `Depository` and `DepositoryVersion` classes.

`Depository` has been replaced by the `Vault` class.

`DepositoryVersion` was functionality is now provided by the `Object` class.
Objects are files, folders, or SolveBio Datasets that exist inside a vault. As part of your account's migration onto Version 2 of SolveBio, we have automatically moved datasets located in Depository "X" and DepositoryVersion "Y" to a Vault named "X" and a folder named "Y".


5. Removal of DatasetCommit approval. The `auto_approve`, `is_approved` and
`approved_by` attributes have been removed. The `/approve` endpoint has also
been removed. All commits will be approved automatically.


### Vault Browsing

List all the vaults you currently have access to.

```
Vault.all()
```


### Your Personal Vault

Each user has a personal vault that is accessible to that user only. Other
users cannot list the contents of this vault, cannot access the objects
contained in it, and cannot modify it in any way. To provide access to
objects stored in your personal vault, you must copy the objects into a
different vault.

Your personal dataset can be retrieved using the following method:

```
Vault.get_personal_vault()
```


### Shortcuts

Browsing the contents of a vault can be easily performed using the following
shortcuts.

First, retrieve a vault:

```
vault = Vault.get_personal_vault()
vault = Vault.get_by_full_path('solvebio:public')
vault = Vault.get_by_full_path('your_account_domain:vault_name')
vault = Vault.get_by_full_path('vault_name')  # Searches inside your account domain
```

Then, you may call the appropriate method:

```
Vault.files(vault$id)
Vault.folders(vault$id)
Vault.datasets(vault$id)
Vault.objects(vault$id)  # Includes files, folders, and datasets

Vault.files(vault$id, filename='hello.txt')   # Can pass filters to all of these methods
```

Search for files, folders, and datasets in a vault using the `search` method:

```
Vault.search(vault$id, query='hello')
Vault.search(vault$id, 'hello', object_type='folder')
Vault.search(vault$id, 'hello', object_type='file')
Vault.search(vault$id, 'hello', object_type='dataset')
```

To get or create a new Vault, use the following method:

```
Vault.get_or_create_by_full_path('acme:test1')
Vault.get_or_create_by_full_path('test1')
```


### File Uploads

```
vault <- Vault.get_personal_vault()
object <- Object.upload_file('./analysis.tsv', vault$id, '/')
```

Re-uploading the same file to the same path auto-increments the filename on
the server. This is required because no two objects can have the same full
path.


You can optionally specify a new filename for the uploaded file:
```
vault <- Vault.get_personal_vault()
object <- Object.upload_file('./analysis.tsv', vault$id, '/', 'analysis_v2.tsv')
```

To delete an object, you need its ID. This action cannot be undone.

```
Object.delete(object$id)
```


### Dataset Imports

The functionality of Dataset Imports remains the same, except that you can now pass an object's ID (after uploading it into a Vault):

```
vault <- Vault.get_personal_vault()
# Upload a file into your personal vault
object <- Object.upload_file('./analysis.tsv', vault$id, '/')

# Create (or get) a dataset
dataset_full_path = paste(vault$name, '/My New Dataset', sep=":")
dataset <- Dataset.get_or_create_by_full_path(dataset_full_path)

# Create the import
DatasetImport.create(dataset_id = dataset$id, commit_mode = 'append', object_id = object$id)
```


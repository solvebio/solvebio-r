# solvebio 2.12.1

* Return NULL if Global Beacon doesn't exist on dataset (instead of raising an error)
* Add Global Beacon alias functions for Dataset resources
* Clarify Global Beacon method docs


# solvebio 2.12.0

* Add support for enabling/disabling Global Beacon on datasets
* Add ability to perform Global Search queries


# solvebio 2.11.0

* Fix issue with order fields from dataset and file queries
* Fix issue with mapping field names to titles for dataset queries
    * This is enabled by default. To disable: `Dataset.query(use_field_titles=FALSE)`


# solvebio 2.10.1

* Fix an issue preventing dataset queries in Shiny apps


# solvebio 2.10.0

* Add ability to query files (`Object.query()`)
* Use the dataset's field order when creating data frames from dataset queries


**NOTE: This version has a bug that prevents dataset queries in Shiny apps, please upgrade to v2.10.1**


# solvebio 2.9.0

* Remove query limit of 500k records when paginating through Dataset.query()
* Use global credentials if found to bypass login modal in the Shiny protectedServer


# solvebio 2.8.0

* Added SavedQuery resource


# solvebio 2.7.2

* Fix bug causing Shiny apps to crash on login


# solvebio 2.7.1

* Allow Shiny apps to use global API host (if set)


# solvebio 2.7.0

* Fixed issue where API errors would not be displayed
* Updated OAuth2 token encryption to use openssl


# solvebio 2.6.1

* Fix issue where exceptions were ignored in Shiny apps


# solvebio 2.6.0

* Adds encrypted cookie storage support for OAuth2 tokens in Shiny apps


# solvebio 2.5.1

* Fix issue where empty dataset queries failed with OAuth2 tokens
* Ability to follow export tasks


# solvebio 2.5.0

* Add Dataset.activity() (#27)
* Add Object.get_or_upload_file() (#79)
* Add DatasetExport.get_download_url() (#58)
* Add DatasetTemplate resource (#88)
* Fix issue with nulls in JSON body (#83)
* Ensure facets and filter requests use correct fromJSON params


# solvebio 2.4.0

* Add support for using SOLVEBIO_ACCESS_TOKEN on load
* Add Dataset.fields() method


# solvebio 2.3.1

* Fix issue where jsonlite would cast tuples as vectors
* Add support for server-side validated vault paths


# solvebio 2.3.0

* Fix issue with deploying to Shiny Server Pro
* Fix issue with oauth redirect URI
* Add client_secret support


# solvebio 2.2.0

* Adds Application resource support (OAuth2 apps)
* [beta] Adds Shiny server wrapper (protected server)
* Fixes issues with env pass-through
* Removes deprecated Upload resource


# solvebio 2.1.0

* Adds Beacon and BeaconSet methods
* Adds a few "update" methods for PATCH requests (editing objects)
* Adds a few new examples for aggregations
* Raises "stop" errors when Objects cannot be found by full path (previously returned NULL)
* Adds support for custom client environments (solvebio::createEnv)
* Removes deprecated Upload methods


# solvebio 2.0.1 / 2.0.2

* Bug fixes 


# solvebio 2.0.0

* Add support for Vaults, Objects (Vault Objects)
* Remove deprecated version 1 methods for Depository and DepositoryVersion
* Upgrade to version 2 endpoints for some methods


# solvebio 0.4.0

* Added support for uploads, dataset imports, migrations, and exports
* Uses dplyr bind_rows to handle JSON inconsistencies in data


# solvebio 0.3.0

* Adds`Dataset.count()` and `Dataset.facets()` methods
* Adds NEWS.md file


# solvebio 0.2.0

* Adds automatic pagination support to `Dataset.query()`


# solvebio 0.1.0

* First alpha release, basic SolveBio API support for querying datasets

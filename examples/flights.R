# In this example we will analyze the `nycflights13` dataset using
# nested aggregation queries on SolveBio.

# Dependencies:
# install.packages("nycflights13")
# install.packages("jsonlite")
# install.packages("solvebio")

library(nycflights13)
library(jsonlite)
library(solvebio)

solvebio::login()

# Set up the flights dataset on SolveBio:
vault <- Vault.get_personal_vault()
dataset_full_path <- paste(vault$full_path, "/flights", sep=":")
dataset <- Dataset.get_or_create_by_full_path(dataset_full_path)

# Avoid importing the data twice
if (is.null(dataset$documents_count) || dataset$documents_count == 0) {
    # Save, compress, and upload the JSON data to your personal vault
    gzfile <- gzfile("flights.json.gz", "w")
    stream_out(flights, con = gzfile)
    close(gzfile)
    object <- Object.upload_file("flights.json.gz", vault$id, '/')

    # Import the dataset and wait until done
    # Since the year, month, and day fields are integersn in the source,
    # add string fields for easier aggregations.
    target_fields = list( 
        list(
            name = 'year_str',
            data_type = 'string',
            expression = 'record.year'
        ),
        list( 
            name = 'month_str',
            data_type = 'string',
            expression = 'record.month'
        ),
        list(
            name = 'day_str',
            data_type = 'string',
            expression = 'record.day'
        )
    )
    import <- DatasetImport.create(dataset_id=dataset$id,
                                   object_id=object$id,
                                   target_fields=target_fields)

    while(import$status == "queued" || import$status == "running") {
        import <- DatasetImport.retrieve(import$id)
        cat(import$status, sep="\n")
        Sys.sleep(3)
    }
}


# Example 1: Find the number of planes and flights to each destination.
#
# In this example, we use nested cardinality facets to
# calculate the results in a single query.

cat("# Example 1: Flights and planes per destination\n")

# Prepare the destination ("dest") field facets parameters.
# The basic terms facet will calculate the total number
# of records (i.e. total number of flights) for each destination.
dest_facets = list(
    # By default, it will return the top 10.
    # Override 'limit' to get the top 10k.
    limit = 10000,
    facets = list(
        # For each destination, get the count of unique
        # tail numbers (i.e. planes).
        tailnum = list(
            facet_type = 'count'
        )
    )
)

results <- Dataset.facets(id = dataset$id, facets=list(dest = dest_facets))

# Convert the nested facets into a data frame
n <- length(results[['dest']])
flights_per_dest <- data.frame(dest = character(n),
                               n_flights = numeric(n),
                               n_tailnums = numeric(n),
                               stringsAsFactors = FALSE)

i <- 1;
for (row in results[['dest']]) {
    # Each row is a list with three elements:
    # dest, n_flights, sub-facets (unique tailnums)
    flights_per_dest[["dest"]][i] = row[[1]]
    flights_per_dest[["n_flights"]][i] = row[[2]]
    flights_per_dest[["n_tailnums"]][i] = row[[3]]$tailnum
    i <- i + 1
}

flights_per_dest


# Example 2: Calculate the number of flights per year, month, and day.
#
# For each year, calculate the number of flights per month,
# and for each month, calculate the number per day.

cat("# Example 2: Flights per day\n")

year_facets = list(
    limit = 100,
    # For each year, aggregate by month
    facets = list(
        month_str = list( 
            limit = 12,
            # For each month, aggregate by day
            facets = list(
                day_str = list(
                    limit = 31
                )
            )
        )
    )
)

results <- Dataset.facets(id = dataset$id, facets=list(year_str = year_facets))

# Convert the nested facets into a data frame
flights_per_day <- data.frame(date = as.Date(rep(0, 365), origin = "1900-01-01"),
                 n_flights = numeric(365),
                 stringsAsFactors = FALSE)
i <- 1
for (row_year in results[['year_str']]) {
    for (row_month in row_year[[3]][['month_str']]) {
        for (row_day in row_month[[3]][['day_str']]) {
            date <- paste(row_year[[1]], row_month[[1]], row_day[[1]], sep="-")
            flights_per_day[["date"]][i] = date
            flights_per_day[["n_flights"]][i] = row_day[[2]]
            i <- i + 1
        }
    }
}

flights_per_day[order(flights_per_day$date), ]

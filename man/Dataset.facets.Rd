% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{Dataset.facets}
\alias{Dataset.facets}
\title{Dataset.facets}
\usage{
Dataset.facets(id, facets, env = .solveEnv, ...)
}
\arguments{
\item{id}{The ID of a SolveBio dataset, or a Dataset object.}

\item{facets}{A list of one or more field facets.}

\item{env}{(optional) Custom client environment.}

\item{...}{(optional) Additional query parameters (e.g. filters, limit, offset).}
}
\description{
Retrieves aggregated statistics or term counts for one or more fields in a SolveBio dataset. Returns a list of data frames, one for each requested facet.
}
\examples{
\dontrun{
Dataset.facets("1234567890", list("clinical_significance", "gene_symbol"))
}

}
\references{
\url{https://docs.solvebio.com/}
}

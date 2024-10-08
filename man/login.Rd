% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solvebio.R
\name{login}
\alias{login}
\title{login}
\usage{
login(api_key, api_host, env = .solveEnv)
}
\arguments{
\item{api_key}{Your SolveBio API key}

\item{api_host}{SolveBio API host (default: https://api.solvebio.com)}

\item{env}{(optional) The R environment used to store API credentials.}
}
\description{
Store and verify your SolveBio credentials.
}
\examples{
\dontrun{
login()
}

}
\references{
\url{https://docs.solvebio.com/}
}

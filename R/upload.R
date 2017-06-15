#' Upload.all
#'
#' Retrieves the metadata about all uploads on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' Upload.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Upload.all <- function(...) {
    .request('GET', "v1/uploads", query=list(...))
}

#' Upload.retrieve
#'
#' Retrieves the metadata about a specific upload from SolveBio.
#'
#' @param id The ID a SolveBio upload
#'
#' @examples \dontrun{
#' Upload.retrieve(123)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Upload.retrieve <- function(id) {
    if (missing(id)) {
        stop("An upload ID is required.")
    }

    path <- paste("v1/uploads", paste(id), sep="/")
    .request('GET', path=path)
}

#' Upload.create
#'
#' Upload a local file to SolveBio.
#'
#' @param path The path to the local file
#'
#' @examples \dontrun{
#' Upload.create("my_file.json.gz")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Upload.create <- function(path) {
    if (missing(path) || !file.exists(path)) {
        stop("A valid path to a local file is required.")
    }

    params <- list(
                   filename = basename(path),
                   mimetype = mime::guess_type(path),
                   size = file.size(path),
                   md5 = digest::digest(file=path)
                   )

    upload <- .request('POST', path='v1/uploads', query=NULL, body=params)

    headers <- c(
                 'Content-MD5' = upload$base64_md5,
                 'Content-Type' = upload$mimetype,
                 'Content-Length' = upload$size
                 )

    res <- httr::PUT(
                     upload$s3_upload_url,
                     httr::add_headers(headers),
                     body = httr::upload_file(path, type=upload$mimetype)
                     )

    return(upload)
}

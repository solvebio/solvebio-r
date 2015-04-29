# Generics for SolveBio R bindings

# Helper function for creating generics only once
setGenericIfNone <- function(x,y){
    if(!isGeneric(x)) {
        setGeneric(x,y)
    }
    else {
        # Do nothing
    }
}


# setGenericIfNone("as.list", function(x, ...) standardGeneric("as.list"),
#                  useAsDefault = function(x, ...) base::as.list(x, ...))

# SolveBioClient methods
setGenericIfNone("connect", function(x, ...) standardGeneric("connect"))
setGenericIfNone("login", function(x, ...) standardGeneric("login"))
setGenericIfNone("isAuthenticated", function(x, ...) standardGeneric("isAuthenticated"))

setGenericIfNone("Depositories", function(x, ...) standardGeneric("Depositories"))

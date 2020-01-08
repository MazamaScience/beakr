#' @export
#' @title GET-binding middleware
#'
#' @description Routes HTTP getr requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance or \code{NULL}.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage getr(beakr, path, ...)
#'
#' @examples
#' \dontrun{
#' createBeakr() %>%
#'   getr("/", function(req, res, err) {
#'     return("Successful getr request!\n")
#'   }) %>%
#'   listen()
#'
#' # In browser:
#' #   http://127.0.0.1:8080
#' # > Successful getr request!
#' }
#'
getr <- function(beakr, path = NULL, ...) {
  # If the beakr is NULL ->
  # create "bundle" beakr for inlcuding in other beakrs
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  FUNS <- list(...)
  lapply(
    X   = FUNS,
    FUN = function(middleware_FUN) {
      routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = "GET" )
    }
  )
  return(beakr)
}

#' @export
#' @title POST-binding middleware
#'
#' @description Routes HTTP postr requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage postr(beakr, path, ...)
#'
#' @examples
#' \dontrun{
#' createBeakr() %>%
#'   postr("/", function(req, res, err) {
#'     return("Successful postr request!\n")
#'   }) %>%
#'   listen()
#'
#' # In terminal:
#' #  curl -X postr http://127.0.0.1:8080/
#' # > Successful postr request!
#' }
postr <- function(beakr, path = NULL, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  FUNS <- list(...)
  lapply(
    X = FUNS,
    FUN = function(middleware_FUN) {
      routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = "POST" )
    }
  )
  return(beakr)
}

#' @export
#' @title PUT-binding middleware
#'
#' @description Routes HTTP putr requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage putr(beakr, path, ...)
#'
#' @examples
#' \dontrun{
#' createBeakr() %>%
#'   putr("/", function(req, res, err) {
#'     return("Successful putr request!\n")
#'   }) %>%
#'   listen()
#'
#' # In terminal:
#' #  curl -X putr http://127.0.0.1:8080/
#' # > Successful putr request!
#' }
#'
putr <- function(beakr, path = NULL, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = "PUT" )
    }
  )
  return(beakr)
}

#' @export
#' @title DELETE-binding middleware
#'
#' @description Routes HTTP deleter requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage deleter(beakr, path, ...)
deleter <- function(beakr, path = NULL, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = "DELETE" )
    }
  )
  return(beakr)
}

#' GET-binding middleware
#'
#' Routes HTTP GET requests to the specified path with the specified callback
#' functions or middleware.
#'
#' @param beakr a beakr instance or \code{NULL}.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @export
GET <- function(beakr, path = NULL, ...) {
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

#' POST-binding middleware
#'
#' Routes HTTP POST requests to the specified path with the specified callback
#' functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage POST(beakr, path, ...)
#' @export
POST <- function(beakr, path = NULL, ...) {
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

#' PUT-binding middleware
#'
#' Routes HTTP PUT requests to the specified path with the specified callback
#' functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage PUT(beakr, path, ...)
#' @export
#'
PUT <- function(beakr, path = NULL, ...) {
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

#' DELETE-binding middleware
#'
#' Routes HTTP DELETE requests to the specified path with the specified callback
#' functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage DELETE(beakr, path, ...)
#' @export
DELETE <- function(beakr, path = NULL, ...) {
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

#' @export
#' @title GET-binding middleware
#'
#' @description Routes HTTP GET requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance or \code{NULL}.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage http_get(beakr, path, ...)
#' @return A `beakr` App object with added middleware.
#'
#' @examples
#' \dontrun{
#' new_beakr() %>%
#'   http_get("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen()
#'
#' # In browser:
#' #   http://127.0.0.1:8080
#' # > Successful GET request!
#' }
#'
http_get <- function(beakr, path = NULL, ...) {
  # If the beakr is NULL ->
  # create "bundle" beakr for inlcuding in other beakrs
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  FUNS <- list(...)
  lapply(
    X   = FUNS,
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
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
#' @description Routes HTTP POST requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage http_post(beakr, path, ...)
#' @return A `beakr` App object with added middleware.
#'
#' @examples
#' \dontrun{
#' new_beakr() %>%
#'   http_post("/", function(req, res, err) {
#'     return("Successful POST request!\n")
#'   }) %>%
#'   listen()
#'
#' # In terminal:
#' #  curl -X POST http://127.0.0.1:8080/
#' # > Successful POST request!
#' }
http_post <- function(beakr, path = NULL, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  FUNS <- list(...)
  lapply(
    X = FUNS,
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
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
#' @description Routes HTTP PUT requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage http_put(beakr, path, ...)
#' @return A `beakr` App object with added middleware.
#'
#' @examples
#' \dontrun{
#' new_beakr() %>%
#'   http_put("/", function(req, res, err) {
#'     return("Successful PUT request!\n")
#'   }) %>%
#'   listen()
#'
#' # In terminal:
#' #  curl -X PUT http://127.0.0.1:8080/
#' # > Successful PUT request!
#' }
#'
http_put <- function(beakr, path = NULL, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
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
#' @description Routes HTTP DELETE requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage http_delete(beakr, path, ...)
#' @return A `beakr` App object with added middleware.
http_delete <- function(beakr, path = NULL, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
                        FUN    = middleware_FUN,
                        path   = path,
                        method = "DELETE" )
    }
  )
  return(beakr)
}

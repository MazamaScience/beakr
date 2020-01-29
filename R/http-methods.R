#' @export
#' @title GET-binding middleware
#'
#' @description Routes HTTP GET requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param path String representing a relative path for which the middleware
#' is invoked.
#' @param ... Additional middleware/functions.
#'
#' @return A \code{Beakr} object with added middleware.
#'
#' @examples
#' \dontrun{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#'
#' # Create a simple beakr pipeline
#' beakr %>%
#'   httpGET("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen(host = '127.0.0.1', port = 12518, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X GET http://127.0.0.1:12518/
#' # > Successful GET request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpGET <- function(beakr, path = NULL, ...) {

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
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param path String representing a relative path for which the middleware
#' is invoked.
#' @param ... Additional middleware/functions.
#'
#' @return A \code{Beakr} object with added middleware.
#'
#' @examples
#' \dontrun{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#'
#' # Create a simple beakr pipeline
#' beakr %>%
#'   httpPOST("/", function(req, res, err) {
#'     return("Successful POST request!\n")
#'   }) %>%
#'   listen(host = '127.0.0.1', port = 12518, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X POST http://127.0.0.1:12518/
#' # > Successful POST request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpPOST <- function(beakr, path = NULL, ...) {

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
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param path String representing a relative path for which the middleware
#' is invoked.
#' @param ... Additional middleware/functions.
#'
#' @return A \code{Beakr} object with added middleware.
#'
#' @examples
#' \dontrun{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#'
#' # Create a simple beakr pipeline
#' beakr %>%
#'   httpPUT("/", function(req, res, err) {
#'     return("Successful PUT request!\n")
#'   }) %>%
#'   listen(host = '127.0.0.1', port = 12518, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X PUT http://127.0.0.1:12518/
#' # > Successful PUT request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpPUT <- function(beakr, path = NULL, ...) {

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
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param path String representing a relative path for which the middleware
#' is invoked.
#' @param ... Additional middleware/functions.
#'
#' @return A \code{Beakr} object with added middleware.
#'
#' @examples
#' \dontrun{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#'
#' # Create a simple beakr pipeline
#' beakr %>%
#'   httpDELETE("/", function(req, res, err) {
#'     return("Successful DELETE request!\n")
#'   }) %>%
#'   listen(host = '127.0.0.1', port = 12518, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X DELETE http://127.0.0.1:12518/
#' # > Successful DELETE request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpDELETE <- function(beakr, path = NULL, ...) {

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

#' @title Middleware class
#'
#' The \code{Middleware} object represents middleware functions that have access
#' to the (\code{req}) object, response object (\code{res}) and (\code{err})
#' object in the request-response cycle via \code{RequestHandler}.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{path}}{
#'   Returns the path for the specified middleware.
#'   }
#'   \item{\code{FUN}}{
#'   Returns the function response.
#'   }
#'   \item{\code{method}}{
#'   Returns the HTTP method for the middleware, i.e. "GET", "POST", etc.
#'   }
#'   \item{\code{protocol}}{
#'   Returns the protocol, "http" or "websocket".
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{initialize(FUN, path, method, websocket)}}{
#'   Initializes the state of new middleware.
#'   }
#' }
#'
#' @seealso \code{\link{RequestHandler}} and \code{\link{Middleware}}
#' @keywords internal
Middleware <-
  R6::R6Class(
    classname = "Middleware",
    public = list(
      path = NULL,
      FUN = NULL,
      method = NULL,
      protocol = NULL,
      initialize = function(FUN, path, method, websocket) {
        self$FUN = FUN
        self$path = path
        self$method = method
        self$protocol = if ( websocket )  {
            return("websocket")
          } else {
            return("http")
          }
      }
    )
  )

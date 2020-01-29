#' @export
#' @title GET-binding middleware
#'
#' @description Routes HTTP GET requests to the specified path with the
#' specified callback functions or middleware.
#'
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param path String representing a path for which the middleware
#' function is invoked.
#' @param FUN Middleware function to be invoked.
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
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X GET http://127.0.0.1:25118/
#' # > Successful GET request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpGET <- function(beakr, path = NULL, FUN = NULL) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  if ( is.null(FUN) )
    stop("'FUN' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = FUN,
      path = path,
      method = "GET",
      websocket = FALSE
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
#' @param path String representing a path for which the middleware
#' function is invoked.
#' @param FUN Middleware function to be invoked.
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
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X POST http://127.0.0.1:25118/
#' # > Successful POST request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpPOST <- function(beakr, path = NULL, FUN = NULL) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  if ( is.null(FUN) )
    stop("'FUN' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = FUN,
      path = path,
      method = "POST",
      websocket = FALSE
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
#' @param path String representing a path for which the middleware
#' function is invoked.
#' @param FUN Middleware function to be invoked.
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
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X PUT http://127.0.0.1:25118/
#' # > Successful PUT request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpPUT <- function(beakr, path = NULL, FUN = NULL) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  if ( is.null(FUN) )
    stop("'FUN' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = FUN,
      path = path,
      method = "PUT",
      websocket = FALSE
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
#' @param path String representing a path for which the middleware
#' function is invoked.
#' @param FUN Middleware function to be invoked.
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
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # IN A TERMINAL:
#' #  curl -X DELETE http://127.0.0.1:25118/
#' # > Successful DELETE request!
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }

httpDELETE <- function(beakr, path = NULL, FUN = NULL) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  if ( is.null(FUN) )
    stop("'FUN' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = FUN,
      path = path,
      method = "DELETE",
      websocket = FALSE
    )

  return(beakr)

}

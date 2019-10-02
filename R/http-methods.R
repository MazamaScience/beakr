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
#' @examples
#' \dontrun{
#' # Create a beakr instance
#' server <- newBeakr() %>%
#'   get("/", function(req, res, err) { "Hello, world!" })
#' # Listen for HTTP/WebSocket requests
#' listen(server, daemonized = TRUE)
#' # In terminal:
#' # $ curl http://127.0.0.1:8080/
#' # > Hello, world!
#' }
# The methods below are used to determine what "get" to use.
# get <- function(beakr, path, ...) {
#   if ( is.null(beakr) ) {
#     return(get.Beakr(beakr, path, ...))
#   } else {
#     return(UseMethod("get"))
#   }
# }
# @export
# get.default <- function(beakr, ...) {
#   return(base::get(beakr, ...))
# }
# @describeIn get Beakr middleware function.
# @export
get <- function(beakr, path, ...) {
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
#' @usage post(beakr, path, ...)
#' @export
#' @examples
#' \dontrun{
#' # Create a beakr instance
#' server <- newBeakr() %>%
#'   post("/", function(req, res, err) { "Successful POST request!" })
#' # Listen for HTTP/WebSocket requests
#' listen(server)
#' # In terminal:
#' # $ curl -X POST http://127.0.0.1:8080/
#' # > Successful POST request!
#' }
post <- function(beakr, path, ...) {
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
#' @usage put(beakr, path, ...)
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a beakr instance
#' server <- newBeakr() %>%
#'   put("/", function(req, res, err) { "Successful PUT request!" })
#' # Listen for HTTP/WebSocket requests
#' listen(server)
#' # In terminal:
#' # $ curl -X PUT http://127.0.0.1:8080/
#' # Successful PUT request!
#' }
put <- function(beakr, path, ...) {
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
#' @usage delete(beakr, path, ...)
#' @export
delete <- function(beakr, path, ...) {
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

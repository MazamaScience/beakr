#' @export
#' @title Create a new Beakr instance
#'
#' @param name an optional name assigned to the \emph{Beakr} object.
#'
#' @description Create a \emph{Beakr} instance object by calling the top-level
#' \code{newBeakr()} function. If \code{name} is not supplied, a random name
#' will be assigned.
#'
#' @usage newBeakr(name = NULL)
#' @return A new and empty `beakr` App.
#' @examples
#' \dontrun{
#' # Standard
#' beakr <- newBeakr()
#' listen(beakr)
#' # Pipe
#' newBeakr() %>% listen()
#' }
newBeakr <- function(name = NULL) {
  beakr <- Beakr$new()
  if ( !is.null(name) ) {
    beakr$name <- name
  }
  return(beakr)
}

#' @export
#' @title Listen for connections on a beakr instance
#'
#' @description Binds and listens for connections on the specified host and port.
#'
#' @details
#' \code{listen()} binds the port and listens for connections on a thread.
#' The thread handles the I/O, and when it receives a HTTP request, it
#' will schedule a call to the user-defined middleware and handle the
#' request.
#'
#' If the daemon boolean value is set to \code{TRUE}, \code{listen()} binds
#' the specified port and listens for connections on a thread running in the
#' background.
#'
#' See the \code{httpuv} package for more information.
#'
#' @param beakr a beakr instance.
#' @param host a string that is a valid IPv4 or IPv6 address to listen on.
#' Defaults to the local host ("127.0.0.1").
#' @param port a number or integer that indicates the port to listen on. Default
#' is a port opened on 8080.
#' @param daemon run the instance in the background, the default is FALSE.
#'
#' @usage listen(beakr, host, port, daemon)
#' @return A `beakr` App object.
#' @examples
#' \dontrun{
#' # Run in foreground
#' newBeakr() %>%
#'   httpGET("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen()
#'
#' # Run in background
#' #' newBeakr() %>%
#'   httpGET("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen(daemon = TRUE)
#' }
listen <- function(
  beakr,
  host = "127.0.0.1",
  port = 8080,
  daemon = FALSE
) {
  message(paste0("Serving beakr instance at http://", host, ":", port))
  beakr$start(host, port, daemon)
  return(beakr)
}

#' @export
#' @title Stop the beakr instance
#'
#' @description Stops an active instance when given a beakr object. This closes
#' all open connections for the instance and unbinds the port.
#'
#' @param beakr a beakr instance.
#'
#' @usage kill(beakr)
#' @return None
#'
#' @examples
#' \donttest{
#' beakr <- newBeakr()
#' listen(beakr, daemon = TRUE)
#' kill(beakr)
#' }
kill <- function(beakr) {
  httpuv::stopServer(beakr$server)
  cat("Stopped ")
  beakr$print()
}

#' @export
#' @title Stop all beakr instances
#'
#' @description Stops all instances that have been activated by
#' \code{\link{listen}} in the session.
#'
#' @usage kill_all()
#' @return None
#' @seealso \code{\link{kill}} and \code{\link{listen}}
#' @examples
#' \dontrun{
#' newBeakr() %>% listen(daemon = TRUE)
#' # Kill all instances
#' kill_all()
#' ## Stopped All Instances
#' }
kill_all <- function() {
  httpuv::stopAllServers()
  cat("Stopped All Instances\n")
}

#' @export
#' @title List active instances
#'
#' @description A list containing the current sessions active instances.
#'
#' @return a list of active instances
#'
#' @usage list_active()
#' @return A `list` of all active instances
#' @examples
#' \dontrun{
#' newBeakr('Test1') %>% listen(port = 1234, daemon = TRUE)
#' newBeakr('Test2') %>% listen(port = 2234, daemon = TRUE)
#' list_active()
#' ## [[1]]
#' ## [1] "Host: 127.0.0.1 | Port: 1234 | Active: TRUE"
#'
#' ## [[2]]
#' ## [1] "Host: 127.0.0.1 | Port: 2234 | Active: TRUE"
#' }
list_active <- function() {
  active <- lapply(
    X = httpuv::listServers() ,
    FUN = function(s) {
      paste( paste0("Host: ", s$getHost()),
             paste0("Port: ", s$getPort()),
             paste0("Active: ", "TRUE"),
             sep = " | " )
    }
  )
  return(active)
}

#' @export
#' @title Error handling middleware for a beakr instance
#'
#' @description This default error-handling middleware function should be added
#' at the end of the middleware function pipeline. Any errors will be returned
#' within a JSON wrapper.
#'
#' @param beakr Beakr instance
#' @param path Path for which the middleware is invoked, typically \code{NULL}.
#'
#' @return A `beakr` App object with added middleware.

handleErrors <- function(beakr, path = NULL) {

  beakr <- .routeMiddleware( beakr  = beakr,
                             FUN    = .jsonError,
                             path   = path,
                             method = NULL )


  return(beakr)

}

#' @export
#' @title Test request
#'
#' @param beakr the beakr instance
#' @param test_request the TestRequest instance
#'
test_request <- function(beakr, test_request) {
  beakr$router$invoke(test_request)
}

#' @export
#' @title Websocket Access
#'
#' @description Access the instance through websocket. \strong{Experimental.}
#'
#' @param beakr a beakr instance
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#'
#' @usage websocket(beakr, path, ...)
#' @return A `beakr` App object with added middleware.
#' @examples
#' \dontrun{
#' newBeakr() %>%
#'   websocket('/ws_test', function(req, res, err) 'Websocket Test') %>%
#'   listen()
#' }
websocket <- function(beakr, path, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = NULL,
                       websocket = TRUE)
    }
  )
  return(beakr)
}

#' @export
#' @title Serve static files
#'
#' @description Binds to GET requests that aren't handled by specified paths.
#' Should support all filetypes; returns image and octet-stream types as a raw
#' string.
#'
#' @param beakr a beakr instance
#' @param path a string representing a relative path for which the middleware
#' is invoked.
#' @param file a string representing the path to the file that is to be served.
#'
#' @details Serve static files from the host machine. Currently supports images,
#' PDF, JSON, HTML, and raw text. The static file will be served on the `path`
#' specified in addition to the file name and extension. For example, an image
#' `example_dir/path_to_pictures/pic.png` will be served on the URL extension
#' `/path/pic.png`.
#' @return A `beakr` App object with added middleware.
#' @examples
#' \dontrun{
#' path_to_file <- "example_dir/path_to_pictures/pic.png"
#' newBeakr() %>%
#'   static(path = 'readme/', file = path_to_file) %>%
#'   listen()
#' }
static <- function(beakr, path = NULL, file = NULL) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  serve_file <- function(req, res, err) {

    if ( file.exists(file) ) {

      url_path <- paste0('/', path, utils::tail(unlist(strsplit(file, '/')), n = 1))

      if ( req$path == url_path ) {
        mime_type <- mime::guess_type(file)
        res$setContentType(mime_type)
        data <- readBin( con  = file,
                         what = "raw",
                         n    = file.info(file)$size )
        if ( grepl("image|octect|pdf|json", mime_type) ) { # Assumptions...
          return(data)
        } else {
          return(rawToChar(data))
        }
      } else {
        res$setStatus(404L)
        return(NULL)
      }

    }

  }

  return(httpGET(beakr = beakr, path = NULL, serve_file))

}

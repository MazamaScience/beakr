#' @export
#' @title Create a new Beakr instance
#'
#' @param name an optional name assigned to the \emph{Beakr} object.
#'
#' @description Create a \code{Beakr} instance object by calling the top-level
#' \code{beakr()} function. If \code{name} is not supplied, a random name
#' will be assigned.
#'
#' @usage beakr(name = NULL)
#' @return A new and empty `beakr` App.
#' @examples
#' \donttest{
#' # Standard
#' app <- beakr()
#' listen(app)
#' # Pipe
#' beakr() %>% listen()
#' }
beakr <- function(name = NULL) {
  beakr <- App$new()
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
#' @raturn A `beakr` App object.
#' @examples
#' \donttest{
#' # Run in foreground
#' beakr() %>%
#'   http_get("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen()
#'
#' # Run in background
#' #' beakr() %>%
#'   http_get("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen(daemon = TRUE)
#' }
listen <-function(
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
#' beakr <- beakr()
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
#' \donttest{
#' beakr() %>% listen(daemon = TRUE)
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
#' \donttest{
#' beakr('Test1') %>% listen(port = 1234, daemon = TRUE)
#' beakr('Test2') %>% listen(port = 2234, daemon = TRUE)
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
#' @title Instance Error Handling
#'
#' @description An error handling middleware for beakr instances.
#'
#' @details
#' \code{beakr} comes with a default error handler. This default error-handling
#' middleware function is added at the end of the middleware function stack.
#' The Errors are handled via JSON output.
#'
#' @param beakr a beakr instance
#' @param path string representing a relative path for which the middleware
#' is invoked.
#'
#' @usage error_handler(beakr, path)
#' @return A `beakr` App object with added middleware.
error_handler <- function(beakr, path = NULL) {
  use(
    beakr = beakr,
    path = NULL,
    function(req, res, err) {
      res$setContentType("application/json")
      if ( err$occurred ) {
        res$status <- 500L
        error_str <- paste(err$errors, collapse = "\n")

        cat("ERROR:\n", error_str, "\n")

        res$json(list( status = "error",
                       status_code = 500L,
                       errors = error_str ))

      } else {
        res$status = 404L
        res$json(list( status = "Page not found.",
                       status_code = 404L ))
      }
    }
  )
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
#' \donttest{
#' beakr() %>%
#'   websocket('/ws_test', function(req, res, err) 'Websocket Test') %>%
#'   listen()
#' }
websocket <- function(beakr, path, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(App$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = NULL )
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
#' beakr() %>%
#'   static(path = 'readme/', file = path_to_file) %>%
#'   listen()
#' }
static <- function(beakr, path = NULL, file = NULL) {
  if ( is.null(beakr) ) {
    beakr <- invisible(App$new())
  }
  serve_file <- function(req, res, err) {

    if ( file.exists(file) ) {

      url_path <- paste0('/', path, tail(unlist(strsplit(file, '/')), n = 1))

      if ( req$path == url_path ) {
        mime_type <- mime::guess_type(file)
        res$setContentType(mime_type)
        data <- readBin( con  = file,
                         what = "raw",
                         n    = file.info(file)$size )
        if( grepl("image|octect|pdf|json", mime_type) ) { # Assumptions...
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

  return(http_get(beakr = beakr, path = NULL, serve_file))

}

#' @export
#' @title Use request method-insensitive middleware
#'
#' @description Mounts the specified middleware function or functions at the
#' specified path: the middleware function is executed when the base of the
#' requested path matches path. to the specified path with the specified
#' callback functions or middleware.
#'
#' @param beakr a beakr instance.
#' @param path string representing a relative path for which the middleware
#' is invoked.
#' @param ... additional middleware/functions.
#' @param method an optional HTTP request method.
#'
#' @usage use(beakr, path, ..., method)
use <- function(beakr, path, ..., method = NULL) {
  if ( is.null(beakr) ) {
    beakr <- invisible(App$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      .routeMiddleware( beakr  = beakr,
                       FUN    = middleware_FUN,
                       path   = path,
                       method = method )
    }
  )
  return(beakr)
}


#' @export
#' @title Add CORS-headers middleware
#'
#' @description Add Cross-Origin Resource Sharing (CORS) middleware to the
#' instance.
#'
#' @details
#' CORS is a mechanism that uses additional HTTP headers to tell browsers to
#' give a web application running at one origin, access to selected resources
#' from a different origin. A web application executes a cross-origin HTTP
#' request when it requests a resource that has a different origin
#' (domain, protocol, or port) from its own.
#'
#' Additional Info:
#' https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
#'
#' @param beakr a beakr instance.
#' @param path a string path.
#' @param with_methods set \code{Access-Control-Allow-Methods} response header
#' that specifies the method or methods allowed when accessing the resource in
#' response to a preflight request.
#' @param with_origin set \code{Access-Control-Allow-Origin} response header
#' indicating whether the response can be shared with requesting code from the
#' given origin.
#' @param with_headers set \code{Acess-Control-Allow-Headers} to indicate which HTTP
#' headers can be used during the actual request.
#' @param with_credentials set \code{Access-Control-Allow-Credentials} response header.
#' @param max_age set \code{Access-Control-Max-Age} in seconds.
#' @param expose_headers set \code{Access-Control-Expose-Headers}  indicate which
#' headers can be exposed as part of the response.
#'
cors <- function(
  beakr,
  path = NULL,
  with_methods = c("POST", "GET", "PUT", "OPTIONS", "DELETE", "PATCH"),
  with_origin = "*",
  with_headers = NULL,
  with_credentials = NULL,
  max_age = NULL,
  expose_headers = NULL
) {

  if ( !is.null(with_headers) ) {
    with_headers <- paste0(with_headers, collapse = ",")
  }
  if ( !is.null(with_methods) ) {
    with_methods <- paste0(with_methods, collapse = ",")
  }

  if ( is.null(beakr) ) {
    beakr <- invisible(App$new())
  }

  headers <- list( `Access-Control-Allow-Origin`      = with_origin,
                   `Access-Control-Expose-Headers`    = expose_headers,
                   `Access-Control-Max-Age`           = max_age,
                   `Access-Control-Allow-Credentials` = with_credentials,
                   `Access-Control-Allow-Methods`     = with_methods,
                   `Access-Control-Allow-Headers`     = with_headers )

  headers <- Filter(f = function(x) { !is.null(x) }, x = headers)

  FUN <- function(req, res, err) {

    if ( req$method == "OPTIONS" ) {
      res$setHeader("Access-Control-Allow-Methods", with_methods)
      res$setHeader("Access-Control-Allow-Origin", with_origin)
      res$setHeader("Access-Control-Allow-Headers", with_headers)
      # Return empty string stops process
      return("")
    }

    lapply(
      X = names(headers),
      FUN = function(header_name) {
        res$setHeader(header_name, headers[[header_name]])
      }
    )

    return(NULL)
  }

  .routeMiddleware(beakr = beakr, FUN = FUN, path = path, method = NULL)

  return(beakr)
}

#' @export
#' @title Construct an instance
#'
#' @description The \code{include} function is used to merge beakr middleware
#' that has been constructed elsewhere.
#'
#' @details Hierarchy
#'
#' @param beakr primary beakr instance.
#' @param include the external middleware to include.
#' @param file the source file path, if external middleware in separate .R file.
#'
#' @usage include(beakr, include, file = NULL)
include <- function(beakr, include, file = NULL) {
  if ( is.null(file) ) {
    bundle <- eval(quote(include))
  } else {
    tempenv <- new.env()
    source(file, local = tempenv)
    bundle <- eval(quote(include), envir = tempenv)
  }

  beakr$include(bundle)

  return(beakr)
}

#' @title Beakr Event Listener
#'
#' @description Add an event listener to a \emph{beakr} instance. Currently
#' supported events are \code{"start", "finish", "error"}. The events \code{"start"} and
#' \code{"finish"} will pass the current state of the \code{req}, \code{res} and \code{err}
#' objects to the Listener. The \code{"error"} event will pass string error message.
#'
#' @param beakr a beakr instance.
#' @param event the event to listen for, (\emph{"start", "finish", "end"}).
#' @param FUN the response middleware function.
#'
#' @export
on <- function(beakr, event, FUN) {
  .addListener(beakr = beakr, event = event, FUN = FUN)
  return(beakr)
}

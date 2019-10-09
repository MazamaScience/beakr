#' @export
#' @title Start a new Beakr instance
#'
#' @description Create a \code{Beakr} instance object by calling the top-level
#' \code{newBeakr()} function.
#'
#' @usage newBeakr()
newBeakr <- function() {
  Beakr$new()
}

#' @export
#' @title Start a beakr instance and listen for connections
#'
#' @description Binds and listens for connections on the specified host and port.
#'
#' @details
#' \code{startBeakr()} binds the port and listens for connections on a thread.
#' The thread handles the I/O, and when it receives a HTTP request, it
#' will schedule a call to the user-defined middleware and handle the
#' request.
#'
#' If the daemon boolean value is set to \code{TRUE}, \code{startBeakr()} binds
#' the specified port and listens for connections on an thread running in the
#' background.
#'
#' See the \code{httpuv} package for more information.
#'
#' @param beakr a beakr instance.
#' @param host a string that is a valid IPv4 or IPv6 address to listen on.
#' @param port a number or integer that indicates the port to listen on.
#' @param daemonized run the instance in the background.
#' @param verbose boolean, debugging.
#'
#' @usage startBeakr(beakr, host, port, daemonized, verbose)
#'
#' @examples
#' \dontrun{
#' newBeakr() %>%
#'   GET("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   startBeakr()
#' }
#'
startBeakr <-function(
  beakr,
  host = "127.0.0.1",
  port = 8080,
  daemonized = FALSE,
  verbose = FALSE
) {
  options("beakr.verbose" = verbose)
  message(paste0("Serving beakr instance at http://", host, ":", port))
  beakr$start(host, port, daemonized)
  return(beakr)
}

#' @export
#' @title Create a new Error
#'
#' @description Used to handle errors.
#'
#' @usage newError()
#'
# TODO:  Is NewError() necessary?
newError <- function() {
  Error$new()
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
#'
#' @examples
#' \donttest{
#' beakr <- newBeakr()
#' startBeakr(beakr, daemonized = TRUE)
#' kill(beakr)
#' }
#'
kill <- function(beakr) {
  httpuv::stopServer(beakr$serverObject)
  cat("Stopped ")
  beakr$print()
}

#' @export
#' @title Stop all beakr instances
#'
#' @description Stops all instances that have been activated by
#' \code{\link{startBeakr}} in the session.
#'
#' @usage killAll()
#'
#' @seealso \code{\link{kill}} and \code{\link{startBeakr}}
killAll <- function() {
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
#' @usage active()
active <- function() {
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
#' @usage errorHandler(beakr, path)
#'
errorHandler <- function(beakr, path = NULL) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  jsoner <- function(req, res, err) {
    res$contentType("application/json")
    if ( err$occurred ) {
      error_str <- paste(err$errors, collapse = "\n")
      res$status <- 500L
      res$json(list( status = "err",
                     status_code = 500L,
                     errors = error_str ))
      if ( getOption("beakr.verbose") ) {
        cat("ERROR:\n", error_str, "\n")
      }

    } else {
      res$status = 404L
      res$json(list( status = "Page not found.",
                     status_code = 404L ))
    }
  }

  return(use(beakr = beakr, path = path, method = NULL, jsoner))
}

#' @export
#' @title Test request
#'
#' @param beakr the beakr instance
#' @param test_request the TestRequest instance
#'
processTestRequest <- function(beakr, test_request) {
  beakr$routerObject$invoke(test_request)
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
#' @usage webSocket(beakr, path, ...)
webSocket <- function(beakr, path, ...) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      routeMiddleware( beakr  = beakr,
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
#' @description Binds to get requests that aren't handled by specified paths.
#' Should support all filetypes; returns image and octet-stream types as a raw
#' string.
#'
#' @param beakr a beakr instance
#' @param path a string representing a relative path for which the middleware
#' is invoked.
#' @param dir a string representing a path for which to serve as the root
#' directory.
#'
static <- function(beakr, path = NULL, dir = NULL) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  dir <- ifelse( test = is.null(dir),
                 yes  = getwd(),
                 no   = dir )
  filer <- function(req, res, err) {
    if ( substring(text = req$path, first = nchar(req$path)) == "/" ) {
      req$path <- paste0(req$path, "index.html")
    }

    if ( is.null(path) ) {
      fpath <- paste0(dir, "/", req$path)
    } else {
      ppath <- gsub(paste0(".*", path, "(.*)"), "\\1", req$path)
      fpath <- paste0(dir, "/", ppath)
    }

    bound <- ifelse( test = is.null(path),
                     yes  = TRUE,
                     no   = substr(req$path, 2, nchar(path) + 1) == path )

    if ( file.exists(fpath) & bound ) {
      mime_type <- mime::guess_type(fpath)
      res$contentType(mime_type)
      data <- readBin( con  = fpath,
                       what = "raw",
                       n    = file.info(fpath)$size )
      if( grepl("image|octect|pdf", mime_type) ) { # Assumptions...
        return(data)
      } else {
        return(rawToChar(data))
      }
    } else {
      res$setStatus(404L)
      return(NULL)
    }
  }
  GET(beakr = beakr, path = NULL, filer)
  return(beakr)
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
    beakr <- invisible(Beakr$new())
  }
  lapply(
    X = list(...),
    FUN = function(middleware_FUN) {
      routeMiddleware( beakr  = beakr,
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
#' @param with_methods tbd
#' @param with_origin tbd
#' @param with_headers tbd
#' @param with_credentials tbd
#' @param max_age tbd
#' @param expose_headers tbd
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
    beakr <- invisible(Beakr$new())
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

  routeMiddleware(beakr = beakr, FUN = FUN, path = path, method = NULL)

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
#'
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

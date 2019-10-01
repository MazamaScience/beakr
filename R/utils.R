#' Start a new beakr instance
#'
#' Create a \code{Beakr} instance object by calling the top-level
#' \code{beakr()} function.
#'
#' @usage beakr()
#' @export
beakr <- function() {
  Beakr$new()
}

#' Listen for connections
#'
#' Binds and listens for connections on the specified host and port.
#'
#' @details
#' \code{listen} binds the port and listens for connections on a thread.
#' The thread handles the I/O, and when it receives a HTTP request, it
#' will schedule a call to the user-defined middleware and handle the
#' request.
#'
#' If the daemon boolean value is set to \code{TRUE}, \code{listen} binds the
#' specified port and listens for connections on an thread running in the
#' background.
#'
#' See the \code{httpuv} package for more information.
#'
#' @param beakr a beaker instance.
#' @param host a string that is a valid IPv4 or IPv6 address to listen on.
#' @param port a number or integer that indicates the port to listen on.
#' @param daemonized run the instance in the background.
#' @param verbose boolean, debugging.
#'
#' @usage listen(beakr, host, port, daemonized
#' @export
#'
#' @examples
#' bk <- beakr() %>% listen(port = 1234, daemonized = TRUE)
listen <-function( beakr, host = "127.0.0.1", port = 8080,
                   daemonized = FALSE, verbose = FALSE ) {
  options("beakr.verbose" = verbose)
  message(paste0("Serving beakr instance at http://", host, ":", port))
  beakr$start(host, port, daemonized)
  return(beakr)
}

#' Create a new Error
#'
#' Used to handle errors.
#'
#' @export
#' @usage newError()
#'
# Necessary?
newError <- function() {
  Error$new()
}

#' Stop the beakr instance
#'
#' Stops an active instance when given a beakr object. This closes all open
#' connections for the instance and unbinds the port.
#'
#' @param beakr a beakr instance.
#'
#' @export
#' @example
#' bk <- beakr()
#' listen(bk, daemonized = TRUE)
#' kill(bk)
kill <- function(beakr) {
  httpuv::stopServer(beakr$serverObject)
  cat("Stopped ")
  beakr$print()
}

#' Stop all instances
#'
#' Stops all instances that have been activated by \code{\link{listen}} in the
#' session.
#'
#' @return
#' @export
#' @usage killall()
#' @seealso \code{\link{kill}} and \code{\link{listen}}
killall <- function() {
  httpuv::stopAllServers()
  cat("Stopped All Instances\n")
}

#' List active instances
#'
#' A list containing the current sessions active instances.
#'
#' @return a list of active instances
#' @export
#'
#' @examples
#' bb <- listen(beakr = beakr(), daemonized = TRUE)
#' active()
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

#' Instance Error Handling
#'
#' An error handling middleware for beakr instances.
#'
#' @details
#' \code{beakr} comes with a default error handler. This default error-handling
#' middleware function is added at the end of the middleware function stack.
#' The Errors are handled via JSON output.
#'
#' @param beakr
#' @param path
#'
#' @usage errorHandler(beakr, path)
#' @export
#'
#' @examples
#' # Create an instance and add the error handler last.
#' beakr() %>%
#'   use("/", decorate(function(n) { paste("Hi, ", n) })) %>%
#'   errorHandler() %>%
#'   listen()
#' # In shell
#' # $ curl http://127.0.0.1:8080/
#' # > {"status":"Page not found.","status_code":404}
#' # An erorr was thrown because the parameter "n" is not provided and
#' # required by the middleware.
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

#' Initialize process of test req
#'
#' @param beakr the beakr instance
#' @param test_request the TestRequest instance
#'
#' @export
processTestRequest <- function(beakr, test_request) {
  beakr$route$invoke(test_request)
}

#' Title
#'
#' @param beakr
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param beakr
#' @param path
#' @param root
#'
#' @return
#' @export
#'
#' @examples
static <- function(beakr, path = NULL, root = NULL) {
  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }
  root <- ifelse( test = is.null(root),
                  yes  = getwd(),
                  no   = root )
  filer <- function(req, res, err) {
    if ( substring(text = req$path, first = nchar(req$path)) == "/" ) {
      req$path <- paste0(req$path, "index.html")
    }

    if ( is.null(path) ) {
      fpath <- paste0(root, "/", req$path)
    } else {
      ppath <- gsub(paste0(".*", path, "(.*)"), "\\1", req$path)
      fpath <- paste0(root, "/", ppath)
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
  beakr::get(beakr = beakr, path = NULL, filer)
  return(beakr)
}

#' Title
#'
#' @param beakr
#' @param path
#' @param method
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param beakr
#' @param path
#' @param with_methods
#' @param with_origin
#' @param with_headers
#' @param with_credentials
#' @param max_age
#' @param expose_headers
#'
#' @return
#' @export
#'
#' @examples
cors <-
  function(
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

#' Title
#'
#' @param beakr
#' @param include
#' @param file
#'
#' @return
#' @export
#'
#' @examples
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

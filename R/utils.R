# ----- Pipeline functions -----------------------------------------------------

#' @export
#' @title Create a new Beakr instance
#'
#' @param name Optional name assigned to the \code{Beakr} object.
#'
#' @description Create a \code{Beakr} instance by calling the top-level
#' \code{newBeakr()} function. If \code{name} is not supplied, a random name
#' will be assigned.
#'
#' This \code{Beakr} instance will then begin a pipeline of separate middleware
#' steps for routing, serving files and handling errors. The pipeline will
#' end with the \code{listen()} function.
#'
#' @return A new and empty \code{Beakr} instance.
#'
#' @examples
#' \donttest{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#'
#' # Create a pipeline of hanldlers
#' beakr %>%
#'   httpGET(path = "/route_A", middleware_A) %>%
#'   httpGET(path = "/route_B", middleware_B) %>%
#'   handleErrors() %>%
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/route_A
#' # * http://127.0.0.1:25118/route_B
#' # ------------------------------------------------------------
#'
#' # Kill the beakr instance
#' kill(beakr)
#' }
newBeakr <- function(name = NULL) {

  beakr <- Beakr$new()

  if ( !is.null(name) )
    beakr$name <- name

  return(beakr)

}


#' @export
#' @title Serve static files
#'
#' @description Binds to GET requests that aren't handled by specified paths.
#' Should support all filetypes; returns image and octet-stream types as a raw
#' string.
#'
#' @details Serve static files from the host machine. Currently supports images,
#' PDF, JSON, HTML, and raw text. The static file will be served on the \code{path}
#' specified in addition to the file name and extension. For example, an image
#' \code{example_dir/path_to_pictures/pic.png} will be served on the URL extension
#' \code{/path/pic.png}.
#'
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param path String representing a path to a directory from which static
#' files are served.
#' @param file a string representing the path to the file that is to be served.
#'
#' @return A `beakr` App object with added middleware.
#'
#' @examples
#' \dontrun{
#' path_to_file <- "example_dir/path_to_pictures/pic.png"
#' newBeakr() %>%
#'   static(path = 'readme/', file = path_to_file) %>%
#'   listen()
#' }

serveStaticFiles <- function(beakr, path = NULL, file = NULL) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

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

  return( httpGET(beakr = beakr, path = NULL, serve_file) )

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
#' @return A \code{Beakr} object with added middleware.

handleErrors <- function(beakr, path = NULL) {

  # TODO:  Could support FUN = NULL in function signature so other error
  # TODO:  functions could be supplied.

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = .jsonError,
      path = path,
      method = NULL,
      websocket = FALSE
    )

  return(beakr)

}

#' @export
#' @title Listen for connections on a Beakr instance
#'
#' @description Binds and listens for connections at the specified host and port.
#'
#' @details
#' \code{listen()} binds the specified host and port and listens for connections
#' on a thread. The thread handles incoming requests. when it receives an HTTP
#' request, it will schedule a call to the user-defined middleware and handle the
#' request.
#'
#' If \code{daemon = TRUE}, \code{listen()} binds the specified port and listens
#' for connections on a thread running in the background.
#'
#' See the \code{httpuv} package for more details.
#'
#' @note The default port number 25118 was arrived at using:
#' \preformatted{
#' > match(c("b","e","a","k","r"), letters) %% 10
#' [1] 2 5 1 1 8
#' }
#'
#' @param beakr a beakr instance.
#' @param host a string that is a valid IPv4 or IPv6 address to listen on.
#' Defaults to the local host ("127.0.0.1").
#' @param port a number or integer that indicates the port to listen on. Default
#' is a port opened on 25118.
#' @param daemon run the instance in the background, the default is FALSE.
#' @param verbose Logical specifying whether to print out details of the
#' \code{Beakr} object now running.
#'
#' @return A \code{Beakr} object.
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
  port = 25118,
  daemon = FALSE,
  verbose = TRUE
) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  beakr$start(host, port, daemon)

  if ( verbose )
    beakr$print()

  return(invisible(beakr))

}

# ----- Other functions --------------------------------------------------------

#' @export
#' @title Stop a beakr instance
#'
#' @description Stops an active \code{Beakr} instance, closing
#' all open connections and unbinding the port.
#'
#' @param beakr \code{Beakr} instance.
#' @param verbose Logical specifying whether to print out details of the
#' \code{Beakr} instance just stopped.
#'
#' @return None
#'
#' @examples
#' beakr <- newBeakr()
#' beakr %>%
#'   listen(daemon = TRUE, verbose = TRUE)
#' kill(beakr, verbose = TRUE)
kill <- function(beakr, verbose = TRUE) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  httpuv::stopServer(beakr$server)

  if ( verbose ) {
    cat("Stopped ")
    beakr$print()
  }

}

#' @export
#' @title Stop all servers
#'
#' @description Stops all \code{Beakr} instances (and any other servers created
#' with the \code{httpuv} package). This convenience function is a wrapper for
#' \code{httpuv::stopAllServers()} and is included to encourage experimentation
#' so that users who create multiple \code{Beakr} instances can quickly stop
#' them all.
#'
#' @return None
#'
#' @examples
#' beakr1 <- newBeakr()
#' beakr2 <- newBeakr()
#' beakr1 %>% listen(daemon = TRUE, port = 1234, verbose = TRUE)
#' beakr2 %>% listen(daemon = TRUE, port = 4321, verbose = TRUE)
#' length(httpuv::listServers())
#' httpuv_stopAllServers()
#' length(httpuv::listServers())
httpuv_stopAllServers <- function() {

  httpuv::stopAllServers()

}

# ----- Pipeline functions -----------------------------------------------------

#' @export
#' @title Create a new Beakr instance
#'
#' @param name Optional name assigned to the \code{Beakr} instance.
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
#'   httpGET(path = "/route_A", function(res, req, err) {
#'     print("This is route 'A'.")
#'   }) %>%
#'   httpGET(path = "/route_B", function(res, req, err) {
#'     print("This is route 'B'.")
#'   }) %>%
#'   handleErrors() %>%
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/route_A
#' # * http://127.0.0.1:25118/route_B
#' # ------------------------------------------------------------
#'
#' # Stop the beakr instance server
#' stopServer(beakr)
#' }
newBeakr <- function(
  name = NULL
) {

  beakr <- Beakr$new()

  if ( !is.null(name) )
    beakr$name <- name

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
#' See the \pkg{httpuv} package for more details.
#'
#' @note The default port number 25118 was generated using:
#' \preformatted{
#' > match(c("b","e","a","k","r"), letters) \%\% 10
#' [1] 2 5 1 1 8
#' }
#'
#' @param beakr \code{Beakr} instance.
#' @param host String that is a valid IPv4 or IPv6 address to listen on.
#' Defaults to the local host ("127.0.0.1").
#' @param port Number or integer that indicates the port to listen on. Default
#' is a port opened on 25118.
#' @param daemon Logical specifying whether the server should be run in the
#' background.
#' @param verbose Logical specifying whether to print out details of the
#' \code{Beakr} instance now running. This should only be used when running
#' a beaker app interactively, not in production.
#'
#' @return A \code{Beakr} instance with an active server.
#'
#' @examples
#' # Run in the background
#' beakr <- newBeakr()
#' beakr %>%
#'   httpGET("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen(daemon = TRUE)
#'
#' # Stop the server
#' stopServer(beakr)

listen <- function(
  beakr = NULL,
  host = "127.0.0.1",
  port = 25118,
  daemon = FALSE,
  verbose = FALSE
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
#' @title Decorate a function for use in a web service
#'
#' @description The \code{decorate()} function can be used to prepare a function
#' for easy use in a beakr pipeline.
#'
#' Decorating a function associates the specified function and its parameters
#' with \code{req}, \code{res}, and \code{err} objects and assigns a
#' content-type to the response object. This prepares a standard R function to
#' be used in \code{Beakr} instances and accept requests.
#'
#' @param FUN Function to decorate.
#' @param content_type HTTP "content-type" of the function output.
#' (\emph{e.g.} "text/plain", "text/html" or other mime type)
#' @param strict Boolean, requiring strict parameter matching.
#'
#' @return A \emph{decorated} middleware function.
#'
#' @examples
#' \donttest{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#' # Create simple hello and goodbye function
#' hello <- function(name) { paste0("Hello, ", name, "!") }
#' goodbye <- function(text = "Adios") { paste0(text, ", dear friend.") }
#'
#' # Create a web service from these functions
#' beakr %>%
#'   httpGET(path = "/hello", decorate(hello)) %>%
#'   httpGET(path = "/goodbye", decorate(goodbye)) %>%
#'   handleErrors() %>%
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/hello?name=Honeydew
#' # * http://127.0.0.1:25118/goodbye
#' # ------------------------------------------------------------
#'
#' # Stop the beakr instance server
#' stopServer(beakr)
#' }

decorate <- function(
  FUN,
  content_type = "text/html",
  strict = FALSE
) {

  # Get the parameters the function allows
  args <- names(formals(FUN))

  # Create a decorated function
  decoratedFUN <- function(req, res, err) {
    res$setContentType(content_type)
    #Inspect passed in parameters
    parameters <- utils::modifyList(req$parameters, req$headers)
    parameters$req <- req
    parameters$res <- res
    parameters$err <- err

    # Check that all arguments are present
    if ( strict ) {
      present <- sapply( X = args,
                         FUN = function(x) x %in% names(parameters) )
      # Throw an err if missing requested params
      if ( !all(present) ) {
        err$set(paste0( "Need requested arguments:\n",
                        paste(args[!present], collapse = ", ") ))
        return(NULL)
      }
    }

    # Drop unrequested params from query params
    if ( !("..." %in% args) ) {
      parameters <- parameters[names(parameters) %in% args]
    }

    # Execute the passed in function with the param
    return(do.call(what = FUN, args = parameters))
  }

  # Return decorated function
  return(decoratedFUN)

}


#' @export
#' @title Stop a beakr instance server
#'
#' @description Stops the server associated with a \code{Beakr} instance,
#' closing all open connections and unbinding the port.
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
#' stopServer(beakr, verbose = TRUE)

stopServer <- function(
  beakr = NULL,
  verbose = FALSE
) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  httpuv::stopServer(beakr$server)

  if ( verbose ) {
    cat("Stopped ")
    beakr$print()
  }

}


# ----- Imports from other packages --------------------------------------------

#' @export
#' @importFrom httpuv listServers
#' @name listServers
#'
#' @title List all servers
#'
#' @description Lists all \code{Beakr} servers currently running (and any other
#' servers created with the \pkg{httpuv} package). This function is included to
#' encourage experimentation so that users who create multiple \code{Beakr}
#' instances can quickly find and stop them all.
#'
#' See \code{httpuv::\link[httpuv:listServers]{listServers}} for details.
#'
#' @usage listServers()
#'
#' @return None
#'
#' @examples
#' beakr1 <- newBeakr()
#' beakr2 <- newBeakr()
#' beakr1 %>% listen(daemon = TRUE, port = 1234, verbose = TRUE)
#' beakr2 %>% listen(daemon = TRUE, port = 4321, verbose = TRUE)
#' length(listServers())
#' stopAllServers()
#' length(listServers())
NULL


#' @export
#' @importFrom httpuv stopAllServers
#' @name stopAllServers
#'
#' @title Stop all servers
#'
#' @description Stops all \code{Beakr} servers currently running (and any other
#' servers created with the \pkg{httpuv} package). This function is included to
#' encourage experimentation so that users who create multiple \code{Beakr}
#' instances can quickly find and stop them all.
#'
#' See \code{httpuv::\link[httpuv:stopAllServers]{stopAllServers}} for details.
#'
#' @usage stopAllServers()
#'
#' @return None
#'
#' @examples
#' beakr1 <- newBeakr()
#' beakr2 <- newBeakr()
#' beakr1 %>% listen(daemon = TRUE, port = 1234, verbose = TRUE)
#' beakr2 %>% listen(daemon = TRUE, port = 4321, verbose = TRUE)
#' length(listServers())
#' stopAllServers()
#' length(listServers())
NULL


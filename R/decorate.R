#' @export
#' @title Decorate a function for use in a web service
#'
#' Convenience function to decorate an existing function for easy use in
#' \code{Beakr} instances.
#'
#' @details
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
#'
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
      if( !all(present) ) {
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

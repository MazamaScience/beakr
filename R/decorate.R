#' Decorate a function
#'
#' Convenience function to decorate an existing function to interface with beakr
#' instances.
#'
#' @details
#' Decorating a function associates the specified function and its parameters
#' with \code{req}, \code{res}, and \code{err} objects and assigns a
#' content-type to the response object. This prepares a standard R function to
#' affect beakr instances and accept requests.
#'
#' @param FUN The function to decorate.
#' @param content_type The type of content to set response as.
#' @param strict Boolean, requiring strict parameter matching.
#'
#' @usage decorate(FUN, content_type, strict = FALSE)
#' @return A `decorated` middleware function.
#' @export
decorate <-
  function(FUN, content_type = "text/html", strict = FALSE) {
    # Get the parameters the function allows
    args <- names(formals(FUN))

    # Create a decorated function
    decorated <- function(req, res, err) {
      res$setContentType(content_type)
      #Inspect passed in parameters
      parameters <- utils::modifyList(req$parameters, req$headers)
      parameters$req <- req
      parameters$res <- res
      parameters$err <- err

      # Check arguments are all present
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
    return(decorated)
  }

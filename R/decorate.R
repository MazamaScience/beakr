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
#' @param FUN the function to decorate.
#' @param content_type the type of content to set response as.
#' @param strict boolean, requiring strict parameter matching.
#'
#' @usage decorate(FUN, content_type, strict = FALSE)
#' @export
decorate <-
  function(FUN, content_type = "text/html", strict = FALSE) {
    # Get the parameters the function allows
    args <- names(formals(FUN))

    # Create a decorated function
    decorated <- function(req, res, err) {
      res$contentType(content_type)
      #Inspect passed in parameters
      params <- utils::modifyList(req$parameters, req$headers)
      params$req <- req
      params$res <- res
      params$err <- err

      # Check arguments are all present
      if ( strict ) {
        present <- sapply( X = args,
                           FUN = function(x) x %in% names(params) )
        # Throw an err if missing requested params
        if( !all(present) ) {
          err$set(paste0( "Need requested arguments:\n",
                             paste(args[!present], collapse = ", ") ))
          return(NULL)
        }
      }

      # Drop unrequested params from query params
      if ( !("..." %in% args) ) {
        params <- params[names(params) %in% args]
      }

      # Execute the passed in function with the params
      return(do.call(what = FUN, args = params))
    }

    # Return decorated function
    return(decorated)
  }

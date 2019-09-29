#' Title
#'
#' @param func
#' @param content_type
#' @param strict
#'
#' @return
#' @export
#'
#' @examples
decorate <-
  function(FUN, type = "text/html", strict = FALSE) {
    # Get the parameters the function allows
    args <- names(formals(FUN))

    # Create a decorated function
    decorated <- function(req, res, err) {
      res$contentType(type)
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

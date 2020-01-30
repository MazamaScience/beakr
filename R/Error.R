#' @export
#' @title Error class
#'
#' @description
#' A \code{Error} class object represents the state and handling of instance
#' or middleware errors.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Fields:
#'
#' \describe{
#'   \item{\code{errors}}{
#'   Returns a list of errors, if any.
#'   }
#'   \item{\code{occured}}{
#'   Returns TRUE if any error has occurred, FALSE otherwise.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{set(err)}}{
#'   Sets an error.
#'   }
#' }
#'
#' @seealso \code{\link{handleErrors}} and \code{\link{Middleware}}

Error <-
  R6::R6Class(
    classname = "Error",
    public = list(
      errors = c(),
      set = function(err) {
        self$errors <- c(self$errors, as.character(err))
      }
    ),
    active = list(
      occurred = function() {
        return(length(self$errors) > 0)
      }
    )
  )

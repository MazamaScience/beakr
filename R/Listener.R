#' @export
#' @title Listener class
#'
#' @description
#' The \code{Listener} class listens provides a simple, programmatically
#' controlled HTTP protocol listener.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Fields:
#'
#' \describe{
#'   \item{\code{FUN}}{
#'   Returns function response.
#'   }
#'   \item{\code{event}}{
#'   Returns event type.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{initialize(FUN, event)}}{
#'   Sets instance object function and event state.
#'   }
#' }
#'
#' @seealso \code{\link{Router}} and \code{\link{Error}}
#' @keywords internal
Listener <-
  R6::R6Class(
    classname = "Listener",
    public = list(
      FUN = NULL,
      event = NULL,
      initialize = function(event, FUN, ...) {
        self$FUN = FUN
        self$event = event
      }
    )
  )

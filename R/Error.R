#' Error Class
#'
#' @description
#' An `Error` object tracks and reports errors that occur during request
#' handling or middleware execution in a [`Router`].
#' Errors are collected in a list, and the `occurred` active binding indicates
#' whether any errors have been set.
#'
#' @docType class
#' @name Error
#' @export
#'
#' @format An [`R6::R6Class`] generator for `Error` objects.
#'
#' @seealso [Middleware], [Router], [handleErrors]

Error <-
  R6::R6Class(
    classname = "Error",
    public = list(
      #' @field errors Character vector of recorded error messages.
      errors = c(),

      #' @description
      #' Append an error message to `errors`.
      #' @param err Error message (coerced to character).
      set = function(err) {
        self$errors <- c(self$errors, as.character(err))
      }
    ),
    active = list(
      #' @field occurred Logical; `TRUE` if any errors have been recorded.
      occurred = function() {
        length(self$errors) > 0
      }
    )
  )


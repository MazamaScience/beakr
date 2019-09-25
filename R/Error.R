#' Error Response Class

Error <-
  R6::R6Class(
    public = list(
      errors = c(),
      set = function(error) {
        self$errors <- c(self$errors, as.character(error))
      }
    ),
    active = list(
      occured = function() {
        return(length(self$errors) > 0)
      }
    )
  )

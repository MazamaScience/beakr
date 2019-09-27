#' Error Response Class
Error <-
  R6::R6Class(
    classname = "Error",
    public = list(
      errors = c(),
      # Set the errors if occurs
      set = function(error) {
        self$errors <- c(self$errors, as.character(error))
      }
    ),
    # Show the occured errors, if any
    active = list(
      occured = function() {
        return(length(self$errors) > 0)
      }
    )
  )

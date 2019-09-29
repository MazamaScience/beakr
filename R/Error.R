#' Error Response Class
Error <-
  R6::R6Class(
    classname = "Error",
    public = list(
      errors = c(),
      # Set the errors if occurs
      set = function(err) {
        self$errors <- c(self$errors, as.character(err))
      }
    ),
    # Show the occured errors, if any
    active = list(
      occurred = function() {
        return(length(self$errors) > 0)
      }
    )
  )

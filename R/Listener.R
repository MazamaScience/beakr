#' Listener class

Listener <-
  R6::R6Class(
    classname = "Listener",
    public = list(
      FUN = NULL,
      event = NULL,
      initialize = function(FUN, event) {
        self$FUN = FUN
        self$event = event
      }
    )
  )

#' Listener Class
#'
#' @description
#' A `Listener` object represents an event handler within a [`Router`].
#' Each listener pairs an `event` type (e.g., `"start"`, `"error"`, `"finish"`)
#' with a function `FUN` to execute when that event is triggered.
#'
#' @docType class
#' @name Listener
#' @export
#'
#' @format An [`R6::R6Class`] generator for `Listener` objects.
#'
#' @seealso [Router], [Error]

Listener <-
  R6::R6Class(
    classname = "Listener",
    public = list(
      #' @field FUN Handler function to execute when `event` is triggered.
      FUN = NULL,
      #' @field event Event name (e.g., `"start"`, `"error"`, `"finish"`).
      event = NULL,

      #' @description
      #' Construct a listener by setting its `event` and handler `FUN`.
      #' @param event Event name string.
      #' @param FUN Function to call when the event occurs.
      #' @param ... Ignored; accepted for flexibility.
      initialize = function(event, FUN, ...) {
        self$FUN <- FUN
        self$event <- event
      }
    )
  )


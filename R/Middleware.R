#' Middleware Class
Middleware <-
  R6::R6Class(
    classname = "Middleware",
    public = list(
      path = NULL,
      FUN = NULL,
      method = NULL,
      protocol = NULL,
      # Initialize
      initialize = function(FUN, path, method, websocket) {
        self$FUN = FUN
        self$path = path
        self$method = method
        self$protocol = ifelse(websocket, "websocket", "http")
      }
    )
  )

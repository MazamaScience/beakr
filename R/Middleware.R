#' Middleware Class
#'
#' @description
#' A `Middleware` object wraps a handler function with associated metadata
#' (`path`, `method`, `protocol`). Middleware functions have access to the
#' request (`req`), response (`res`), and error (`err`) objects during the
#' request–response cycle via the [`Router`].
#'
#' @docType class
#' @name Middleware
#' @export
#'
#' @format An [`R6::R6Class`] generator for `Middleware` objects.
#'
#' @seealso [Router], [Request], [Response], [Error]

Middleware <-
  R6::R6Class(
    classname = "Middleware",
    public = list(
      #' @field path Path this middleware matches, or `NULL` for all paths.
      path = NULL,
      #' @field FUN Handler function executed when matched.
      FUN = NULL,
      #' @field method HTTP method to match (e.g., `"GET"`), or `NULL` for any.
      method = NULL,
      #' @field protocol Protocol string: `"http"` or `"websocket"`.
      protocol = NULL,

      #' @description
      #' Initialize middleware with handler, path, method, and protocol selection.
      #' @param FUN Handler function (e.g., `(req, res, err)` for HTTP).
      #' @param path Route path to match, or `NULL` for all.
      #' @param method HTTP method to match, or `NULL` for any.
      #' @param websocket If `TRUE`, set `protocol = "websocket"`; otherwise `"http"`.
      initialize = function(FUN, path, method, websocket) {
        self$FUN <- FUN
        self$path <- path
        self$method <- method
        self$protocol <- if (websocket) "websocket" else "http"
      }
    )
  )

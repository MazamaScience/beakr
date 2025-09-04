#' Response Class
#'
#' @description
#' A `Response` object represents the HTTP response that a `Beakr` app sends
#' when handling a request. By convention, the response object is named
#' `res` (with the corresponding request named `req`).
#'
#' @docType class
#' @name Response
#' @export
#' @importFrom base64enc base64encode
#'
#' @format An [`R6::R6Class`] generator for `Response` objects.
#'
#' @seealso [Router], [Request], [Error]
NULL

Response <-
  R6::R6Class(
    classname = "Response",
    public = list(
      #' @field headers A named list of HTTP response headers.
      #'   Defaults to `list("Content-Type" = "text/html")`.
      headers = list("Content-Type" = "text/html"),

      #' @field status An integer HTTP status code. Defaults to `200L`.
      status = 200L,

      #' @field body The response body. May be `NULL`, character, raw, JSON, or base64.
      body = NULL,

      #' @description
      #' Set a header key-value pair (e.g., `"Content-Type" = "text/html"`).
      #' @param key Header name.
      #' @param value Header value.
      setHeader = function(key, value) {
        self$headers[[key]] <- value
      },

      #' @description
      #' Set the response `Content-Type`.
      #' @param type MIME type string.
      setContentType = function(type) {
        self$headers[["Content-Type"]] <- type
      },

      #' @description
      #' Set the HTTP status code.
      #' @param status Integer HTTP status code.
      setStatus = function(status) {
        self$status <- status
      },

      #' @description
      #' Set the response body, respecting the current `Content-Type`.
      #' @param body Body content, type depends on `Content-Type`.
      setBody = function(body) {
        # Hack to avoid numeric res failure
        if ( self$headers[["Content-Type"]] == "text/html" ) {
          self$text(body)
        } else {
          self$body <- body
        }
      },

      #' @description
      #' Redirect the client by setting status 302 and `Location` header.
      #' @param url The URL to redirect to.
      redirect = function(url) {
        self$status <- 302L
        self$setHeader("Location", url)
      },

      #' @description
      #' Convert `txt` to JSON and set content type to `"application/json"`.
      #' @param txt Content to convert to JSON.
      #' @param auto_unbox Logical; whether to simplify length-1 vectors.
      json = function(txt, auto_unbox = TRUE) {
        self$body <- jsonlite::toJSON(txt, auto_unbox = auto_unbox)
        self$setContentType("application/json")
      },

      #' @description
      #' Set the response body as plain text and content type `"text/html"`.
      #' @param txt Content to include as plain text.
      text = function(txt) {
        self$body <- as.character(txt)
        self$setContentType("text/html")
      },

      #' @description
      #' Return a structured response depending on `protocol`.
      #' @param protocol `"http"` or `"websocket"`.
      structured = function(protocol) {
        switch(
          EXPR = protocol,
          "http" = list(
            status  = self$status,
            headers = self$headers,
            body    = self$body
          ),
          "websocket" = self$body
        )
      },

      #' @description
      #' Render a plot to PNG (optionally base64-encode) and set as response body.
      #' @param plot_object A plot object to render.
      #' @param base64 Logical; if `TRUE`, encode image as base64.
      #' @param ... Passed to [graphics::png()].
      plot = function(plot_object, base64 = TRUE, ...) {
        # Create the plot
        plot_file <- tempfile(pattern = "beakr")
        png(filename = plot_file, ...)
        print(plot_object)
        dev.off()

        # Read plot image
        file_connection <- file(description = plot_file, open = "rb")
        binary_image <- readBin(con = file_connection, what = "raw", n = 1e6)

        if ( base64 ) {
          self$setContentType("application/base64")
          self$body <- base64enc::base64encode(binary_image)
        } else {
          self$setContentType("image/png")
          self$body <- binary_image
        }
        close(con = file_connection)
      }
    )
  )

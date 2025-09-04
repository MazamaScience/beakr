#' Request Class
#'
#' @description
#' A `Request` object represents an incoming HTTP request. It stores query
#' string, parameters, body, headers, method, and protocol information.
#' By convention, the request object is named `req` (with the corresponding
#' response named `res`).
#'
#' @docType class
#' @name Request
#' @export
#'
#' @format An [`R6::R6Class`] generator for `Request` objects.
#'
#' @seealso [Response]

Request <-
  R6::R6Class(
    classname = "Request",
    public = list(
      #' @field parameters Named list of route parameters.
      parameters = list(),
      #' @field headers Named list of request headers.
      headers = list(),
      #' @field path The request path.
      path = NULL,
      #' @field method HTTP method (e.g., `"GET"`, `"POST"`).
      method = NULL,
      #' @field raw The raw request object as received.
      raw = NULL,
      #' @field type Content type (e.g., `"text/html"`, `"application/json"`).
      type = NULL,
      #' @field body Raw request body as a single string.
      body = NULL,
      #' @field protocol Protocol string (`"http"` or `"websocket"`).
      protocol = "http",

      #' @description
      #' Attach a parameter key-value to `parameters`.
      #' @param key Parameter name.
      #' @param value Parameter value.
      attach = function(key, value) {
        self$parameters[[key]] <- value
      },

      #' @description
      #' Get the value of a request header.
      #' @param key Header name (case-insensitive).
      getHeader = function(key) {
        self$headers[[key]]
      },

      #' @description
      #' Set or overwrite a request header.
      #' @param key Header name.
      #' @param value Header value.
      setHeader = function(key, value) {
        self$headers[[key]] <- value
      },

      #' @description
      #' Merge a named list of parameters into `parameters`.
      #' @param named_list Named list of key-value pairs.
      addParameters = function(named_list) {
        self$parameters <- c(self$parameters, named_list)
      },

      #' @description
      #' Parse fields from the raw request and populate the object.
      #' @param req Raw request object.
      initialize = function(req) {
        self$raw <- req
        self$path <- req$PATH_INFO
        self$method <- toupper(req$REQUEST_METHOD)

        if ( length(req$CONTENT_TYPE) > 0 ) {
          self$type <- tolower(req$CONTENT_TYPE)
        }

        # rook.input from test _request.R
        self$body <- paste0(req$rook.input$read_lines(), collapse = "")

        self$parameters <- .parseParameters(
          req   = req,
          body  = self$body,
          query = req$QUERY_STRING,
          type  = self$type
        )

        header_keys <- Filter(function(x) grepl("^HTTP", x), names(req))
        self$headers <- as.list(req)[header_keys]
        names(self$headers) <- Map(function(x) tolower(gsub("^HTTP_", "", x)),
                                   names(self$headers))

        if ( any(tolower(self$getHeader("upgrade")) == "websocket") ) {
          self$protocol <- "websocket"
        }
      }
    )
  )

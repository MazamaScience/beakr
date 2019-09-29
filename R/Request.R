#' Request Class
#'
#' The \code{Request} object represents the HTTP request and has properties for
#' the request query string, parameters, body, HTTP headers, and so on.
#' In this documentation and by convention, the object is always referred to as
#' \code{req} (and the HTTP response is \code{res}).
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Fields:
#'
#' \describe{
#'   \item{\code{parameters}}{
#'   A list containing properties mapped to the named route parameters.
#'   }
#'   \item{\code{headers}}{
#'   A list of response headers.
#'   }
#'   \item{\code{path}}{
#'   Contains the path part of the request URL.
#'   }
#'   \item{\code{method}}{
#'   Contains a string corresponding to the HTTP method of the request:
#'   GET, POST, PUT, and so on.
#'   }
#'   \item{\code{raw}}{
#'   Returns the raw request (\code{req}) object.
#'   }
#'   \item{\code{type}}{
#'   Contains the body content-type, i.e. "text/html" or "application/json".
#'   }
#'   \item{\code{body}}{
#'   Contains the data submitted in the request body.
#'   }
#'   \item{\code{protocol}}{
#'   Contains the request protocol string.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#' \item{\code{attach(key, value)}}{
#'   Returns a key-value.
#'   }
#'   \item{\code{getHeader(key)}}{
#'   Returns the key element of the \code{headers} list.
#'   }
#'   \item{\code{setHeader(key, value)}}{
#'   Attaches a header to \code{headers} list.
#'   }
#'   \item{\code{addParameters(named_list)}}{
#'   Adds parameters to the named key-value \code{parameters} list.
#'   }
#'   \item{\code{intialize(req)}}{
#'   Creates a new \code{Request} object by parsing and extracting features of
#'   \code{req} input and populating the object fields.
#'   }
#' }
#'
#' @seealso \code{\link{Response}} and \code{\link{TestRequest}
#' @keywords internal
Request <-
  R6::R6Class(
    classname = "Request",
    public = list(
      parameters = list(),
      headers = list(),
      path = NULL,
      method = NULL,
      raw = NULL,
      type = NULL,
      body = NULL,
      protocol = "http",

      # Necessary?
      # attach = function(key, value) {
      #   self$parameters[[key]] <- value
      # },

      getHeader = function(key) {
        self$headers[[key]]
      },

      setHeader = function(key, value) {
        self$headers[[key]] <- value
      },

      addParameters = function(named_list) {
        self$parameters <- c(self$parameters, named_list)
      },
      # Run on new object creation
      initialize = function(req) {
        self$raw <- req
        self$path <- req$PATH_INFO
        self$method <- toupper(req$REQUEST_METHOD)

        if ( length(req$CONTENT_TYPE) > 0 ) {
          self$type <- tolower(req$CONTENT_TYPE)
        }

        # rook.input from test _request.R
        self$body <- paste0(req$rook.input$read_lines(), collapse = "")

        # Parse the parameters passed, helper func in 'utils.R'
        self$parameters <- parseParameters( req     = req,
                                            body    = self$body,
                                            query   = req$QUERY_STRING,
                                            type    = self$type )

        header_keys <- Filter(
          f = function(x) { grepl("^HTTP", x) },
          x = names(req)
        )

        self$headers <- as.list(req)[header_keys]
        names(self$headers) <- Map(
          f = function(x) { tolower(gsub("^HTTP", "", x)) },
          names(self$headers)
        )

        if ( any(tolower(self$getHeader("upgrade")) == "websocket") ) {
          self$protocol <- "websocket"
        }
      }

    )
  )

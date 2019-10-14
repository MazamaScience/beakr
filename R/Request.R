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
#'   A list containing properties mapped to the named router parameters.
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
#' @seealso \code{\link{Response}} and \code{\link{TestRequest}}
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
        self$parameters <- .parseParameters( req     = req,
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

#' TestRequest Class
#'
#' The \code{TestRequest} object is for internal testing only.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @seealso \code{\link{Response}} and \code{\link{Request}}
#' @keywords internal
TestRequest <-
  R6::R6Class(
    classname = "TestRequest",
    public = list(
      req = list(
        HTTP_CACHE_CONTROL = "no-cache",
        HTTP_CONNECTION = "keep-alive",
        HTTP_ACCEPT = "*/*",
        HTTP_ACCEPT_LANGUAGE = "nl-NL,nl;q=0.8,en-US;q=0.6,en;q=0.4,de;q=0.2",
        QUERY_STRING = "",
        httpuv.version = list(c(1L, 3L, 3L)),
        SERVER_NAME = "127.0.0.1",
        SCRIPT_NAME = "",
        SERVER_PORT = "8080",
        REMOTE_PORT = "60144",
        PATH_INFO = "/",
        REMOTE_ADDR = "127.0.0.1",
        CONTENT_TYPE = "application/x-www-form-urlencoded",
        rook.url_scheme = "http",
        rook.input = list(read_lines = function() { return("") }),
        HTTP_ACCEPT_ENCODING = "gzip, deflate, sdch",
        HTTP_COOKIE = "",
        REQUEST_METHOD = "GET",
        HTTP_USER_AGENT = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36
                          (KHTML, like Gecko) Chrome/45.0.2454.101 Safari/537.36",
        HTTP_TREADS = "5",
        HTTP_HOST = "127.0.0.1:8080"
      ),
      path = function(path) {
        self$req$PATH_INFO <- path
      },
      method = function(method) {
        self$req$REQUEST_METHOD <- method
      },
      query_string = function(qstring) {
        self$req$QUERY_STRING <- qstring
      },
      body = function(body) {
        self$req$rook.input$read_lines = function() { return(body) }
      },
      set_header = function(key, value, prefix = "HTTP_") {
        self$req[[paste0(prefix, toupper(key))]] <- value
      },
      get_header = function(key, prefix = "HTTP_"){
        self$req[[paste0(prefix, toupper(key))]]
      },
      print = function(...){
        cat("A TestRequest instance\n")
        invisible(self$req)
      }
    )
  )

# ===== Internal Functions =====================================================

#' @keywords internal
#' @title Parse the parameters passed by the req
#'
#' @param req an HTTP req
#' @param body a body text string
#' @param query a url-encoded query string
#' @param type a media mime type (a.k.a. Multipurpose Internet Mail Extensions
#' or MIME type).
.parseParameters <- function(req, body, query, type) {
  parameters <- list()
  parameters <- c(parameters, webutils::parse_query(query))

  if ( is.null(type) ) {
    return(parameters)
  }

  if ( grepl("json", type) && nchar(body) > 0 ) {
    parameters <- c( parameters,
                     jsonlite::fromJSON(body, simplifyDataFrame = FALSE) )
  }

  if ( grepl("multipart", type) ) {
    parameters <- c( parameters,
                     mime::parse_multipart(req) )
  }

  if ( grepl("form-urlencoded", type) ) {
    parameters <- c( parameters,
                     webutils::parse_query(body) )
  }
  return(parameters)
}

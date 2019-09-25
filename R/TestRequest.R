#' Generate request for testing purposes
#'
#' solely used for testing purposes
#'
#' @export

TestRequest <-
  R6::R6Class(
    classname = "TestRequest",
    public = list(
      request = list(
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
        rook.input = list(readLines = function() { return("") }),
        HTTP_ACCEPT_ENCODING = "gzip, deflate, sdch",
        HTTP_COOKIE = "",
        REQUEST_METHOD = "GET",
        HTTP_USER_AGENT = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.101 Safari/537.36",
        HTTP_TREADS = "5",
        HTTP_HOST = "127.0.0.1:8080"
        ),

      path = function(path) {
        self$request$PATH_INFO <- path
      },

      method = function(method) {
        self$request$REQUEST_METHOD <- method
      },

      query_string = function(qstring) {
        self$request$QUERY_STRING <- qstring
      },

      body = function(body) {
        self$request$rook.input$read_lines = function() { return(body) }
      },

      set_header = function(key, value, prefix = "HTTP_") {
        self$request[[paste0(prefix, toupper(key))]] <- value
      },

      get_header = function(key, prefix = "HTTP_"){
        self$request[[paste0(prefix, toupper(key))]]
      },

      print = function(...){
        cat("A TestRequest instance\n")
        invisible(self$request)
      }
    )
  )

#' Initialize process of test request
#'
#' @param beakr the beakr instance
#' @param test_request the TestRequest instance
#'
#' @export
processTestRequest <- function(beakr, test_request) {
  beakr$requestHandler$invoke(test_request)
}

#' Request Object Class
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

      attach = function(key, value) {
        self$parameters[[key]] <- value
      },

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
      initialize = function(request) {
        self$raw <- request
        self$path <- request$PATH_INFO
        self$method <- toupper(request$REQUEST_METHOD)

        if ( length(request$CONTENT_TYPE) > 0 ) {
          self$type <- tolower(request$CONTENT_TYPE)
        }

        # rook_input from test _request.R
        self$body <- paste0(request$rook_input$readLines(), collapse = "")

        # Parse the parameters passed, helper func in 'utils.R'
        self$parametes <- parseParameters( request = request,
                                           body    = self$body,
                                           query   = request$QUERY_STRING,
                                           type    = self$type )

        header_keys <- Filter(
          f = function(x) { grepl("^HTTP", x) },
          x = names(request)
        )

        self$headers <- as.list(request)[header_keys]
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

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
        self$parameters <- parseParameters( req = req,
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

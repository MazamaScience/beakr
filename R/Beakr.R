#' The Beakr class
#'
#' @importFrom magrittr %>%
Beakr <-
  R6::R6Class(
    classname = "Beakr",
    public = list(

      # Initialize server request handling
      requestHandler = NULL,
      # Initialize server object
      serverObject = NULL,
      # Initialize instance
      appDefinition = function(self, request) {
        list(
          # Call a request invoke
          call = function(req) {
            self$requestHandler$invoke(request)
          },
          onWebsocketOpen = function(websocket) {
            websocket$onMessage(function(binary, message) {
              websocket$send(self$requestHandler$invoke(
                request          = websocket$request,
                websocket_msg    = message,
                websocket_binary = binary
              ))
            })
          }
        )
      },
      # Method to add middelware using `RequestHandler.R`
      addCollectedMiddleware = function(collector) {
        self$requestHandler$addMiddleware(
          collector$requestHandler$middleware
        )
      },
      # Initialize the new requestHandler obj using `RequestHandler.R`
      initialize = function() {
        self$requestHandler <- RequestHandler$new()
        # Set Early for testing purposes when serve_it isn't called - Optional?
        options("beakr.verbose" = FALSE)
      },
      # Method for starting/creating http/websocket server
      start = function(host, port) {
        httpuv::startServer( host = host,
                             port = port,
                             app = self$appDefinition )
      },
      # Let the user know what middleware has been loaded in the beakr instance
      print = function(...) {
        cat( "A beakr instance with ",
             length(sefl$requestHandler$middleware),
             " middleware attached\n",
             sep = "" )
        invisible(self)
      }
    )
  )


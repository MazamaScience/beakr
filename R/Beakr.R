#' @docType package
#' @name Beakr
#' @title A Simple Web Framework for R.
#' @description A web framework mimicking the functionality found in python Flask
#' including basic functionality for handling web GET and PUT requests. This
#' package is a ground-up rewrite of the original "jug" package by Bart Smeets.
NULL

#' The Beakr class
#'
Beakr <-
  R6::R6Class(
    classname = "Beakr",
    public = list(
      # Initialize server request handling
      requestHandler = NULL,
      # Initialize server object
      serverObject = NULL,
      # Initialize instance
      appDefinition = function() {
        list(
          # Call a request invoke
          call = function(request) {
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
        self$serverObject <-
          httpuv::startServer( host = host,
                               port = port,
                               app  = self$appDefinition() )

      },
      # Let the user know what middleware has been loaded in the beakr instance
      print = function(...) {
        cat( "Beakr instance:\n",
             length(self$requestHandler$middleware),
             " middleware attached\n",
             sep = "" )
        invisible(self)
      }
    )
  )


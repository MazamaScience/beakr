#' The Jug class
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

    )
  )


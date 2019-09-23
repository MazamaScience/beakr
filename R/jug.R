#
# TODO:  How to properly document R6 classes?
#
Jug <-
  R6::R6Class(
    classname = "Jug",

    # Define public interface
    public = list(

      # TODO:  Explain this field
      request_handler = NULL,

      # TODO:  Explain this field
      daemon_obj = NULL,

      # TODO:  Method to ???
      app_definition = function() {
        list(
          call = function(req) {
            self$request_handler$invoke(req)
          },
          onWSOpen = function(ws) {
            ws$onMessage(function(binary, message) {
              ws$send(self$request_handler$invoke(ws$request,
                                                  ws_message = message,
                                                  ws_binary = binary)
              )
            })
          }
        )
      },

      # TODO:  Method to ???
      add_collected_middelware = function(collector) {
        self$request_handler$add_middleware(collector$request_handler$middlewares)
      },

      # TODO:  Method to ???
      initialize = function() {
        self$request_handler = RequestHandler$new()
        options("jug.verbose" = FALSE) # set early for testing purposes were serve_it isn't called
      },

      # Start an HTTP/WebSocket server
      start = function(host, port, daemonized) {
        if ( daemonized ) {
          self$daemon_obj<-
            httpuv::startServer(host, port, self$app_definition())
        } else {
          httpuv::runServer(host, port, self$app_definition())
        }

      },

      # TODO:  Method to ???
      print = function(...) {
        cat("A Jug instance with ",
            length(self$request_handler$middlewares),
            " middlewares attached\n", sep = "")
        invisible(self)
      }

    ) # END public interface definition

  ) # END R6 class definition




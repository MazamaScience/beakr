# Request Handler object class definition

RequestHandler <-
  R6::R6Class(
    classname = "RequestHandler",
    public = list(
      # Define middleware and listeners
      middleware = c(),
      listeners = c(),

      # Add passed in middleware
      addMiddleware = function(middleware) {
        self$middleware <- c(self$middleware, middleware)
        return(self$middleware)
      },

      # Add passed in  listeners
      addListener = function(listener) {
        self$listeners <- c(self$listeners, listener)
        return(self$listeners)
      },

      # Process passed event
      processEvent = function(event, ...) {
        # Filter the listeners
        listeners <- Filter(
          f = function(ls) { ls$event == event },
          x = self$listeners
        )
        # ??
        out <- lapply(listeners, function(ls) { ls$func(event, ...) })
        return(out)
      },

      # Invoke commands
      invoke = function( request,
                         websocket_msg = NULL,
                         websocket_binary = NULL ) {
        # Define new request and response objects
        response <- Response$new()
        request <- Request$new(request)
        error <- newError() # DNE-yet

        body <- NULL

        self$processEvent(event = "start", request, response, error)


      }

    )
  )

#' Request Handler Object Class

RequestHandler <-
  R6::R6Class(
    classname = "RequestHandler",
    public = list(
      # Define middleware and listeners
      middleware = c(),
      listeners = c(),

      # Add passed in middleware
      addMiddleware = function(mw) {
        self$middleware <- c(self$middleware, mw)
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
        listeners <- Filter( f = function(l) { l$event == event },
                             x = self$listeners )
        # handle the listener defined function for the event
        lapply(listeners, function(l) { l$FUN(event, ...) })
      },

      # Invoke commands
      invoke = function( request,
                         websocket_msg = NULL,
                         websocket_binary = NULL ) {
        # Define new request and response objects
        response <- Response$new()
        request <- Request$new(request)
        error <- newError()

        body <- NULL

        self$processEvent(event = "start", request, response, error)

        for ( mw in self$middleware ) {
          path <- matchPath(mw$path, request$path)
          request$addParameters(path$params)

          # Handle http protocol logic
          httpLogic <- any( path$match && (mw$method == request$method),
                            path$match && is.null(mw$method),
                            is.null(mw$path) && (mw$method == request$method),
                            is.null(mw$path) && is.null(mw$method) )
          # Handle websocket logic
          wsLogic <- any( path$match,
                          is.null(mw$path) )

          # Check capture group logic
          desired <- any( (request$protocol == "http" && httpLogic),
                         (request$protocol == "websocket" && wsLogic) )

          # Check websocket/http logic and return proper response
          if ( desired ) {
            result <-
              try({
                body <- switch(request$protocol,
                  "http" = mw$FUN( request = request,
                                   response = response,
                                   error = error ),
                  "websocket" = mw$FUN( binary = websocket_binary,
                                        message = websocket_msg,
                                        response = response,
                                        error = error )
                )}, silent = TRUE)

            if ( "try-error" %in% class(result) ) {
              self$processEvent( event = "error",
                                 request, response,
                                 error = error,
                                 as.character(body) )
              error$set(as.character(body))
              body <- NULL
            }

            if ( is.null(response$body) && !is.null(body) ) {
              response$setBody(body)
            }

            # if ( !is.null(response$body) ) {
            #   break
            # }
          }
          # if ( !is.null(response$body) ) {
          #   break
          # }
        }

        # if output is verbose return the options
        if ( getOption("beakr.verbose") ) {
          cat( toupper(request$protcol), "|",
               request$path, "-",
               response$status, "\n",
               sep = " " )
        }

        # Show failure
        if ( is.null(response$body) ) {
          msg <- "Request not handled: No body set by middleware"
          self$processEvent(event = "error", request, response, error, msg)
          stop(msg)
        }

        self$processEvent(event = "finish", request, response, error)
        response$structured(protocol = request$protocol)
      }

    )
  )

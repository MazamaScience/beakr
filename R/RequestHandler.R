#'  Request Handler Object Class
#' @method middleware
#' @method listeners
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
      invoke = function( req,
                         websocket_msg = NULL,
                         websocket_binary = NULL ) {
        # Define new req and res objects
        res <- Response$new()
        req <- Request$new(req)
        err <- Error$new()

        body <- NULL

        self$processEvent(event = "start", req, res, err)

        for ( mw in self$middleware ) {
          path <- matchPath(mw$path, req$path)
          req$addParameters(path$params)

          # Handle http protocol logic
          httpLogic <- any( path$match && (mw$method == req$method),
                            path$match && is.null(mw$method),
                            is.null(mw$path) && (mw$method == req$method),
                            is.null(mw$path) && is.null(mw$method) )
          # Handle websocket logic
          wsLogic <- any( path$match,
                          is.null(mw$path) )

          # Check capture group logic
          desired <- any( (req$protocol == "http" && httpLogic),
                         (req$protocol == "websocket" && wsLogic) )

          # Check websocket/http logic and return proper res
          if ( desired ) {
            result <-
              try({
                body <- switch(req$protocol,
                  "http" = mw$FUN( req = req,
                                   res = res,
                                   err = err ),
                  "websocket" = mw$FUN( binary = websocket_binary,
                                        message = websocket_msg,
                                        res = res,
                                        err = err )
                )}, silent = TRUE)

            if ( "try-err" %in% class(result) ) {
              self$processEvent( event = "err",
                                 req,
                                 res,
                                 err,
                                 as.character(body) )
              err$set(as.character(body))
              body <- NULL
            }

            if ( is.null(res$body) && !is.null(body) ) {
              res$setBody(body)
            }

            if ( !is.null(res$body) ) {
              break
            }
          }
        }

        # if output is verbose return the options
        if ( getOption("beakr.verbose") ) {
          cat( toupper(req$protcol), "|",
               req$path, "-",
               res$status, "\n",
               sep = " " )
        }

        # Show failure
        if ( is.null(res$body) ) {
          msg <- "Request not handled: No body set by middleware"
          self$processEvent(event = "err", req, res, err, msg)
          stop(msg)
        }

        self$processEvent(event = "finish", req, res, err)
        res$structured(protocol = req$protocol)
      }

    )
  )

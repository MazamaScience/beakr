#' @export
#' @title Router Class
#'
#' @description
#' The \code{Router} object represents the handling of routing and middleware
#' (such as getr(), putr(), postr(), and so on). Once a \code{Router} object is
#' instantiated, middleware and HTTP method routes can be added. The top level
#' \code{Beakr} object initializes with the creation of a \code{Router} object.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Fields:
#'
#' \describe{
#'   \item{\code{middleware}}{
#'   A list of specified middleware function or functions.
#'   }
#'   \item{\code{listeners}}{
#'   A list of specified listeners.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#' \item{\code{addMiddleware(middlware)}}{
#'   A method to add middleware function(s) to \code{middleware}.
#'   }
#'   \item{\code{addListener(listener)}}{
#'   A method to add listeners to \code{listeners}.
#'   }
#'   \item{\code{processEvent(event, ...)}}{
#'   Processes the event heard by the \code{Listener}.
#'   }
#'   \item{\code{invoke(req, websocket_msg, websocket_binary)}}{
#'   This method is used to create the a request-response cycle objects of the
#'   provided middleware.
#'   }
#' }
#'
#' @seealso \code{\link{Response}} and \code{\link{TestRequest}}
#' @keywords internal
Router <-
  R6::R6Class(
    classname = "Router",
    public = list(
      middleware = c(),
      listeners = c(),
      addMiddleware = function(middleware) {
        self$middleware <- c(self$middleware, middleware)
        return(self$middleware)
      },
      addListener = function(listener) {
        self$listeners <- c(self$listeners, listener)
        return(self$listeners)
      },
      processEvent = function(event, ...) {
        # Filter the listeners
        listeners <- Filter( f = function(l) { l$event == event },
                             x = self$listeners )
        # handle the listener defined function for the event
        lapply(listeners, function(l) { l$FUN(event, ...) })
      },
      invoke = function( req,
                         websocket_msg = NULL,
                         websocket_binary = NULL ) {
        # Define new req and res objects
        res <- Response$new()
        req <- Request$new(req)
        err <- Error$new()

        body <- NULL

        self$processEvent(event = 'start', req, res, err)

        for ( mw in self$middleware ) {
          path <- .matchPath(mw$path, req$path)
          req$addParameters(path$parameters)

          # Handle http protocol logic
          httpLogic <- any( path$match && (mw$method == req$method),
                            path$match && is.null(mw$method),
                            is.null(mw$path) && (mw$method == req$method),
                            is.null(mw$path) && is.null(mw$method) )
          # Handle websocket logic
          wsLogic <- any( path$match,
                          is.null(mw$path) )

          # Check capture group logic
          desired <- any( (req$protocol == 'http' && httpLogic),
                         (req$protocol == 'websocket' && wsLogic) )

          # Check websocket/http logic and return proper res
          if ( desired ) {

            body <-
              try({
                switch( req$protocol,
                        'http' = mw$FUN(req = req, res = res, err = err),
                        'websocket' = mw$FUN( binary = websocket_binary,
                                              message = websocket_msg,
                                              res = res,
                                              err = err ))
              })

            if ( 'try-error' %in% class(body) ) {
              self$processEvent( event = 'error',
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

        # Show failure
        if ( is.null(res$body) ) {
          msg <- "Request not handled: No body set by middleware"
          self$processEvent(event = "error", req, res, err, msg)
          stop(msg)
        }

        self$processEvent(event = "finish", req, res, err)
        res$structured(protocol = req$protocol)
      }

    )
  )


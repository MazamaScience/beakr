#' Router Class
#'
#' @description
#' `Router` coordinates HTTP/WebSocket routing and middleware execution.
#' After instantiation, you can register middleware and event listeners; then
#' call `$invoke()` to run the request/response cycle.
#'
#' @docType class
#' @name Router
#' @export
#'
#' @format An [`R6::R6Class`] generator for `Router` objects.
#'
#' @seealso [Response], [Request], [Error]

Router <-
  R6::R6Class(
    classname = "Router",
    public = list(
      #' @field middleware List of middleware entries.
      middleware = c(),
      #' @field listeners List of listeners (event handlers).
      listeners = c(),

      #' @description
      #' Append middleware entry/entries to `middleware`.
      #' @param middleware A middleware object/function or list of them.
      addMiddleware = function(middleware) {
        self$middleware <- c(self$middleware, middleware)
        self$middleware
      },

      #' @description
      #' Append a listener to `listeners`.
      #' @param listener A listener object with fields like `event` and `FUN`.
      addListener = function(listener) {
        self$listeners <- c(self$listeners, listener)
        self$listeners
      },

      #' @description
      #' Dispatch an event to all matching listeners.
      #' @param event Event name (e.g., `"start"`, `"error"`, `"finish"`).
      #' @param ... Additional arguments forwarded to each listener `FUN`.
      processEvent = function(event, ...) {
        listeners <- Filter(function(l) { l$event == event }, self$listeners)
        lapply(listeners, function(l) { l$FUN(event, ...) })
      },

      #' @description
      #' Run the routing/middleware pipeline and return a structured response.
      #' @param req Raw request object or `Request` instance.
      #' @param websocket_msg Optional WebSocket text message.
      #' @param websocket_binary Optional WebSocket binary payload (raw).
      invoke = function(req, websocket_msg = NULL, websocket_binary = NULL) {
        res <- Response$new()
        req <- Request$new(req)
        err <- Error$new()
        body <- NULL

        self$processEvent(event = "start", req, res, err)

        for ( mw in self$middleware ) {
          path <- .matchPath(mw$path, req$path)
          req$addParameters(path$parameters)

          httpLogic <- any(
            path$match && (mw$method == req$method),
            path$match && is.null(mw$method),
            is.null(mw$path) && (mw$method == req$method),
            is.null(mw$path) && is.null(mw$method)
          )
          wsLogic <- any(path$match, is.null(mw$path))
          desired <- any(
            (req$protocol == "http" && httpLogic),
            (req$protocol == "websocket" && wsLogic)
          )

          if ( desired ) {
            body <- try({
              switch(req$protocol,
                     "http" = mw$FUN(req = req, res = res, err = err),
                     "websocket" = mw$FUN(binary = websocket_binary,
                                          message = websocket_msg,
                                          res = res, err = err))
            })

            if ( "try-error" %in% class(body) ) {
              self$processEvent("error", req, res, err, as.character(body))
              err$set(as.character(body))
              body <- NULL
            }

            if ( is.null(res$body) && !is.null(body) ) {
              res$setBody(body)
            }

            if ( !is.null(res$body) ) break
          }
        }

        if ( is.null(res$body) ) {
          msg <- "Request not handled: No body set by middleware"
          self$processEvent("error", req, res, err, msg)
          stop(msg)
        }

        self$processEvent("finish", req, res, err)
        res$structured(protocol = req$protocol)
      }
    )
  )

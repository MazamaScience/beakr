#' Beakr class
#'
#' The \code{Beakr} class defines the server instance utilizing the
#' \code{httpuv} package. This class defines an interface for the rest of the
#' \code{beakr} package and is therefore meant to be instantiated.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{routerObject()}}{
#'   An instantiated \code{Router} object.
#'   }
#'   \item{\code{serverObject()}}{
#'   The instantiated \code{Server} object.
#'   }
#'   \item{\code{appDefinition()}}{
#'   A method to define the functions or middleware of users application.
#'   }
#'   \item{\code{addCollectedMiddleware(collector)}}{
#'   Adds the users "collected" middleware to the \code{Beakr} defined
#'   middleware.
#'   }
#'   \item{\code{initialize()}}{
#'   Creates a new \code{Router} object.
#'   }
#'   \item{\code{start(host, port, daemonized)}}{
#'   Returns a running server. If \code{daemonized = TRUE}, the server will run
#'   in the background.
#'   }
#'   \item{\code{print(...)}}{
#'   Returns a console output of the instance and its number of middleware
#'   attached.
#'   }
#' }
#'
#' @section Package details:
#'
#' The \code{beakr} package provides a minimal web framework for for developing
#' lightweight APIs in R. The package includes basic functionality for handling
#' common \code{HTTP} requests. \code{beakr} is a ground-up rewrite and
#' continuation of the \code{jug} package developed by Bart Smeets. The
#' \code{beakr} package is supported and maintained by
#' \href{http://www.mazamascience.com/}{Mazama Science}.
#'
#' @seealso \code{\link{Router}} and \code{\link{Middleware}}
#' @keywords internal
Beakr <-
  R6::R6Class(
    classname = "Beakr",
    public = list(
      routerObject = NULL,
      serverObject = NULL,
      appDefinition = function() {
        list(
          # Call a req invoke
          call = function(req) {
            self$routerObject$invoke(req)
          },
          onWSOpen = function(websocket) {
            websocket$onMessage(function(binary, message) {
              websocket$send(self$routerObject$invoke(
                req              = websocket$req,
                websocket_msg    = message,
                websocket_binary = binary
              ))
            })
          }
        )
      },
      include = function(bundle) {
        self$routerObject$addMiddleware(
          bundle$routerObject$middleware
        )
      },
      initialize = function() {
        self$routerObject <- Router$new()
        # Set Early for testing purposes when serve_it isn't called - Optional?
        options("beakr.verbose" = FALSE)
      },
      start = function(host, port, daemonized) {
        # Run in background
        if ( daemonized ) {
        self$serverObject <-
          httpuv::startServer( host = host,
                               port = port,
                               app  = self$appDefinition() )
        # Run in foreground
        } else {
          self$serverObject <-
            httpuv::runServer( host = host,
                               port = port,
                               app  = self$appDefinition() )
        }

      },
      print = function(...) {
        if ( !is.null(self$serverObject) ) {
          st <- ifelse(self$serverObject$isRunning(), "Active", "Inactive")
          hst <- self$serverObject$getHost()
          prt <- self$serverObject$getPort()
          mws <- length(self$routerObject$middleware)
        } else {
          st <- "Inactive"
          hst <- "..."
          prt <- "..."
          mws <- length(self$routerObject$middleware)
        }
        cat( "Beakr Instance\n",
             "State:",st,"|","Host:",hst,"|","Port:",prt,"|","Middlewares:",mws,
             "\n",
             sep = " " )
        invisible(self)
      }
    )
  )


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
#'   \item{\code{router()}}{
#'   An instantiated \code{Router} object.
#'   }
#'   \item{\code{server()}}{
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
#'   Creates a new \code{Router} object for the \code{router}
#'   method.
#'   }
#'   \item{\code{start(host, port, daemon)}}{
#'   Returns a running server. If \code{daemon = TRUE}, the server will run
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
      router = NULL,
      server = NULL,
      appDefinition = function() {
        list(
          # Call a req invoke
          call = function(req) {
            self$router$invoke(req)
          },
          onWSOpen = function(websocket) {
            websocket$onMessage(function(binary, message) {
              websocket$send(self$router$invoke(
                req              = websocket$req,
                websocket_msg    = message,
                websocket_binary = binary
              ))
            })
          }
        )
      },
      include = function(bundle) {
        self$router$addMiddleware(
          bundle$router$middleware
        )
      },
      initialize = function() {
        self$router <- Router$new()
        # Set Early for testing purposes when serve_it isn't called - Optional?
        options("beakr.verbose" = TRUE)
      },
      start = function(host, port, daemon) {
        # Run in background
        if ( daemon ) {
          self$server <-
            httpuv::startServer( host = host,
                                 port = port,
                                 app  = self$appDefinition() )
        # Run in foreground
        } else {
          self$server <-
            httpuv::runServer( host = host,
                               port = port,
                               app  = self$appDefinition() )
        }
      },
      print = function() {

        if ( !is.null(self$server) ) {
          st <- ifelse(self$server$isRunning(), "Active", "Inactive")
          hst <- self$server$getHost()
          prt <- self$server$getPort()
          mws <- length(self$router$middleware)
        } else {
          st <- "Inactive"
          hst <- "..."
          prt <- "..."
          mws <- length(self$router$middleware)
        }

        cat( "Beakr Instance\n",
             "State:",st,"|","Host:",hst,"|","Port:",prt,"|","Middlewares:",mws,
             "\n",
             sep = " " )
        invisible(self)
      }
    )
  )


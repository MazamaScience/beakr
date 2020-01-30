#'
#' @title A minimalist web framework.
#'
#' @description
#' The \pkg{beakr} package provides a minimalist web framework for for
#' developing application programming interfaces in R. The package includes
#' basic functionality for handling common \code{HTTP} requests.
#'
#' \pkg{beakr} allows R code to listen for and respond to HTTP requests, so
#' you can serve web traffic directly from an R process. \pkg{beakr} relies heavily
#' on the \href{https://github.com/rstudio/httpuv}{httpuv} package, and therefore
#' the lower level \href{https://github.com/joyent/libuv}{libuv} and
#' \href{https://github.com/joyent/http-parser}{http-parser} C libraries.
#' \pkg{beakr} is a ground-up rewrite and
#' continuation of the \pkg{jug} package developed by Bart Smeets. The
#' \pkg{beakr} package is supported and maintained by
#' \href{http://www.mazamascience.com/}{Mazama Science}.
#'
#' @seealso \code{\link{newBeakr}}
#'
#' @name beakr-package
#' @aliases Beakr-Package
#' @docType package
#' @title A minimalist web framework
#' @author Hans Martin \email{hans@mazamascience.com}
#' @keywords package
NULL

#' @export
#' @importFrom R6 R6Class
#' @title Beakr Application class
#'
#' @description The \code{Beakr} class defines the server instance utilizing the
#' \pkg{httpuv} package. This class defines an interface for the rest of the
#' \pkg{beakr} package and is therefore meant to be instantiated.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Methods:
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
#' The \pkg{beakr} package provides a minimal web framework for for developing
#' lightweight APIs in R. The package includes basic functionality for handling
#' common \code{HTTP} requests. \pkg{beakr} is a ground-up rewrite and
#' continuation of the \pkg{jug} package developed by Bart Smeets. The
#' \pkg{beakr} package is supported and maintained by
#' \href{http://www.mazamascience.com/}{Mazama Science}.
#'
#' @seealso \code{\link{Router}} and \code{\link{Middleware}}
#'
Beakr <-
  R6::R6Class(
    classname = "Beakr",
    public = list(
      name = NULL,
      router = NULL,
      server = NULL,
      appDefinition = function() { # Look into renaming
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
      initialize = function() {
        self$router <- Router$new()

        if ( is.null(self$name) ) {
          self$name <- .randomName()
        }
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
          name <- self$name
          st <- ifelse(self$server$isRunning(), "Active", "Inactive")
          hst <- self$server$getHost()
          prt <- self$server$getPort()
          mws <- length(self$router$middleware)
        } else {
          name <- self$name
          st <- "Inactive"
          hst <- "..."
          prt <- "..."
          mws <- length(self$router$middleware)
        }

        cat( "Beakr Instance: ", self$name, "\n",
             "State:",st,"|","Host:",hst,"|","Port:",prt,"|","Middlewares:",mws,
             "\n",
             sep = " " )
        invisible(self)
      }
    )
  )


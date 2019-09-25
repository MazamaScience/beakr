#' Middleware Class

Middleware <-
  R6::R6Class(
    classname = "Middleware",
    public = list(
      path = NULL,
      FUN = NULL,
      method = NULL,
      protocol = NULL,
      # Initialize
      initialize = function(FUN, path, method, websocket) {
        self$FUN = FUN
        self$path = path
        self$method = method
        self$protocol = if ( websocket ) {"websocket"} else {"http"}#ifelse(websocket, "websocket", "http")
      }
    )
  )

#' Internal function to add middleware
#'
#' @param beakr
#' @param FUN
#' @param path
#' @param method
#' @param websocket
#'
#' @return

addMiddleware <-
  function(beakr, FUN, path = NULL, method = NULL, websocket = FALSE) {
    method <- ifelse(!is.null(method), toupper(method), NULL)
    # Create new middleware
    mw <- Middleware$new(FUN, path, method, websocket)
    # Add the middleware
    beakr$requestHandler$addMiddleware(mw)
    return(beakr)
  }

#' GET-binding middleware
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
get <- function(beakr, ...) {
  return(UseMethod("get"))
}

#' @export
get.default <- function(beakr, ...) {
  return(base::get(beakr, ...))
}


#' @describeIn GET-binding middleware
#' @export
get.Beakr<-function(beakr, ...){
  funcs<-list(...)
  path<-funcs[[1]]
  funcs<-funcs[-1]

  lapply(funcs, function(mw_func) addMiddleware(beakr, mw_func, path, method="GET"))

  return(beakr)
}

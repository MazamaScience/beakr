# #' Internal function to add listeners
# #'
# #' Internal function.
# #'
# #' @param beakr a beakr instance.
# #' @param FUN listener function
# #' @param event the event to listen for, such as "start" or "finish".
# addListener <- function(beakr, FUN, event) {
#   mw <- Listener$new(FUN, event)
#   beakr$routerObject$addListener(mw)
#   return(beakr)
# }

#' Internal function to add middleware
#'
#' Internal function.
#'
#' @param beakr a beakr instance.
#' @param FUN the function to route middleware
#' @param path the path to rouet the middleware
#' @param method the HTTP method to employ
#' @param websocket boolean, TRUE if websocket.
routeMiddleware <- function( beakr, FUN, path = NULL,
                             method = NULL, websocket = FALSE ) {

  if ( !is.null(method) ) {
    method <- toupper(method)
  } else {
    method <- NULL
  }

  # Create new middleware
  mw <- Middleware$new(FUN, path, method, websocket)
  # Add the middleware
  beakr$routerObject$addMiddleware(mw)
  return(beakr)
}


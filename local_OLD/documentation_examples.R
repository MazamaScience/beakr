
# ------ utils.R errorHandler() ------------------------------------------------

#' @examples
#' \dontrun{
#' # Create an instance and add the error handler last.
#' newBeakr() %>%
#'   use(path = "/", decorate(function(n) { paste("Hi, ", n) })) %>%
#'   handler() %>%
#'   listen()
#' # In shell
#' #  curl http://127.0.0.1:12518/
#' # > "status":"Page not found.","status_code":404
#' # An error was thrown because the parameter "n" is not provided and
#' # required by the middleware.
#' killAll()
#' }

# ------ utils.R include() -----------------------------------------------------

#' @examples
#' \dontrun{
#' # Construct primary instance
#' primary <- newBeakr() %>%
#'   getr("/primary-app",
#'       function(req, res, err) { "Primary Application!" })
#'
#' # Construct middleware
#' ext <- use(beakr = NULL, "/",
#'            decorate(function(name) { paste("Hi ", name, "!\n") })) %>%
#'   handler()
#'
#' # Include the external middleware in the primary instance
#' primary %>% include(ext)
#' }


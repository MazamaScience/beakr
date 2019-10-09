#
# ----- http-methods.R GET() ---------------------------------------------------

#' @examples
#' \dontrun{
#' # Create a beakr instance
#' server <- newBeakr() %>%
#'   GET(path = "/", function(req, res, err) { "Hello, world!" })
#' # Listen for HTTP/WebSocket requests
#' starBeakr(server)
#' # In terminal:
#' #  curl http://127.0.0.1:8080/
#' # > Hello, world!
#' }
# TODO:  Some cleanup needed here.
# The methods below are used to determine what "get" to use.
# get <- function(beakr, path, ...) {
#   if ( is.null(beakr) ) {
#     return(get.Beakr(beakr, path, ...))
#   } else {
#     return(UseMethod("get"))
#   }
# }
# @export
# get.default <- function(beakr, ...) {
#   return(base::get(beakr, ...))
# }
# @describeIn GET Beakr middleware function.
# @export


# ----- http-methods.R POST() --------------------------------------------------

#' @examples
#' \dontrun{
#' # Create a beakr instance
#' server <- newBeakr() %>%
#'   POST("/", function(req, res, err) { "Successful POST request!" })
#' # Listen for HTTP/WebSocket requests
#' starBeakr(server)
#' # In terminal:
#' #  curl -X POST http://127.0.0.1:8080/
#' # > Successful POST request!
#' }

# ----- http-methods.R PUT() ---------------------------------------------------

#' @examples
#' \dontrun{
#' # Create a beakr instance
#' server <- newBeakr() %>%
#'   PUT("/", function(req, res, err) { "Successful PUT request!" })
#' # Listen for HTTP/WebSocket requests
#' starBeakr(server)
#' # In terminal:
#' #  curl -X PUT http://127.0.0.1:8080/
#' # > Successful PUT request!
#' }


# ------ utils.R starBeakr() ---------------------------------------------------

#' @examples
#' \dontrun{
#' bk <- newBeakr() %>% starBeakr(port = 1234, daemonized = TRUE)
#' }

# ------ utils.R kill() --------------------------------------------------------

#' @examples
#' bk <- newBeakr()
#' starBeakr(bk, daemonized = TRUE)
#' kill(bk)

# ------ utils.R errorHandler() ------------------------------------------------

#' @examples
#' \dontrun{
#' # Create an instance and add the error handler last.
#' newBeakr() %>%
#'   use(path = "/", decorate(function(n) { paste("Hi, ", n) })) %>%
#'   errorHandler() %>%
#'   starBeakr()
#' # In shell
#' #  curl http://127.0.0.1:8080/
#' # > "status":"Page not found.","status_code":404
#' # An error was thrown because the parameter "n" is not provided and
#' # required by the middleware.
#' killall()
#' }

# ------ utils.R include() -----------------------------------------------------

#' @examples
#' \dontrun{
#' # Construct primary instance
#' primary <- newBeakr() %>%
#'   GET("/primary-app",
#'       function(req, res, err) { "Primary Application!" })
#'
#' # Construct middleware
#' ext <- use(beakr = NULL, "/",
#'            decorate(function(name) { paste("Hi ", name, "!\n") })) %>%
#'   errorHandler()
#'
#' # Include the external middleware in the primary instance
#' primary %>% include(ext)
#' }


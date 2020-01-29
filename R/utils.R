# ----- Pipeline functions -----------------------------------------------------

#' @export
#' @title Create a new Beakr instance
#'
#' @param name Optional name assigned to the \code{Beakr} instance.
#'
#' @description Create a \code{Beakr} instance by calling the top-level
#' \code{newBeakr()} function. If \code{name} is not supplied, a random name
#' will be assigned.
#'
#' This \code{Beakr} instance will then begin a pipeline of separate middleware
#' steps for routing, serving files and handling errors. The pipeline will
#' end with the \code{listen()} function.
#'
#' @return A new and empty \code{Beakr} instance.
#'
#' @examples
#' \donttest{
#' # Create an new Beakr instance
#' beakr <- newBeakr()
#'
#' # Create a pipeline of hanldlers
#' beakr %>%
#'   httpGET(path = "/route_A", function(res, req, err) {
#'     print("This is route 'A'.")
#'   }) %>%
#'   httpGET(path = "/route_B", function(res, req, err) {
#'     print("This is route 'B'.")
#'   }) %>%
#'   handleErrors() %>%
#'   listen(host = '127.0.0.1', port = 25118, daemon = TRUE)
#'
#' # ------------------------------------------------------------
#' # POINT YOUR BROWSER AT:
#' # * http://127.0.0.1:25118/route_A
#' # * http://127.0.0.1:25118/route_B
#' # ------------------------------------------------------------
#'
#' # Stop the beakr instance server
#' stopServer(beakr)
#' }
newBeakr <- function(
  name = NULL
) {

  beakr <- Beakr$new()

  if ( !is.null(name) )
    beakr$name <- name

  return(beakr)

}


################################################################################
################################################################################
################################################################################
#' @export
#' @title Serve static files
#'
#' @description Binds to GET requests that aren't handled by specified paths.
#' Should support all filetypes; returns image and octet-stream types as a raw
#' string.
#'
#' @details Serve static files from the host machine. Currently supports images,
#' PDF, JSON, HTML, and raw text. The static file appear at a URL composed of
#' the \code{route} and the file name and extension which will be extracted
#' from \code{filePath}.
#'
#' For example, specifying:
#' \preformatted{
#' beakr %>%
#'   ...
#'   serverStaticFiles(/)
#'   ...
#'   listen()
#' }
#'
#' TODO
#'
#' @param beakr \code{Beakr} instance or \code{NULL}.
#' @param route String representing a \emph{route}, a virtual directory path
#' where the static file will appear in a URL.
#' @param filePath String representing path and name of the file to be served
#' relative to the directory in which the beakr script is running.
#'
#' @return A \code{Beakr} with added middleware.

serveStaticFiles <- function(
  beakr = NULL,
  route = NULL,
  filePath = NULL
) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  # serve_file <- function(req, res, err) {
  #
  #   if ( file.exists(file) ) {
  #
  #     url_path <- paste0('/', path, utils::tail(unlist(strsplit(file, '/')), n = 1))
  #
  #     if ( req$path == url_path ) {
  #       mime_type <- mime::guess_type(file)
  #       res$setContentType(mime_type)
  #       data <- readBin( con  = file,
  #                        what = "raw",
  #                        n    = file.info(file)$size )
  #       if ( grepl("image|octect|pdf|json", mime_type) ) { # Assumptions...
  #         return(data)
  #       } else {
  #         return(rawToChar(data))
  #       }
  #     } else {
  #       res$setStatus(404L)
  #       return(NULL)
  #     }
  #
  #   }
  #
  # }

  beakr <-
    httpGET(
      beakr = beakr,
      path = NULL,
      FUN = function(req, res, err) {

        if ( file.exists(file) ) {

          url_path <- paste0('/', route, utils::tail(unlist(strsplit(filePath, '/')), n = 1))

          if ( req$path == url_path ) {
            mime_type <- mime::guess_type(filePath)
            res$setContentType(mime_type)
            data <- readBin( con  = filePath,
                             what = "raw",
                             n    = file.info(file)$size )
            if ( grepl("image|octect|pdf|json", mime_type) ) { # Assumptions...
              return(data)
            } else {
              return(rawToChar(data))
            }
          } else {
            res$setStatus(404L)
            return(NULL)
          }

        } # END if ( file.exists(file) )

      } # END FUN
    )

  return(beakr)

}

#' #' Middleware to serve static files
#' #'
#' #' Binds to get requests that aren't handled by specified paths. Should support
#' #' all filetypes; returns image and octet-stream types as a raw string. \cr\cr
#' #' Note: the \code{path} argument is not related to the file being served. If
#' #' \code{path} is given, the static file middleware will bind to \code{path},
#' #' however for finding the files on the local filesystem it will strip
#' #' \code{path} from the file location. For example, let's assume
#' #' \code{path='my_path'}, the following url \code{/my_path/file/to/serve.html}
#' #' will serve the file \code{file/to/serve.html} from the \code{root_path} folder.
#' #'
#' #' @param jug the jug instance
#' #' @param path the path to bind to, default = NULL (all paths)
#' #' @param root_path the file path to set as root for the file server
#' #'
#' #' @export
#' serve_static_files<-function(jug, path=NULL, root_path=getwd()){
#'   get(jug, path = NULL, function(req, res, err){
#'
#'     if(substring(req$path, nchar(req$path)) == "/"){
#'       req$path <- paste0(req$path, "index.html")
#'     }
#'
#'     if(is.null(path)){
#'       file_path <- paste0(root_path, '/', req$path)
#'     } else {
#'       partial_file_path <- gsub(paste0('.*', path, '(.*)'), '\\1', req$path)
#'       file_path <- paste0(root_path, '/', partial_file_path)
#'     }
#'
#'     bound <- ifelse(is.null(path), TRUE, substr(req$path, 2, nchar(path) + 1) == path)
#'
#'     if(file.exists(file_path) & bound){
#'       mime_type <- mime::guess_type(file_path)
#'       res$content_type(mime_type)
#'
#'       data <- readBin(file_path, 'raw', n=file.info(file_path)$size)
#'
#'       if(grepl("image|octet|pdf", mime_type)){ # making a lot of assumptions here
#'         return(data)
#'
#'       } else {
#'         return(rawToChar(data))
#'
#'       }
#'
#'     } else {
#'       res$set_status(404)
#'       return(NULL)
#'     }
#'
#'   })
#' }
################################################################################
################################################################################
################################################################################



#' @export
#' @title Error handling middleware for a beakr instance
#'
#' @description This default error-handling middleware function should be added
#' at the end of the middleware function pipeline. Any errors will be returned
#' within a JSON wrapper.
#'
#' The general structure for a stand-alone executable script with a
#' \code{Beakr} webservice typically looks like this:
#'
#' \preformatted{
#' newBeakr() %>%
#'
#'   httpGET(<route_A>, function(req, res, err) {
#'     ...
#'   }) %>%
#'
#'   httpGET(<route_B>, function(req, res, err) {
#'     ...
#'   }) %>%
#'
#'   handleErrors() %>%
#'
#'   listen()
#' }
#'
#' @param beakr Beakr instance
#'
#' @return A \code{Beakr} instance with added middleware.
#'

handleErrors <- function(
  beakr = NULL
) {

  # TODO:  Could support FUN = NULL in function signature so other error
  # TODO:  functions could be supplied.

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  beakr <-
    .routeMiddleware(
      beakr = beakr,
      FUN = jsonError,
      path = NULL,
      method = NULL,
      websocket = FALSE
    )

  return(beakr)

}

#' @export
#' @title Listen for connections on a Beakr instance
#'
#' @description Binds and listens for connections at the specified host and port.
#'
#' @details
#' \code{listen()} binds the specified host and port and listens for connections
#' on a thread. The thread handles incoming requests. when it receives an HTTP
#' request, it will schedule a call to the user-defined middleware and handle the
#' request.
#'
#' If \code{daemon = TRUE}, \code{listen()} binds the specified port and listens
#' for connections on a thread running in the background.
#'
#' See the \code{httpuv} package for more details.
#'
#' @note The default port number 25118 was generated using:
#' \preformatted{
#' > match(c("b","e","a","k","r"), letters) %% 10
#' [1] 2 5 1 1 8
#' }
#'
#' @param beakr \code{Beakr} instance.
#' @param host String that is a valid IPv4 or IPv6 address to listen on.
#' Defaults to the local host ("127.0.0.1").
#' @param port Number or integer that indicates the port to listen on. Default
#' is a port opened on 25118.
#' @param daemon Logical specifying whether the server should be run in the
#' background.
#' @param verbose Logical specifying whether to print out details of the
#' \code{Beakr} instance now running.
#'
#' @return A \code{Beakr} instance with an active server.
#'
#' @examples
#' # Run in the background
#' beakr <- newBeakr()
#' beakr %>%
#'   httpGET("/", function(req, res, err) {
#'     return("Successful GET request!\n")
#'   }) %>%
#'   listen(daemon = TRUE)
#'
#' # Stop the server
#' stopServer(beakr)

listen <- function(
  beakr = NULL,
  host = "127.0.0.1",
  port = 25118,
  daemon = FALSE,
  verbose = TRUE
) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  beakr$start(host, port, daemon)

  if ( verbose )
    beakr$print()

  return(invisible(beakr))

}

# ----- Other functions --------------------------------------------------------

#' @export
#' @title Stop a beakr instance server
#'
#' @description Stops the server associated with a \code{Beakr} instance,
#' closing all open connections and unbinding the port.
#'
#' @param beakr \code{Beakr} instance.
#' @param verbose Logical specifying whether to print out details of the
#' \code{Beakr} instance just stopped.
#'
#' @return None
#'
#' @examples
#' beakr <- newBeakr()
#' beakr %>%
#'   listen(daemon = TRUE, verbose = TRUE)
#' stopServer(beakr, verbose = TRUE)

stopServer <- function(
  beakr = NULL,
  verbose = TRUE
) {

  if ( is.null(beakr) )
    stop("'beakr' is not defined")

  httpuv::stopServer(beakr$server)

  if ( verbose ) {
    cat("Stopped ")
    beakr$print()
  }

}

#' @export
#' @title Stop all servers
#'
#' @description Stops all \code{Beakr} instances (and any other servers created
#' with the \code{httpuv} package). This convenience function is a wrapper for
#' \code{httpuv::stopAllServers()} and is included to encourage experimentation
#' so that users who create multiple \code{Beakr} instances can quickly stop
#' them all.
#'
#' @return None
#'
#' @examples
#' beakr1 <- newBeakr()
#' beakr2 <- newBeakr()
#' beakr1 %>% listen(daemon = TRUE, port = 1234, verbose = TRUE)
#' beakr2 %>% listen(daemon = TRUE, port = 4321, verbose = TRUE)
#' length(httpuv::listServers())
#' httpuv_stopAllServers()
#' length(httpuv::listServers())

httpuv_stopAllServers <- function() {

  httpuv::stopAllServers()

}

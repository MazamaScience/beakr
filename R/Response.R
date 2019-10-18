#' Response Class
#'
#' The \code{Response} object represents represents the HTTP response that a
#' \code{Beakr} sends when it gets an HTTP request. It is by convention, the
#' object is always referred to as \code{res} (and the HTTP request is
#' \code{req}).
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @section Fields:
#'
#' \describe{
#'   \item{\code{headers}}{
#'   A list containing a key-value header list.
#'   }
#'   \item{\code{status}}{
#'   An integer HTTP status code.
#'   }
#'   \item{\code{body}}{
#'   Contains the response body.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#' \item{\code{setHeader(key, value)}}{
#'   Sets a key-value header, i.e. \code{"Content-Type" = "text/html"}.
#'   }
#'   \item{\code{setContentType(type)}}{
#'   Sets the response content-type.
#'   }
#'   \item{\code{setStatus(status)}}{
#'   Sets the HTTP status code.
#'   }
#'   \item{\code{redirect(url)}}{
#'   Sets the HTTP status to 302, "Found" and redirects to \code{url}.
#'   }
#'   \item{\code{setBody(body)}}{
#'   Sets the body response.
#'   }
#'   \item{\code{json(txt, auto_unbox = TRUE)}}{
#'   Applies a function to text convert to JSON and sets the content-type to
#'   JSON.
#'   }
#'   \item{\code{text(txt)}}{
#'   Sets the response body text.
#'   }
#'   \item{\code{structured(protocol)}}{
#'   Sets the response protocol, i.e. "http"
#'   }
#'   \item{\code{plot(plot_object, base64 = TRUE, ...)}}{
#'   Sets the response type to plot image output.
#'   }
#' }
#'
#' @seealso \code{\link{Response}} and \code{\link{TestRequest}}
#' @keywords internal
Response <-
  R6::R6Class(
    classname = "Response",
    public = list(
      headers = list("Content-Type" = "text/html"),
      status = 200L,
      body = NULL,
      setHeader = function(key, value) {
        self$headers[[key]] <- value
      },
      setContentType = function(type) {
        self$headers[["Content-Type"]] <- type
      },
      setStatus = function(status) {
        self$status <- status
      },
      redirect = function(url) {
        # Set status to 'Found'
        self$status <- 302L
        self$setHeader("Location", url)
      },
      setBody = function(body) {
        # Hack to avoid numeric res failure
        if ( self$headers[["Content-Type"]] == "text/html" ) {
          self$text(body)
        } else {
          self$body <- body
        }
      },
      json = function(txt, auto_unbox = TRUE) {
        self$body <- jsonlite::toJSON(txt, auto_unbox = auto_unbox)
        self$setContentType("application/json")
      },
      text = function(txt) {
        self$body <- as.character(txt)
        self$setContentType("text/html")
      },
      structured = function(protocol) {
        switch(
          EXPR = protocol,
          "http" = list( status  = self$status,
                         headers = self$headers,
                         body    = self$body ),
          "websocket" = self$body
        )
      },
      plot = function(plot_object, base64 = TRUE, ...) {
        # Create the plot
        plot_file <- tempfile(pattern = "beakr")
        png(filename = plot_file, ...)
        print(plot_object)
        dev.off()

        # Open file connection reading in binary mode
        file_connection <- file(description = plot_file, open = "rb")
        binary_image <- readBin(con = file_connection, what = "raw", n = 1e6)

        # if base 64 colors, encode binary image, otherwise dont
        if ( base64 ) {
          # Set content type to base64 image render
          self$setContentType("application/base64")
          # Display plot
          self$body <- base64enc::base64encode(binary_image)
        } else {
          # Set content type to image
          self$setContentType("image/png")
          # Display plot
          self$body <- binary_image
        }
        # Close the file connection
        close(con = file_connection)
      }

    )
  )

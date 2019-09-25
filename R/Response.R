#' Response Object Class
Response <-
  R6::R6Class(
    classname = "Response",
    public = list(
      headers = list("Content-Type" = "text/html"),
      # Initialize status as 'OK'
      status = 200L,
      body = NULL,

      setHeader = function(key, value) {
        self$headers[[key]] <- value
      },

      contentType = function(type) {
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
        # Hack to avoid numeric response failure
        if ( self$headers[["Content-Type"]] == "text/html" ) {
          self$text(body)
        } else {
          self$body <- body
        }
      },

      # Convert body to json
      json =  function(obj, auto_unbox = TRUE) {
        self$body <- jsonlite::toJSON(obj, auto_unbox = auto_unbox)
        sel$contentType("application/json")
      },

      text = function(text) {
        self$body <- as.character(text)
        self$contentType("text/html")
      },

      # Eval ??
      structured = function(protocol) {
        switch(
          EXPR = protocol,
          "http" = list( status  = self$status,
                         headers = self$headers,
                         body    = self$body ),
          "websocket" = self$body
        )
      },

      # Plotting functionality
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
          self$contentType("application/base64")
          # Display plot
          self$body <- base64enc::base64encode(binary_image)
        } else {
          # Set content type to image
          self$contentType
          # Display plot
          self$body <- binary_image
        }
        # Close the file connection
        close(con = file_connection)
      }

    )
  )

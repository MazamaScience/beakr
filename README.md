[![sm-beakr.png](https://i.postimg.cc/7YBB0Hnp/sm-beakr.png)](https://postimg.cc/bstHjjMT)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/beakr)](https://cran.r-project.org/package=beakr)
[![Downloads](http://cranlogs.r-pkg.org/badges/beakr)](https://cran.r-project.org/package=beakr)

## A Minimalist Web Framework for R

**beakr** is a minimalist web framework for developing web services in the R 
Language. **beakr** offers a robust set of fundamental web application features 
and is intended to simplify the development of web services that reflect R 
package APIs — without obscuring R's data processing capability and ease of use.

### Hello, world! - The beakr way

```r
library(beakr)

# Create a new beakr server
newBeakr() %>% 

  # Respond to GET requests at the "/hi" route
  httpGET(path = "/hi", function(req, res, err) {
    print("Hello, World!")
  }) %>% 
  
  # Respond to GET requests at the "/bye" route
  httpGET(path = "/bye", function(req, res, err) {
    print("Farewell, my friends.")
  }) %>% 
  
  # Handle any errors with a JSON response
  handleErrors() %>%
  
  # Start the server on port 25118
  listen(host = "127.0.0.1", port = 25118) 
```

A new web service is now available on the local host that responds to two
URLs:

* http://127.0.0.1:25118/hi
* http://127.0.0.1:25118/bye

## Overview

The **beakr** package allows R code to listen for and respond to HTTP requests, 
so you can serve web traffic directly from a _Beakr_ instance. The **beakr** 
package is intended to be simple, lightweight and unopinionated.  

While **beakr** is not recommended for building extensive web frameworks, R and 
the flexibility of the package are (potentially) up to the task. Keep in mind 
that **beakr** was not designed to be an especially performant web framework and 
the  _"batteries are certainly not included"_. If you're looking for full 
featured  web frameworks, there are better tools and languages for that 
(see [Shiny](https://shiny.rstudio.com), [django](https://www.djangoproject.com), etc.). 
**beakr** is inspired by the minimalist and massively-expandable frameworks 
offered by [Express.js](https://expressjs.com) and 
[Flask](https://palletsprojects.com/p/flask/). 

One of the reasons to use **beakr** is that it is incredibly flexible. It allows 
you to integrate your R code as _Middleware_ in a _Beakr_ instance. Middleware 
functions can execute any R code, make changes to the _Request_, _Response_, and
_Error_ objects, and then serve up the response at the end the request-response 
cycle. The **beakr** package loosely follows Express.js middleware semantics, 
where middleware functions are functions that have access to the _Request_, 
_Response_, and _Error_ objects of a _Beakr_ instance.

_Note:_ By convention, the _Response_, _Request_, and _Error_ objects are always 
referred to as `res`, `req` and `err`, respectively. See the package documentation 
for more information.

## Installation

When released, you will be able to install the latest release version from CRAN:

```
install.packages("beakr")
```

Or you can install the latest development version from GitHub: 

```
install.packages("devtools")
devtools::install_github("MazamaScience/beakr")
```

## Examples

### A KNN model webservice 

A _Beakr_ instance can easily expose R function signatures as webservice APIs. 
As an example, let's expose a simple machine learning model using the 
[caret](https://github.com/topepo/caret) package and the Iris data set. The 
`predict_species()` function accepts four arguments which it uses to predict the 
species of iris associated with incoming data. The _Beakr_ instance 
exposes this API and, when given JSON input with the required arguments, 
identifies and returns the species.

_Note_ that `httpPOST` attaches the URL path `/predict-species` only to http POST
requests. Pointing a browser at this URL path will issue a File Not found error
because the browser is issuing an http GET request. Like other frameworks,
**beakr** allows for method-specific URL routing.

```r
# Import libraries 
library(beakr)
library(caret)

# Load the Iris data set 
data('iris')

# Train using KNN
knn_model <- train(
  Species ~ ., 
  data = iris, 
  method = 'knn', 
  trControl = trainControl(method='cv', number=10), 
  metric = 'Accuracy'
)

# Function to predict the species using the trained model.
predict_species <- function(sl, sw, pl, pw) {
  test <- data.frame(
    Sepal.Length = as.numeric(sl),
    Sepal.Width = as.numeric(sw),
    Petal.Length = as.numeric(pl),
    Petal.Width = as.numeric(pw),
    Species = NA
  )
  return(predict(knn_model, test))
}

# Use beakr to expose the model in the "/predict-species" url path.
#   See help("decorate") for more info about decorating functions.
newBeakr() %>%
  httpPOST(path = "/predict-species", decorate(predict_species)) %>%
  handleErrors() %>%
  listen(host = "127.0.0.1", port = 25118)
```

You can interact with this webservice by sending an HTTP POST request to  
`http://127.0.0.1:25118/predict-species` with incoming data supplied as a JSON 
string containing sepal length and width (`sl`, `sw`) and petal length and width 
(`pl`, `pw`). The _Beakr_ instance responds with the predicted species of iris. 

```bash
$ curl -X POST http://127.0.0.1:25118/predict-species \
  -H 'content-type: application/json' \
  -d '{ "sl": 5.3, "sw": 4, "pl": 1.6, "pw": 0.2 }'
  
> setosa
```

### A state plotting webservice

We can use a built-in convenience function of a **beakr**'s _Response_ object to 
print and return a _ggplot_ object. Use `help("Response"")` to view other 
_Response_ object methods and documentation.  In this example we'll wrap some map 
generation code and serve it with a _Beakr_ instance.  Instead of decorating an 
existing package function, we will create a **beakr**-oriented function 
that uses a response object method to send back raw image bytes. Parameters in 
the URL request will be converted into arguments to the function.

```r
library(beakr)
library(ggplot2)

# Create a plot of a US state
state_plot <- function(state = NULL, res) {
  states <- ggplot2::map_data('state')

  if ( !is.null(state) ) {
    states <- subset(states, region == tolower(state))
  }

  plot <-
    ggplot(data = states) +
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") +
    coord_fixed(1.3) +
    guides(fill = FALSE)

  # Pass the plot to the beakrs response plot method
  res$plot(plot, base64 = FALSE, height = 800, width = 800)
}

# Create and start a default beakr instance
newBeakr() %>%
  httpGET(path = '/usa', decorate(state_plot)) %>%
  listen()
```

View a map of Washington state by visiting: http://127.0.0.1:8080/usa?state=washington.

### A custom webservice

Users can create custom functions that will be run when specific URLs are
accessed using specific HTTP methods. The following example provides a basic
outline for creating more complex webservices:

```r
library(beakr)
library(MazamaCoreUtils)

newBeakr() %>%

  # ----- Welcome --------------------------------------------------------------

  httpGET("/", function(req, res, err) {

    response <-
"
<html>
<body>
<h1>Welcome to repeater!</h1>
<p>URL paths look like <code>/repeater?text=...&amp;times=...&amp;responseType=...</code></p>
<p>The following <code>responseTypes</code> are supported:</p>
<ul>
<li><code>txt</code></li>
<li><code>json</code></li>
</ul>
</body>
</html>
"
    return(response)

  }) %>%

  # ----- Repeater -------------------------------------------------------------

  httpGET("/repeater", function(req, res, err) {

    text <- MazamaCoreUtils::setIfNull(req$parameters$text, "Howdy")
    times <- MazamaCoreUtils::setIfNull(req$parameters$times, 8)
    responseType <- MazamaCoreUtils::setIfNull(req$parameters$responseType, "txt")

    if ( times > 10 )
      stop("Parameter 'times' must be < 10")

    res$setContentType(mime::mimemap[responseType])

    if ( responseType == "txt" ) {

      response <- paste(rep(text, times), collapse = "\n")

    } else if ( responseType == "json" ) {

      responseList <- list(
        status = "success",
        output = paste(rep(text, times), collapse = "\n")
      )

      response <- jsonlite::toJSON(
        responseList,
        na = "null",
        pretty = TRUE,
        auto_unbox = TRUE
      )

    } else if ( responseType == "png" ) {

      pngFile <- tempfile(pattern = "repeater", fileext = "png")
      png(pngFile)
      plot(0:11,0:11, col = "transparent", axes = FALSE, xlab = "", ylab = "")
      for ( i in 1:times ) {
        text(1, 10 - i, text)
      }
      dev.off()
      response <- readr::read_file_raw(pngFile)

    } else {

      stop(paste0("responseType 'responseType' is not recognized"))

    }

    return(response)

  }) %>%

  # ----- Handle errors --------------------------------------------------------

  handleErrors() %>%

  # ----- Start Beakr ----------------------------------------------------------

  listen()
```

## Notes

Fundamentally, **beakr** is built on top of the **libuv** and **http-parser** C 
libraries as beakr relies heavily upon [httpuv](https://github.com/rstudio/httpuv),
a package that provides low-level socket and protocol support for handling HTTP 
and WebSocket requests directly from within R. Much of the development of the 
package was inspired by the excellent but no longer supported 
[jug](https://github.com/Bart6114/jug) package, developed by Bart Smeets.

---- 

The beakr package is supported by [Mazama Science](http://mazamascience.com/).

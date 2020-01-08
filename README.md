[![sm-beakr.png](https://i.postimg.cc/7YBB0Hnp/sm-beakr.png)](https://postimg.cc/bstHjjMT)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AirSensor)](https://cran.r-project.org/package=beakr)
[![Downloads](http://cranlogs.r-pkg.org/badges/AirSensor)](https://cran.r-project.org/package=beakr)
[![Build Status](https://travis-ci.org/MazamaScience/AirSensor.svg?branch=master)](https://travis-ci.org/MazamaScience/beakr)


### A Minimalist Web Framework for R

**beakr** is a minimalist web framework for developing web services in the R Language. **beakr** offers a robust set of fundamental web application features and is intended to simplify the development of web services that reflect R package APIs — without obscuring R's data processing capability and ease of use.

#### Hello, world! - The beakr way

```
library(beakr)
createBeakr() %>% 
  getr(path = "/", function(req, res, err) "Hello, World!") %>% 
  listen(host = "127.0.0.1", port = 1234) 
```
```
## Serving beakr instance at http://127.0.0.1:1234
## Beakr Instance: Human Room
##  State: Active | Host: 127.0.0.1 | Port: 1234 | Middlewares: 1
```

### Overview

The **beakr** package allows R code to listen for and interact with HTTP and WebSocket clients, so you can serve web traffic directly from a _beakr_ instance. The **beakr** package is intended to be simple, lightweight and unopinionated.  

While **beakr** is not recommended for building extensive web frameworks, R and the flexibility of the package are (potentially) up to the task. Keep in mind that **beakr** was not designed to be an especially performant web framework and the  _"batteries are certainly not included"_. If you're looking for full featured  web frameworks, there are better tools and languages for that (see [Shiny](https://shiny.rstudio.com), [django](https://www.djangoproject.com), _etc.). **beakr** is inspired by the minimalist and massively-expandable frameworks offered by [Express.js](https://expressjs.com) and [Flask](https://palletsprojects.com/p/flask/). 

One of the reasons to use **beakr** is that it is incredibly flexible. It allows you to integrate your R code as _middleware_ in a _beakr_ instance. Middleware functions can execute any R code, make changes to the _request_, _response_, and _error_ objects, and then serve up the response at the end the request-response cycle. The **beakr** package loosely follows Express.js middleware semantics, where middleware functions are functions that have access to the _request_, _response_, and _error_ objects of a _beakr_ instance.

_Note:_ By convention, the _response_, _request_, and _error_ objects are always referred to as `res`, `req`, `err`, respectively. See the package documentation for more information.

### Installation

Install the release version from CRAN:

```
install.packages("beakr")
```

Install the development version for GitHub: 

```
install.packages("devtools")
devtools::install_github("MazamaScience/beakr")
```

### Examples

#### 1. Expose R code 

A _beakr_ instance can easily expose R function signatures (APIs). As an example, let's expose a simple machine learning model using the [caret](https://github.com/topepo/caret) package and the Iris data set. The `predict_species()` function trains a KNN model for categorizing iris species and  predicts the species associated with incoming data. The _beakr_ instance exposes this API and, when given JSON input with the required arguments, identifies and returns the species.

```
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
    Sepal.Length = sl, 
    Sepal.Width = sw, 
    Petal.Length = pl, 
    Petal.Width = pw, 
    Species = NA
  )
                      
  return(predict(knn_model, test))
}

# Create the beakr instance 
beakr <- createBeakr()

# Use beakr to expose the model in the '/predict-species' url path. 
#   See help('decorate') for more info about decorating functions. 
beakr %>%  
  postr(path = '/predict-species', decorate(predict_species)) %>% 
  handleErrors() %>% 
  listen(host = '127.0.0.1', port = 1234)
```

The user can send an HTTP postr request to `http://127.0.0.1:1234/predict-species` with incoming data supplied as a JSON string containing sepal length and width (`sl`, `sw`) and petal length and width (`pl`, `pw`). The _beakr_ instance responds with the predicted species of iris. 

```
$ curl -X postr http://127.0.0.1:1234/predict-species \
  -H 'content-type: application/json' \
  -d '{ "sl": 5.3, "sw": 4, "pl": 1.6, "pw": 0.2 }'
  
> setosa
```

#### 2. Plotting

We can use a built-in convenience function of a **beakr**'s _response_ object to display a _ggplot_ object. Use `help('Response')` to view other _response_ object methods and documentation.  In this example we'll wrap some map generation code and serve it with a _beakr_ instance.  Instead of decorating an existing package function, we will create a **beakr**-oriented wrapper function that extracts parameters fromthe request object and uses a response object method to send back raw image bytes.

```
library(beakr)
library(ggplot2)

# Create a beakr-oriented wrapper
states_plot <- function(req, res, err) {
  # Extract stateName from the request object
  stateName <- req$parameters$stateName
  
  # Create the plot
  states <- ggplot2::map_data('state', stateName)
  gg <- ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
    coord_fixed(1.3) +
    guides(fill=FALSE) 
  
  # Pass the ggplot object to the response object plot() method 
  res$plot(gg, base64 = FALSE, height = 800, width = 800)
  
  return()
}

# Create and start a default beakr instance
createBeakr() %>% 
  getr(path = '/usa', states_plot) %>% 
  listen()

```

By visiting `http://127.0.0.1:8080/usa`, we can view a ggplot map of the United States.

### Notes

Fundamentally, **beakr** is built on top of the **libuv** and **http-parser** C libraries as beakr relies heavily upon [httpuv](https://github.com/rstudio/httpuv), an package that provides low-level socket and protocol support for handling HTTP and WebSocket requests directly from within R. Much of the development of the package was inspired by the excellent but no longer supported(?) [jug](https://github.com/Bart6114/jug) package, developed by Bart Smeets.

---- 

The beakr package is supported by [Mazama Science](http://mazamascience.com/).

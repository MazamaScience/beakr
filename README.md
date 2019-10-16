![beakr_logo](https://cdn3.imggmi.com/uploads/2019/10/16/f74987f8804a4512f70e0e8d17f05983-full.png)

### A Minimalist Web Framework for R. 
beakr is a minimalist web framework for developing APIs in the R Language. beakr offers
a robust set of fundamental web application features and is intended to simplify data 
science API development - without obscuring R's data processing capability and ease of use.

#### Hello, world! - The beakr way.
```
library(beakr)
newBeakr() %>% 
  GET(path = "/", function(req, res, err) "Hello, World!") %>% 
  startBeakr(host = "127.0.0.1", port = 1234) 
```
```
## Serving beakr instance at http://127.0.0.1:1234
## Beakr Instance: Human Room
##  State: Active | Host: 127.0.0.1 | Port: 1234 | Middlewares: 1
```

###  Overview
The beakr package allows R code to listen for and interact with HTTP and WebSocket clients, 
so you can serve web traffic directly out of an instance made with beakr. The beakr 
package is intended to be lightweight, unoppioniated, and simple.  

While beakr is not recommended for building extensive web frameworks, R and the flexibility 
of the package - in theory - could allow it. Keep in mind, it is not intended to be an 
especially performant web framework and the 'batteries' are certainly not included. If 
you're looking for full featured web frameworks, there are better tools and languages for 
that (see Shiny, Django, etc., etc.). beakr is inspired by the minimalist and
massively-expandable frameworks offered by Express and Flask. 

beakr is incredibly flexible. It provides the ability for integrating your R 
code as middleware in a beakr instance. Middleware functions can execute any 
R code, make changes to the request, response, and error objects, and end the 
request-response cycle. The beakr package loosely follows Express.js middleware 
semantics, where middleware functions are functions that have access to the request, 
response, and error objects of a beakr instance. In this documentation and by 
convention, the response, request, and error objects are always referred to as 
`res`, `req`, `err`, respectively. 

### Installation
Install the release version from CRAN:
```
# Not yet on CRAN network. 
install.packages("beakr")
```
Install the development version for GitHub: 
```
# install.packages("devtools")
devtools::install_github("MazamaScience/beakr")
```

### Examples

#### 1. Deploy a machine learning model
Let's use the [caret](https://github.com/topepo/caret) package and Iris data set
to train a simple model for predicting the species of iris, given a sepal length
& width, and petal length & width. We can expose this model with beakr.
```
# Import libraries 
library(beakr)
library(caret)

# Load the Iris data set 
data('iris')
# Train using KNN
knn_model <- train( Species ~ ., 
                    data = iris, 
                    method = 'knn', 
                    trControl = trainControl(method='cv', number=10), 
                    metric = 'Accuracy' )

# Function to predict the species using the trained model. 
predict_species <- function(sl, sw, pl, pw) {
  test <- data.frame( Sepal.Length = sl, 
                      Sepal.Width = sw, 
                      Petal.Length = pl, 
                      Petal.Width = pw, 
                      Species = NA )
                      
  return(predict(knn_model, test))
}

# Create the beakr instance 
beakr <- newBeakr()

# Use beakr to expose the model in the '/predict-species' url path. 
#   See help('decorate') for more info about decorating functions. 
beakr %>%  
  POST(path = '/predict-species', decorate(predict_species)) %>% 
  errorHandler() %>% 
  startBeakr(host = '127.0.0.1', port = 1234)
```

By sending an HTTP POST request to `http://127.0.0.1:1234/predict-species`, the 
beakr instance will return a predicted species of iris. 
We can use JSON and beakr will parse the parameters. In this case, we will 
supply a sepal length & width (`sl`, `sw`) and petal length & width (`pl`, `pw`).
```
$ curl -X POST http://127.0.0.1:1234/predict-species \
  -H 'content-type: application/json' \
  -d '{ "sl": 5.3, "sw": 4, "pl": 1.6, "pw": 0.2 }'
  
> setosa
```

#### 2. Plotting
We can use a built-in convience function of a beakr's response object to display 
an image of a static plot. In this example we'll make a map of the United States 
and serve it with beakr. Use `help('Response')` to view other response object 
methods and documentation.

```
library(beakr)
library(ggplot2)

# Create a plot of the US States
states_plot <- function(res) {
  states <- ggplot2::map_data('state')

  plot <- ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
    coord_fixed(1.3) +
    guides(fill=FALSE) 
  
  # Pass the plot to the beakrs response plot method 
  res$plot(plot, base64 = FALSE, height = 800, width = 800)
}

# Create and start a default beakr instance
newBeakr() %>% 
  GET(path = '/usa', decorate(states_plot)) %>% 
  startBeakr()

```
By visiting `http://127.0.0.1:8080/usa`, we can view a ggplot of the United States.

Note: By convention, the response, request, and error objects are always 
referred to as `res`, `req`, `err`, respectively, but its actual name is 
determined by the parameters to the callback function in which you’re working with. 


See the package documentation for more information.

### Notes
Fundamentally, beakr is built on top of the libuv and http-parser C libraries as 
beakr relies heavily upon the [httpuv](https://github.com/rstudio/httpuv), a package 
that provides low-level socket and protocol support for handling HTTP and WebSocket 
requests directly from within R. beakr and much of the development of the package 
was inspired by the excellent and no longer supported 
[jug](https://github.com/Bart6114/jug) package, developed by Bart Smeets.

The beakr package was developed by [Hans Martin](https://github.com/hansmrtn) 
and [Jonathan Callahan](https://github.com/jonathancallahan), and is supported by 
[Mazama Science](http://mazamascience.com/).





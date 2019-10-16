![beakr_logo](/docs/beakr_logo.png)
### A Minimalist Web Framework for R. 
beakr is a minimalist web framework for developing APIs in the R Language.
beakr offers a robust set of fundemental web application features and is 
intended to simplify data science API development - without obscurring 
R's data processing capability and ease of use. 

#### Hello, world! - The beakr way.
```
library(beakr)
newBeakr() %>% 
  GET(path = "/", function(req, res, err) "Hello, World!") %>% 
  startBeakr(host = "127.0.0.1", port = 1234) 
```

###  Overview
beakr allows R code to listen for and interact with HTTP and WebSocket clients, 
so you can serve web traffic directly out of a beakr instance. The beakr 
package is intended to be lightweight, unoppioniated, and simple.  

beakr provides foundational functionality for handling common HTTP-requests, 
errors, and logging. While beakr is not recommended for building extensive web 
frameworks, the flexibility of beakr could, in theory, allow it. beakr is not 
intended to be an especially preformant web framework and the 'batteries' are 
not included (see Shiny, Django, etc., etc.).

beakr is incredibly flexible. It provides the ability for integrating
your R code as middleware in a beakr instance. Middleware functions
can execute any R code, make changes to the request, response, and error objects, and 
end the request-response cycle. The beakr package loosely follows
[Express.js](http://expressjs.com/) middleware semantics. beakr middleware functions 
are functions that have access to the request object (`req`), the response 
object (`res`), and error (`err`) object of a beakr instance. 

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

### Example

#### Deploy a machine learning model
Let's use the [caret](https://github.com/topepo/caret) package and Iris data set
to train a simple model for predicting the species of iris, given a sepal length
& width, and petal length & width. 
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
See the package documentation for more information.

### Notes
Fundementally, beakr is built on top of the `libuv` and `http-parser` C libraries 
as beakr relies heavily upon the [httpuv](https://github.com/rstudio/httpuv), 
a package that provides low-level socket and protocol support for handling HTTP 
and WebSocket requests directly from within R. beakr and much of the development
of the package was inpsired by the excellent (now deprecated) [jug](https://github.com/Bart6114/jug) 
package, developed by Bart Smeets. 

The beakr package was developed by [Hans Martin](https://github.com/hansmrtn) 
and [Jonathan Callahan](https://github.com/jonathancallahan), and is supported by 
[Mazama Science](http://mazamascience.com/).





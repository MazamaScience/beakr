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

The beakr package includes functionality for handling common
HTTP-requests, custom middleware, errors, and logging. 

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

### Notes
Fundementally, beakr is built on top of the `libuv` and `http-parser` C libraries 
as beakr relies heavily upon the [httpuv](https://github.com/rstudio/httpuv), 
a package that provides low-level socket and protocol support for handling HTTP 
and WebSocket requests directly from within R. The development of beakr and much of the packages functionality
was inpsired by the excellent (deprecated) [jug](https://github.com/Bart6114/jug) 
package, developed by Bart Smeets. 

The beakr package was developed by [Hans Martin](https://github.com/hansmrtn) and [Jonathan Callahan](https://github.com/jonathancallahan), and is supported by 
[Mazama Science](http://mazamascience.com/).





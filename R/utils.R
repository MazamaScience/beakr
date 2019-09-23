#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Parse the parameters passed by the request
#'
#' @param request
#' @param body
#' @param query
#' @param type
#'
#' @return parsed parameters list
#' @export
#'
parseParameters <-
  function(request, body, query, type) {
    parameters <- list()
    parameters <- c(parameters, webutils::parse_query(query))

    if ( is.null(type) ) {
      return(parameters)
    }

    if ( grepl("json", type) && nchar(body) > 0 ) {
      parameters <- c( parameters,
                       jsonlite::fromJSON(body, simplifyDataFrame = FALSE) )
    }

    if ( grepl("multipart", type) ) {
      parameters <- c( parameters,
                       mime::parse_multipart(request) )
    }

    if ( grepl("form-urlencoded", type) ) {
      parameters <- c( parameters,
                       webutils::parse_query(body) )
    }

    return(parameters)

  }

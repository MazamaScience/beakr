#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @keywords internal
#' @title Regex path query
#'
#' @param pattern the string pattern to parse
#' @param path the path to match to
#' @param ... additional parameters
.matchPath <- function(pattern, path, ...) {
  # Result init
  result <- list(match = FALSE, src = path, parameters = list())

  if ( !is.null(pattern) ) {
    if ( !(grepl("^\\^", pattern) ||
           grepl("\\$$", pattern)) ) {
      pattern <- paste0("^", pattern, "$")
    }

    rex <- regexpr(pattern, path, perl = TRUE, ...)

    for ( n in attr(x = rex, which = "capture.name") ) {
      result$parameters[[n]] <- substr( x     = result$src,
                                        start = attr(rex, "capture.start")[,n],
                                        stop  = (attr(rex, "capture.start")[,n] +
                                                   attr(rex, "capture.length")[,n] - 1) )

    }
    result$match <- ifelse(rex[[1]] > -1, TRUE, FALSE)
  } else {
    result$match <- TRUE
  }

  return(result)
}

#' @keywords internal
#' @title Parse the parameters passed by the req
#'
#' @param req an HTTP req
#' @param body a body text string
#' @param query a url-encoded query string
#' @param type a media mime type (a.k.a. Multipurpose Internet Mail Extensions
#' or MIME type).
.parseParameters <- function(req, body, query, type) {
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
                     mime::parse_multipart(req) )
  }

  if ( grepl("form-urlencoded", type) ) {
    parameters <- c( parameters,
                     webutils::parse_query(body) )
  }
  return(parameters)
}

#' @keywords internal
#' @title Internal function to add middleware
#'
#' @param beakr a beakr instance.
#' @param FUN the function to route middleware
#' @param path the path to route the middleware
#' @param method the HTTP method to employ
#' @param websocket boolean, TRUE if websocket.
.routeMiddleware <- function(
  beakr,
  FUN,
  path = NULL,
  method = NULL,
  websocket = FALSE
) {

  if ( is.null(beakr) ) {
    beakr <- invisible(App$new())
  }

  if ( !is.null(method) ) {
    method <- toupper(method)
  } else {
    method <- NULL
  }

  # Create new middleware
  mw <- Middleware$new(FUN = FUN, path, method, websocket)

  # Add the middleware
  beakr$router$addMiddleware(mw)
  return(beakr)

}

#' @keywords internal
#' @title Internal function to add listeners
#'
#' @param beakr a beakr instance
#' @param event an event (\emph{"start", "finish", "error"})
#' @param FUN middleware function.
.addListener <- function(beakr, event, FUN){
  l <- Listener$new(event = event, FUN)
  beakr$router$addListener(l)
  return(beakr)
}

#' @keywords internal
#' @title Internal random name generator.
.randomName <-  function() {
  # f U n  n a M e S!
  dic <- c("Fear", "Frontier", "Nanny", "Job", "Yard", "Airport", "Pint",
             "Commando", "Basketball", "Bachelorette", "Diva",
             "Baggage", "College", "Octane", "Clean", "Sister", "Army", "Drama",
             "Backyard", "Pirate", "Shark", "Project", "Model", "Survival",
             "Justice", "Jersey", "Ax", "Warrior", "Ancient",
             "Pawn", "Throttle", "Knight", "Mazama",
             "Outback", "Celebrity", "Air", "Restaurant", "Bachelor", "Family",
             "Royal", "Surf", "Ulitmate", "Date", "Operation", "Tank",
             "Logging", "Hollywood", "Amateur", "Craft", "Mystery", "Dog",
             "Human", "Rock", "Ice", "Shipping", "Modern", "Crocodile",
             "Farm", "Tool", "Boot", "Pioneer", "Kid", "Action", "Bounty",
             "Paradise", "Mega", "Love", "Style", "Teen", "Pop", "Treasure",
             "Myth", "Empire", "Motorway", "Room", "Casino", "Comedy",
             "Undercover", "Millionaire", "Chopper", "Space", "Cajun", "The",
             "Colonial", "Dance", "Flying",
             "Mountain", "Auction", "Extreme", "Whale", "Storage", "Cake",
             "Turf", "UFO", "Real", "Wild", "Duck", "Queer", "Voice",
             "Fame", "Music", "BBQ", "Spouse", "Wife", "Road",
             "Star", "Renovation", "Comic", "Chef", "Band", "House", "Sweet",
             "Hunters", "Hoarders", "Contest", "Party", "Stars", "Truckers",
              "Camp", "Crew", "Casting", "Inventor", "Search",
              "Pitmasters", "Blitz", "Marvels", "Wedding", "Crew", "Men",
              "Project", "Intervention", "Celebrities", "Treasure", "Master",
              "Days", "Wishes", "Sweets", "Haul", "Hour", "Mania", "Warrior",
              "Wrangler", "Restoration", "Factor", "Love",
              "Inventors", "Kitchen", "Casino", "Queens", "Academy",
              "Superhero", "Battles", "Behavior", "Rules", "Justice",
              "Date", "Discoveries", "Club", "Brother", "Showdown",
              "Disasters", "Attack", "Contender", "People", "Raiders",
              "Story", "Patrol", "House", "Gypsies", "Challenge", "School",
              "Aliens", "Towers", "Brawlers", "Garage", "Whisperer",
              "Supermodel", "Boss", "Secrets", "Apprentice", "Icon",
              "Party", "Pickers", "Crashers", "Nation", "Files",
              "Office", "Wars", "Rescue", "VIP", "Fighter", "Job", "Experiment",
              "Quest", "Eats", "Moms", "Idol", "Consignment", "Life", "Dynasty",
              "Diners", "Chef", "Makeover", "Ninja", "Show", "Dancing",
              "Greenlight", "Mates", "Wives", "Jail", "Model", "Ship",
              "Family", "Videos", "Repo", "Rivals", "Room", "Dad", "Star",
              "Island", "Neighbor", "Missions", "Kings", "Loser", "Shore",
              "Assistant", "Comedians", "Rooms", "Enigma")

  return(paste(sample(dic, 2, replace = FALSE), collapse = " "))
}


# Internal, unexported functions

# ----- Beakr object modifiers -------------------------------------------------

#' @keywords internal
#' @title Internal function to add middleware to a Beakr object
#'
#' @description This function is used in each of the \code{http_~()} methods
#' as well as the \code{cors()}, \code{websocket()} and \code{use()} utility
#' functions.
#'
#' @param beakr Beakr instance.
#' @param FUN Function to route middleware.
#' @param path Path to route the middleware.
#' @param method HTTP method to employ.
#' @param websocket Boolean, TRUE if websocket.
#'
#' @return A \code{Beakr} object with added middleware.

.routeMiddleware <- function(
  beakr,
  FUN,
  path = NULL,
  method = NULL,
  websocket = FALSE
) {

  if ( is.null(beakr) ) {
    beakr <- invisible(Beakr$new())
  }

  if ( !is.null(method) ) {
    method <- toupper(method)
  } else {
    method <- NULL
  }

  # Create new middleware
  mw <- Middleware$new( FUN = FUN,
                        path = path,
                        method = method,
                        websocket = websocket )

  # Add the middleware
  beakr$router$addMiddleware(mw)

  return(beakr)

}

# ----- Request object internals -----------------------------------------------

#' @keywords internal
#' @title Parse the parameters passed by in the request
#'
#' @description Internal function used in the \code{Request$initialize()}
#' method to extract URL parameters from the request path.
#'
#' @param req HTTP request object.
#' @param body Body text string.
#' @param query Url-encoded query string.
#' @param type Media mime type.
#'
#' @return A list of parameters and values.

.parseParameters <- function(req, body, query, type) {

  # Initialize result
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

# ----- Router object internals ------------------------------------------------

#' @keywords internal
#' @title Regex path query
#'
#' @description This function is used in the \code{Router$invoke()}
#' method to match middleware paths to request paths.
#'
#' @param pattern String pattern to parse. (A middleware path.)
#' @param path Path to match to. (A request path.)
#' @param ... Additional parameters passed to \code{regexpr()}.
#'
#' @return A List with information on matching paths and URL parameters.

.matchPath <- function(pattern, path, ...) {

  # Initialize result
  result <- list(match = FALSE, src = path, parameters = list())

  if ( !is.null(pattern) ) {
    if ( !(grepl("^\\^", pattern) ||
           grepl("\\$$", pattern)) ) {
      pattern <- paste0("^", pattern, "$")
    }

    rex <- base::regexpr(pattern, path, perl = TRUE, ...)

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

# ----- Response functions -----------------------------------------------------

#' @keywords internal
#' @title JSON error function
#'
#' @description This function is used to add a JSON error response to the
#' \code{res} object. It is called by the \code{handleErrors()} utility
#' function.
#'
#' @return The incoming \code{res} object is modified.

.jsonError <- function(req, res, err) {

  res$setContentType("application/json")

  if ( err$occurred ) {
    res$status <- 500L
    error_str <- paste(err$errors, collapse = "\n")

    cat("ERROR:\n", error_str, "\n")

    res$json(list( status = "error",
                   status_code = 500L,
                   errors = error_str ))

  } else {
    res$status = 404L
    res$json(list( status = "Page not found.",
                   status_code = 404L ))
  }

}

# ----- Helper functions -------------------------------------------------------

#' @keywords internal
#' @title Internal random name generator
#'
#' @description Every instantiated \code{Beakr} object is assigned a name generated
#' with this function. This makes it easy to keep track of multiple instances
#' and stop/kill only certain ones. "Hollywood Diva" is unlikely to be confused
#' with "Cajun Bachelor".
#'
#' @return An identifying text string.

.randomName <- function() {

  # f U n  n a M e S!
  dictionary <- c(
    "Fear", "Frontier", "Nanny", "Job", "Yard", "Airport", "Pint",
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
    "Assistant", "Comedians", "Rooms", "Enigma"
  )

  return(paste(sample(dictionary, 2, replace = FALSE), collapse = " "))

}


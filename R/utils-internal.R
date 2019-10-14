# #' Internal function to add listeners
# #'
# #' Internal function.
# #'
# #' @param beakr a beakr instance.
# #' @param FUN listener function
# #' @param event the event to listen for, such as "start" or "finish".
# addListener <- function(beakr, FUN, event) {
#   mw <- Listener$new(FUN, event)
#   beakr$router$addListener(mw)
#   return(beakr)
# }

#' @keywords internal
#' @title Internal function to add middleware
#'
#' @param beakr a beakr instance.
#' @param FUN the function to route middleware
#' @param path the path to rouet the middleware
#' @param method the HTTP method to employ
#' @param websocket boolean, TRUE if websocket.
routeMiddleware <- function(
  beakr,
  FUN,
  path = NULL,
  method = NULL,
  websocket = FALSE
) {

  # if ( is.null(beakr) ) {
  #   beakr <- invisible(Beakr$new())
  # }

  if ( !is.null(method) ) {
    method <- toupper(method)
  } else {
    method <- NULL
  }

  # Create new middleware
  mw <- Middleware$new(FUN, path, method, websocket)

  # Add the middleware
  beakr$router$addMiddleware(mw)
  return(beakr)

}

#' @keywords internal
#' @title Internal function to add listeners
#'
#' @param beakr
#' @param event
#' @param FUN
addListener <- function(beakr, event, FUN){
  mw <- Listener$new(event = event, FUN = FUN)
  beakr$router$addListener(mw)
  return(beakr)
}

#' @keywords internal
#'
#' @title Internal random name generator.
randomName <-  function() {
  first <- c("Fear", "Frontier", "Nanny", "Job", "Yard", "Airport", "Pint",
             "Commando", "Basketball", "Bachelorette", "Diva",
             "Baggage", "College", "Octane", "Clean", "Sister", "Army", "Drama",
             "Backyard", "Pirate", "Shark", "Project", "Model", "Survival",
             "Justice", "Jersey", "Ax", "Warrior", "Ancient",
             "Pawn", "Throttle", "Knight", "Mazama",
             "Outback", "Celebrity", "Air", "Restaurant", "Bachelor", "Family",
             "Royal", "Surf", "Ulitmate", "Date", "Operation", "Tank",
             "Logging", "Hollywood", "Amateur", "Craft", "Mystery", "Dog",
             "Human", "Rock", "Ice Road", "Shipping", "Modern", "Crocodile",
             "Farm", "Tool", "Boot Camp", "Pioneer", "Kid", "Action", "Bounty",
             "Paradise", "Mega", "Love", "Style", "Teen", "Pop", "Treasure",
             "Myth", "Empire", "Motorway", "Room", "Casino", "Comedy",
             "Undercover", "Millionaire", "Chopper", "Space", "Cajun", "The",
             "Colonial", "Dance", "Flying",
             "Mountain", "Auction", "Extreme", "Whale", "Storage", "Cake",
             "Turf", "UFO", "Real", "Wild", "Duck", "Queer", "Voice",
             "Fame", "Music", "BBQ", "Spouse", "Wife", "Road",
             "Star", "Renovation", "Comic", "Chef", "Band", "House", "Sweet")
  second <- c("Hunters", "Hoarders", "Contest", "Party", "Stars", "Truckers",
              "Camp", "Dance Crew", "Casting Call", "Inventor", "Search",
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
              "Island", "Next Door", "Missions", "Kings", "Loser", "Shore",
              "Assistant", "Comedians", "Rooms", "Enigma")

  rname <- paste0( sample(sample(first, round(runif(1, 1, 25))), 1),
                  sample(sample(second, round(runif(1, 1, 25))), 1),
                  collapse = " " )
  return(rname)
}


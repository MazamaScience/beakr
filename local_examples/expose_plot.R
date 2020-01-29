library(beakr)
library(ggplot2)

# Create a plot of the US States
states_plot <- function(res) {
  states <- ggplot2::map_data('state')

  plot <- ggplot(data = states) +
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") +
    coord_fixed(1.3) +
    guides(fill = FALSE)

  # Pass the plot to the beakrs response plot method
  res$plot(plot, base64 = FALSE, height = 800, width = 800)
}

# NOTE:  safe port numbers above 1023
# > which(letters %in% c("b","e","a","k","r")) %% 10
# [1] 1 2 5 1 8

# Create and start a default beakr instance

beakr <- newBeakr() # Assign it so we can stopServer it later

beakr %>%
  httpGET(path = '/usa', decorate(states_plot)) %>%
  listen(host = '127.0.0.1', port = 25118, daemon = TRUE)



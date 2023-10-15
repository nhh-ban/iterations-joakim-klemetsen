# Function creating a pretty plot
plotTraffic <- function(data, name) {
  data %>% 
    ggplot(aes(x = from, y = volume)) +
    geom_line() +
    labs(
      title = paste("Hourly Traffic Volume for station:", name),
      x = "From",
      y = "Volume"
    ) + 
    theme_classic()
}

theme_bw_map <- function() {
  theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      )
}

theme_density <- function() {
  theme_bw() +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
    )
}

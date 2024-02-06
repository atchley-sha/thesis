plot_tlfd <- function(trips, group_cols = NULL, pivot_cols = NULL, grid = NULL) {
  
  t <- trips %>% 
    make_mode_and_purpose_pretty() %>% 
    select(any_of(group_cols), any_of(pivot_cols))
    
  if(!is.null(pivot_cols)) t <- t %>% pivot_longer(all_of(pivot_cols))
  
  p <- t %>% 
    ggplot() +
    geom_density(aes(x = distance, color = name, weight = value))
  
  if(!is.null(grid)) p <- p + facet_grid(rows = vars(.data[[grid$rows]]), cols = vars(.data[[grid$cols]]))
  
  p
}

plot_trip_diff_by_purpose <- function(trip_diff) {
  trip_diff %>% 
    make_mode_and_purpose_pretty() %>% 
    group_by(o_TAZ, purpose) %>% 
    summarise(diff = sum(trips_difference)) %>% 
    ggplot(aes(x = diff)) +
    geom_bar(aes(x = diff)) +
    scale_x_continuous(
      breaks = -10:10, limits = c(-10.5,10.5), expand = c(0,0)) +
    scale_y_continuous(
      expand = expansion(c(0,0.05))) +
    # labs(
    #   x = "Increase in trips by origin TAZ, WFH scenario compared to baseline",
    #   y = "Number of TAZs") +
    theme_bw() +
    facet_wrap(vars(purpose), nrow = 1)
}

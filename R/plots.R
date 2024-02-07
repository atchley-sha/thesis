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

plot_wfh_trip_diff_by_purpose <- function(trip_diff) {
  summ <- trip_diff %>%
    make_mode_and_purpose_pretty() %>%
    group_by(purpose) %>%
    summarise(diff = sum(trips_difference)) %>%
    mutate(diff = paste0("\u03A3 =\n", diff))

  trip_diff %>%
    make_mode_and_purpose_pretty() %>%
    group_by(o_TAZ, purpose) %>%
    summarise(diff = sum(trips_difference)) %>%
    ggplot() +
    geom_bar(aes(x = diff, fill = as.factor(abs(diff))), show.legend = FALSE) +
    geom_text(aes(label = diff), x = -10, y = 650, data = summ) +
    scale_x_continuous(
      limits = c(-15.5,15.5), expand = c(0,0)) +
    scale_y_continuous(
      expand = expansion(c(0,0.05))) +
    scale_fill_manual(values = colorRampPalette(brewer_pal(palette = "Paired")(12))(16)) +
    # scale_fill_brewer(palette = "Paired") +
    labs(
      x = "Increase in trips by origin TAZ, Increased WFH scenario compared to baseline",
      y = "Number of TAZs") +
    theme_bw() +
    facet_wrap(vars(purpose), nrow = 1)
}

plot_wfh_vs_by_mode <- function(trips){
  trips %>%
    make_mode_and_purpose_pretty()

}

plot_frontrunner <- function(line, stops) {
  format_line <- line %>%
    mutate(Year = factor(Year, levels = c("2019", "2050")))
  format_stops <- stops %>%
    mutate(Year = factor(Year, levels = c("2019", "2050")))
  text_stops <- format_stops %>%
    bind_cols(st_coordinates(.) %>% as_tibble())

  ggplot() +
    annotation_map_tile("cartolight", zoom = 10) +
    geom_sf(aes(color = Year), linewidth = 1, data = format_line) +
    geom_sf(aes(color = Year), size = 3, data = format_stops) +
    geom_label_repel(
      aes(label = Name, geometry = geometry, fill = Year),
      box.padding = 0.4, point.padding = 0.5,
      nudge_x = 0.04, show.legend = FALSE,
      stat = "sf_coordinates", data = format_stops) +
    coord_sf(xlim = c(-112.3, -111.4), crs = st_crs(4326)) +
    scale_fill_manual(values = c("2019" = "white", "2050" = "grey90")) +
    scale_color_manual(values = c("2019" = "purple4", "2050" = "coral"), labels = c("2019" = "Existing", "2050" = "Improved Transit (addt'l)")) +
    theme_bw_map() +
    labs(color = "Scenario")

}

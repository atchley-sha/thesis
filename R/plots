
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


compare_tc <- function(wfrc, asim, trans) {
  left_join(wfrc, asim) %>% 
    left_join(trans) %>% 
    relocate(name) %>% 
    select(-jobcode)
}

plot_wfh_diff_tlfd <- function(trips = list()) {
  trips <- bind_rows(trips, .id = "model")
  
  trips %>%
    make_mode_and_purpose_pretty() %>% 
    make_model_pretty() %>% 
    mutate(fewer_trips = -trips_difference) %>%
    ggplot() +
    geom_density(aes(x = distance, weight = fewer_trips, color = model)) +
    coord_cartesian(xlim = c(0,25)) +
    scale_y_continuous(expand = expansion(c(0,0.05))) +
    facet_wrap(vars(mode), nrow = 3, scales = "free_y") +
    labs(
      x = "Distance (mi.)",
      y = "Trip Frequency (kernel density)",
      color = "Model"
      ) +
    theme_bw()
}

plot_wfh_vs_by_tlfd <- function(trips) {
  trips %>%
    make_mode_and_purpose_pretty() %>% 
    ggplot() +
    geom_density(aes(x = distance, weight = trips, color = scenario)) +
    coord_cartesian(xlim = c(0,25)) +
    scale_y_continuous(expand = expansion(c(0,0.05))) +
    facet_wrap(vars(mode), nrow = 3, scales = "free_y") +
    labs(
      x = "Distance (mi.)",
      y = "Trip Frequency (kernel density)",
      color = "Scenario"
    ) +
    theme_bw()
}

make_all_asim_tlfd_trips <- function(by, wfh, dist) {
   by %>% 
    filter(purpose == "hbw") %>% 
    full_join(select(wfh, -distance), join_by(o_TAZ, d_TAZ, o_DIST, d_DIST, purpose, mode)) %>%
    mutate(
      o_TAZ, d_TAZ, mode, purpose,
      by = trips, wfh_diff = -trips_difference,
      .keep = "none") %>% 
    left_join(distances, join_by(o_TAZ == origin, d_TAZ == destination)) %>% 
    pivot_longer(c(by, wfh_diff), names_to = "scenario", values_to = "trips") %>% 
    filter(!is.na(trips))
}

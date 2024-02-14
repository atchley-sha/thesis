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
    mutate(purpose = "hbw") %>%
    make_mode_and_purpose_pretty() %>%
    ggplot() +
    geom_density(aes(x = distance, weight = trips, color = scenario, lty = scenario)) +
    coord_cartesian(xlim = c(0,25)) +
    scale_y_continuous(expand = expansion(c(0,0.05))) +
    facet_wrap(vars(mode), nrow = 3, scales = "free_y") +
    labs(
      x = "Trip Length (mi.)",
      y = "Trip Frequency (kernel density)",
      color = "Scenario"
    ) +
    scale_color_brewer(
      labels = c(
        by = "Baseline",
        wfh = "Increased WFH"),
      palette = "Paired") +
    theme_bw()
}

make_all_asim_tlfd_trips <- function(by, wfh, distances) {
   by %>%
    filter(purpose == "hbw") %>%
    full_join(select(wfh, -distance), join_by(o_TAZ, d_TAZ, o_DIST, d_DIST, purpose, mode)) %>%
    mutate(
      o_TAZ, d_TAZ, mode, purpose,
      by = trips, wfh = trips_scenario, #wfh_diff = -trips_difference,
      .keep = "none") %>%
    left_join(distances, join_by(o_TAZ == origin, d_TAZ == destination)) %>%
    pivot_longer(c(by, wfh), names_to = "scenario", values_to = "trips") %>%
    filter(!is.na(trips))
}

make_all_wfrc_tlfd_trips <- function(by, wfh, distances) {
  by %>%
    full_join(wfh, join_by(origin, destination, mode), suffix = c("_by", "_wfh")) %>%
    left_join(distances, join_by(origin, destination)) %>%
    pivot_longer(c(trips_by, trips_wfh), names_prefix = "trips_", names_to = "scenario", values_to = "trips") %>%
    filter(trips > 0)
}

summarise_trip_diff <- function(trips) {
  summ <- trips %>%
    pivot_longer(c(trips_scenario, trips_reference), names_to = "scenario", values_to = "trips", names_prefix = "trips_") %>%
    select(-any_of(c("trips_difference"))) %>%
    mutate(miles = trips*distance) %>%
    group_by(purpose, mode, scenario) %>%
    summarise(
      trips = sum(trips),
      pmt = sum(miles, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(trips, pmt)) %>%
    pivot_wider(names_from = "scenario") %>%
    mutate(
      diff = scenario - reference,
      diff_pct = diff / reference
      ) %>%
    filter(mode == "auto") %>%
    group_by(name, purpose) %>%
    summarise(
      diff = sum(diff),
      diff_pct = sum(diff) / sum(reference)
    ) %>%
    pivot_wider(values_from = c("diff", "diff_pct"))

  summ
}

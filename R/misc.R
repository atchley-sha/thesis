pivot_tbm_edges <- function(edges) {

  read_csv(edges) %>%
    mutate(across(where(is.numeric), ~ ifelse(.x < 0, 0, .x))) %>%
    pivot_longer(-c(from, to), names_to = "mode", values_to = "number")

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
      color = "Scenario",
      lty = "Scenario"
    ) +
    scale_color_brewer(
      labels = c(
        by = "Baseline",
        wfh = "Increased WFH"),
      palette = "Paired") +
    scale_linetype_manual(
      values = c(
        by = "solid",
        wfh = "dotted"
      ),
      labels = c(
        by = "Baseline",
        wfh = "Increased WFH")
    ) +
    theme_bw()
}

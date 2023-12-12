compare_lu_vmt_plot <- function(df) {
  df %>% 
    ggplot(aes(y = primary_purpose, x = dist, fill = trip_mode)) +
    geom_col(position = position_dodge(preserve = "single")) +
    facet_wrap(
      vars(scenario),
      ncol = 1,
      labeller = labeller(
        scenario = c(base = "Base Scenario", land_use = "New Land Use")
      ),
      strip.position = "left",
      scales = "free_y") +
    labs(
      x = "Total Distance Traveled (mi.)",
      y = "Tour Purpose",
      fill = "Mode") +
    scale_fill_discrete(
      labels = c(auto = "Auto", transit = "Transit", nonmotor = "Non-motorized")) +
    scale_y_discrete(
      labels = \(x) str_to_title(x)) +
    scale_x_continuous(
      trans = sqrt_trans(),
      breaks = c(0, 10^3, 10^4, 5*10^4, 10^5, 2*10^5),
      labels = label_comma(),
      expand = expansion(c(0,0.05))) +
    theme_bw() +
    theme(
      strip.placement = "outside",
      panel.spacing = unit(0, "lines"),
      legend.position = "bottom"
      )
}
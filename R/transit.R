compare_transit_trips <- function(by_trp, tr_trp) {
  by_trip <- by_trp %>% 
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      primary_purpose = reduce_asim_purposes(primary_purpose)) %>% 
    select(trip_id, person_id, primary_purpose, trip_mode)
  
  tr_trip <- tr_trp %>% 
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      primary_purpose = reduce_asim_purposes(primary_purpose)) %>% 
    select(trip_id, person_id, primary_purpose, trip_mode)
  
  temp <- bind_rows(base = by_trip, transit = tr_trip, .id = "scenario") %>% 
    pivot_wider(names_from = "scenario", values_from = "trip_mode") %>% 
    filter(base != transit) %>%
    count(primary_purpose, base, transit)
  
  temp %>% 
    mutate(
      across(
        c(base, transit),
        \(x) if_else(x == "nonmotor", "Non-motorized", str_to_title(x))),
      primary_purpose = case_match(
        primary_purpose,
        c("shopping", "eatout", "escort", "social", "other") ~ "other",
        .default = primary_purpose
      ),
      primary_purpose = str_to_title(primary_purpose) %>% 
        fct_relevel("Work", "Atwork", "School", "Other") %>% 
        fct_relabel(\(x) paste("Purpose:", x))
      ) %>% 
    ggplot(aes(axis1 = base, axis2 = transit, y = n)) +
    geom_alluvium(aes(fill = transit)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    facet_wrap(vars(primary_purpose), scales = "free") +
    scale_x_continuous(breaks = c(1, 2), labels = c("Base", "Improved Transit")) +
    scale_y_continuous(expand = c(0,0)) +
    # scale_fill_discrete(na.value = "white") +
    labs(x = "Scenario", fill = "Switched to:", y = "Trips") +
    theme_minimal()

}

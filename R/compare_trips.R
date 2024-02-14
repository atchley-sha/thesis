#' @param od A named list of OD trip counts (long format)
combine_wfrc_od <- function(od, ex_zones){

  trips_mode_purpose <- imap(
    od, \(df, i) rename_with(
      df,
      \(x) paste(x, i, sep = "_"),
      .cols = -c(origin, destination)
    )) %>%
    reduce(function(x,y) left_join(x, y, join_by(origin, destination))) %>%
    filter(!origin %in% ex_zones, !destination %in% ex_zones) %>%
    filter(if_any(-c(origin, destination), \(x) x != 0)) %>%
    pivot_longer(
      -c(origin, destination),
      names_to = c("mode", "purpose"),
      names_sep = "_",
      values_to = "trips") %>%
    filter(trips > 0)

  # There is a lot of data here, so we're trying to do this in the least
  # memory-intensive way

  purpose_all <- trips_mode_purpose %>%
    group_by(origin, destination, mode) %>%
    summarise(trips = sum(trips)) %>%
    mutate(purpose = "all")

  trips <- bind_rows(trips_mode_purpose, purpose_all) %>%
    filter(trips > 0)

  rm(purpose_all)
  gc()

  mode_all <- trips_mode_purpose %>%
    group_by(origin, destination, purpose) %>%
    summarise(trips = sum(trips)) %>%
    mutate(mode = "all")

  trips <- bind_rows(trips, mode_all) %>%
    filter(trips > 0)

  rm(mode_all)
  gc()

  all_trips <- trips_mode_purpose %>%
    group_by(origin, destination) %>%
    summarise(trips = sum(trips)) %>%
    mutate(mode = "all", purpose = "all")

  trips <- bind_rows(trips, all_trips) %>%
    filter(trips > 0)

  rm(all_trips)
  rm(trips_mode_purpose)
  gc()


  trips %>%
    #WFRC multiplies the trips by 100 to avoid rounding away small trip numbers
    #that add up. We don't need that here, because R can handle more than 2
    #decimal places.
    mutate(trips = trips/100) %>%
    mutate(model = "wfrc", .after = destination)

}

get_asim_od <- function(tripsfile, toursfile, ex_zones){

  trips <- read_csv(tripsfile) %>%
    select(tour_id, origin, destination, primary_purpose, purpose, trip_mode) %>%
    rename(tour_purpose = primary_purpose, trip_purpose = purpose)
  # tours <- read_csv(toursfile) %>%
  #   select(tour_id, tour_mode)

  trips_mode_purpose <- trips %>%
    # left_join(
    #   trips, tours,
    #   join_by(tour_id),
    #   suffix = c("_trip", "_tour")
    # ) %>%
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      # tour_mode = convert_asim_mode(tour_mode),
      trip_purpose = convert_asim_purpose(trip_purpose),
      tour_purpose = convert_asim_purpose(tour_purpose),
      purpose = get_asim_purpose(pick(tour_id, tour_purpose, trip_purpose))) %>%
    select(origin, destination, trip_mode, purpose) %>%
    rename(mode = trip_mode) %>%
    group_by(origin, destination, mode, purpose) %>%
    summarise(trips = n(), .groups = "drop") %>%
    filter(!origin %in% ex_zones, !destination %in% ex_zones) %>%
    filter(trips > 0)

  # There is a lot of data here, so we're trying to do this in the least
  # memory-intensive way

  purpose_all <- trips_mode_purpose %>%
    group_by(origin, destination, mode) %>%
    summarise(trips = sum(trips)) %>%
    mutate(purpose = "all")

  trips <- bind_rows(trips_mode_purpose, purpose_all) %>%
    filter(trips > 0)

  rm(purpose_all)
  gc()

  mode_all <- trips_mode_purpose %>%
    group_by(origin, destination, purpose) %>%
    summarise(trips = sum(trips)) %>%
    mutate(mode = "all")

  trips <- bind_rows(trips, mode_all) %>%
    filter(trips > 0)

  rm(mode_all)
  gc()

  all_trips <- trips_mode_purpose %>%
    group_by(origin, destination) %>%
    summarise(trips = sum(trips)) %>%
    mutate(mode = "all", purpose = "all")

  trips <- bind_rows(trips, all_trips) %>%
    filter(trips > 0)

  rm(all_trips)
  rm(trips_mode_purpose)
  gc()

  trips %>%
    mutate(model = "asim", .after = destination)
}

combine_all_od <- function(wfrc_trips, asim_trips, distances){

  all_trips <- bind_rows(wfrc_trips, asim_trips)

  rm(asim_trips, wfrc_trips)
  gc()

  all_trips %>%
    left_join(distances, join_by(origin, destination)) %>%
    filter(trips > 0, distance < 1000)
}

convert_asim_mode <- function(mode){
  new_mode <- case_when(
    mode %in% c("DRIVEALONEFREE", "SHARED2FREE", "SHARED3FREE") ~ "auto",
    str_detect(mode, "COM|EXP|HVY|LOC|LRF|TAXI|TNC") ~ "transit",
    mode %in% c("BIKE", "WALK") ~ "nonmotor",
    TRUE ~ "ERROR IN MODE CONVERSION"
  ) %>%
    fct_relevel("auto", "transit", "nonmotor")
  new_mode
}

convert_asim_purpose <- function(purpose){
  new_purpose <- ifelse(
    purpose %in% c("home", "work", "atwork"),
    purpose,
    "other"
  )
  new_purpose
}

#' @param purpose_df Dataframe containing columns `tour_id`, `tour_purpose`, and `trip_purpose`
get_asim_purpose <- function(modes_df){
  combined_purposes <- modes_df %>%
    group_by(tour_id) %>%
    #See methodology section for rationale on this code segment
    mutate(trip_purpose = replace(trip_purpose, 1, "home")) %>%
    ungroup() %>%
    mutate(
      home_based = case_when(
        tour_purpose == "atwork" ~ FALSE,
        trip_purpose == "home" ~ TRUE,
        TRUE ~ FALSE
      ),
      purpose = case_when(
        !home_based ~ "nhb",
        tour_purpose == "work" ~ "hbw",
        tour_purpose == "other" ~ "hbo",
        TRUE ~ "ERROR IN PURPOSE CONVERSION"
      ) %>%
        fct_relevel("hbw", "hbo", "nhb"))

    combined_purposes$purpose
}

compare_mode_split <- function(combined_trips) {
  comp_modes <- combined_trips %>%
    group_by(model, mode, purpose) %>%
    summarise(trips = sum(trips)) %>%
    pivot_wider(names_from = model, values_from = trips) %>%
    arrange(purpose, mode)

  comp_modes
}

make_mode_split_comp <- function(comp_modes){

  asim_scale <- comp_modes %>%
    filter(purpose == "all" & mode == "all") %>%
    {.$wfrc/.$asim}

  pretty <- comp_modes %>%
    make_mode_and_purpose_pretty() %>%
    mutate(
      asim_scaled = asim*asim_scale,
      error = asim/wfrc - 1,
      scaled_error = asim_scaled/wfrc - 1,
      pct_error = label_percent(accuracy = 0.1)(error),
      pct_scaled_error = label_percent(accuracy = 0.1)(scaled_error),
    ) %>%
    select(purpose, mode, asim, wfrc, pct_error, pct_scaled_error)

  pretty
}

make_tlfd_comp_plot <- function(combined_trips){
  combined_trips %>%
    group_by(model, mode, purpose) %>%
    slice_sample(prop = 0.1, weight_by = trips) %>%
    make_mode_and_purpose_pretty() %>%
    make_model_pretty() %>%
    ggplot() +
    geom_density(aes(x = distance, weight = trips, color = model)) +
    facet_grid(
      vars(mode), vars(purpose),
      scales = "free_y") +
    scale_x_continuous(
      limits = c(0,25),
      sec.axis = sec_axis(~ . , name = "Trip Purpose", breaks = NULL, labels = NULL)) +
    scale_y_continuous(
      sec.axis = sec_axis(~ . , name = "Trip Mode", breaks = NULL, labels = NULL)) +
    labs(x = "Trip Distance (miles)", y = "Kernel density", color = "Model") +
    theme_density() +
    theme(legend.position = "bottom")
}

plot_calibration <- function(calibration_iters) {
  calibration_iters %>%
    select(iter, purpose, mode, asim_share, wfrc_share) %>%
    rename_with(\(x) str_remove(x, "_share"), c(asim_share, wfrc_share)) %>%
    pivot_longer(c(asim, wfrc), names_to = "model", values_to = "share") %>%
    filter(purpose == "all", mode != "all") %>%
    make_mode_and_purpose_pretty() %>%
    make_model_pretty() %>%
    ggplot() +
    geom_path(aes(x = iter, y = share, color = mode, lty = model)) +
    labs(x = "Calibration Iteration", y = "Mode Share", lty = "Model", color = "Mode") +
    scale_y_continuous(
      limits = c(0,1),
      trans = scales::pseudo_log_trans(0.1),
      breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.75,1),
      labels = scales::percent
      ) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank())
}

make_wfrc_hbj <- function(se_data) {
  data <- se_data %>%
    select(TAZ, ALLEMP, HBJ) %>%
    mutate(
      hbj_pct = case_when(
        ALLEMP == 0 & HBJ == 0 ~ 0,
        TRUE ~ HBJ/(ALLEMP+HBJ)))

  num_pct <- weighted.mean(data$hbj_pct, data$ALLEMP)
  overall_pct <- num_pct %>%
    label_percent(accuracy = 0.1)()

  plot <- data %>%
    ggplot() +
    geom_density(aes(x = hbj_pct, weight = ALLEMP, color = "Weighted by\nTAZ employment")) +
    geom_density(aes(x = hbj_pct, color = "Unweighted")) +
    geom_vline(xintercept = num_pct, lty = "dashed") +
    annotate(
      "text",
      x = num_pct - 0.003, y = 4,
      label = "ActivitySim target WFH %",
      angle = 90) +
    theme_density() +
    scale_x_continuous(labels = label_percent(), trans = "sqrt", expand = expansion(c(0,0.05))) +
    labs(x = "Home-based Job % by TAZ", y = "Kernel density", color = element_blank())

  list(pct = overall_pct, plot = plot)
}

get_trip_difference <- function(trips, reference) {
  diff <- trips %>%
    full_join(
      reference,
      join_by(o_TAZ, d_TAZ, o_DIST, d_DIST, purpose, mode),
      suffix = c("_scenario", "_reference")) %>%
    mutate(across(contains("trips_"), \(x) replace_na(x, 0))) %>%
    mutate(trips_difference = trips_scenario - trips_reference)
}

add_taz_distances <- function(trips, distances) {
  trips %>%
    left_join(distances, join_by(o_TAZ == origin, d_TAZ == destination))
}

diff_trip_matrix <- function(scen, by) {
  full_join(
    scen, by,
    join_by(origin, destination, mode),
    suffix = c("_scen", "_by")) %>%
    mutate(
      diff = trips_scen - trips_by
    ) %>%
    filter(round(diff) != 0) %>%
    mutate(origin, destination, mode, trips = diff, .keep = "none")
}

get_trip_diff <- function(trip_list = list()) {
  trip_list[[1]] %>%
    left_join(
      trip_list[[2]],
      join_by(origin, destination, purpose, mode),
      suffix = c("_1", "_2")) %>%
    mutate(
      diff = trips_2 - trips_1
      # pct_diff = diff / trips_1
    ) %>%
    rename_with(
      \(x) case_match(
        x,
        "trips_1" ~ names(trip_list)[1],
        "trips_2" ~ names(trip_list)[2]
      ),
      .cols = c(trips_1, trips_2)
    )
}

summ_trip_diff <- function(trips, groups = NULL) {
  trips %>%
    group_by(any_of(groups)) %>%
    summarise(

    )
}

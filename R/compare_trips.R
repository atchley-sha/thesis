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
  )
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
      ))

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
    make_mode_and_purpose_pretty() %>% 
    mutate(
      model = case_when(
        model == "asim" ~ "ActivitySim",
        model == "wfrc" ~ "WFRC/MAG"
      )
    ) %>% 
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

make_mode_and_purpose_pretty <- function(x) {
  x %>% 
    mutate(
      purpose = case_when(
        purpose == "hbw" ~ "Home-based Work",
        purpose == "hbo" ~ "Home-based Other",
        purpose == "nhb" ~ "Non\u2013Home-based",
        purpose == "all" ~ "All"
      ),
      mode = case_when(
        mode == "auto" ~ "Auto",
        mode == "transit" ~ "Transit",
        mode == "nonmotor" ~ "Non-motorized",
        mode == "all" ~ "All"
      )) %>% 
    mutate(
      purpose = factor(purpose, c("All", "Home-based Work", "Home-based Other", "Non\u2013Home-based")),
      mode = factor(mode, c("All", "Auto", "Transit", "Non-motorized")),
    )
}

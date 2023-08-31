#' @param od A named list of OD trip counts (long format)
combine_wfrc_od <- function(od){
  
  trips <- imap(
    od, \(df, i) rename_with(
      df,
      \(x) paste(x, i, sep = "_"),
      .cols = -c(origin, destination)
    )) %>% 
    reduce(function(x,y) left_join(x, y, join_by(origin, destination))) %>% 
    pivot_longer(
      -c(origin, destination),
      names_to = c("mode", "purpose"),
      names_sep = "_",
      values_to = "trips"
    )
  
  mode_all <- trips %>% 
    filter(purpose != "all") %>% 
    group_by(origin, destination, purpose) %>% 
    summarise(trips = sum(trips)) %>% 
    mutate(mode = "all")
  
  all_trips <- trips %>% 
    group_by(origin, destination) %>% 
    summarise(trips = sum(trips)) %>% 
    mutate(mode = "all", purpose = "all")
  
  combined <- bind_rows(
    trips, mode_all, all_trips) %>% 
    pivot_wider(
      names_from = c("mode", "purpose"),
      names_sep = "_",
      values_from = "trips") %>% 
    mutate(across(-c(origin, destination), \(x) replace_na(x, 0)))
  
  # 
  # distances %>% 
  # rename(distance = HBW) %>% 
  # left_join(trips, join_by(origin, destination)) %>% 
  # pivot_longer(
  #   -c(origin, destination, distance),
  #   names_to = c("mode", "purpose"),
  #   names_sep = "_",
  #   values_to = "trips")
  
  combined
}

plot_wfrc_test <- function(trips, color){

  trips %>% 
    filter(distance <= 50) %>% 
    pivot_longer(
      -c(origin, destination, distance),
      names_to = c("mode", "purpose"),
      names_sep = "_",
      values_to = "trips") %>% 
    ggplot() +
    geom_density(aes(distance, weight = trips, color = .data[[color]])) +
    xlim(0,50)
}

get_asim_od <- function(tripsfile, toursfile){
  
  trips <- read_csv(tripsfile) %>% 
    select(tour_id, origin, destination, primary_purpose, purpose, trip_mode) %>%
    rename(tour_purpose = primary_purpose, trip_purpose = purpose)
  tours <- read_csv(toursfile) %>% 
    select(tour_id, tour_mode)
  
  trips_mode_purpose <- left_join(
    trips, tours,
    join_by(tour_id),
    suffix = c("_trip", "_tour")
  ) %>% 
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      tour_mode = convert_asim_mode(tour_mode),
      trip_purpose = convert_asim_purpose(trip_purpose),
      tour_purpose = convert_asim_purpose(tour_purpose)) %>% 
    group_by(tour_id) %>% 
    # The first trip in each tour should be home-based (as is the last)
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
      )) %>% 
    select(origin, destination, trip_mode, purpose) %>% 
    rename(mode = trip_mode) %>% 
    group_by(origin, destination, mode, purpose) %>% 
    summarise(trips = n(), .groups = "drop")
  
  purpose_all <- trips_mode_purpose %>% 
    group_by(origin, destination, mode) %>% 
    summarise(trips = sum(trips)) %>% 
    mutate(purpose = "all")
  
  mode_all <- trips_mode_purpose %>% 
    group_by(origin, destination, purpose) %>% 
    summarise(trips = sum(trips)) %>% 
    mutate(mode = "all")
  
  all_trips <- trips_mode_purpose %>% 
    group_by(origin, destination) %>% 
    summarise(trips = sum(trips)) %>% 
    mutate(mode = "all", purpose = "all")
  
  combined <- bind_rows(
    trips_mode_purpose, purpose_all, mode_all, all_trips) %>% 
    pivot_wider(
      names_from = c("mode", "purpose"),
      names_sep = "_",
      values_from = "trips") %>% 
    mutate(across(-c(origin, destination), \(x) replace_na(x, 0)))
  
  combined
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


combine_all_od <- function(wfrc_trips, asim_trips, distances){
  
  wfrc <- wfrc_trips %>% 
    rename_with(
      \(x) paste("wfrc", x, sep = "_"),
      .cols = -c(origin, destination))
  asim <- asim_trips %>% 
    rename_with(
      \(x) paste("wfrc", x, sep = "_"),
      .cols = -c(origin, destination))
  
  distances %>% 
    rename(distance = HBW) %>% 
    left_join(wfrc, join_by(origin, destination)) %>% 
    left_join(asim, join_by(origin, destination))
}

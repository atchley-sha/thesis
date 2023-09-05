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
    pivot_longer(
      -c(origin, destination),
      names_to = c("mode", "purpose"),
      names_sep = "_",
      values_to = "trips") %>% 
    filter(trips > 0)
    
    
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
    filter(trips > 0) %>% 
    arrange(origin, destination) %>% 
    mutate(model = "wfrc", .after = destination)
  
  combined
}

get_asim_od <- function(tripsfile, toursfile, ex_zones){
  
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
    summarise(trips = n(), .groups = "drop") %>% 
    filter(!origin %in% ex_zones, !destination %in% ex_zones) %>% 
    filter(trips > 0)
  
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
    filter(trips > 0) %>% 
    arrange(origin, destination) %>% 
    mutate(model = "asim", .after = destination)
  
  combined
}

combine_all_od <- function(wfrc_trips, asim_trips, distances){
  
  combined <- bind_rows(wfrc_trips, asim_trips) %>% 
    left_join(distances, join_by(origin, destination)) %>% 
    # pivot_longer(
    #   -c(origin, destination, distance),
    #   names_to = c("model", "mode", "purpose"),
    #   names_sep = "_",
    #   values_to = "trips") %>% 
    filter(trips > 0)
  
  combined
}

# tlfd_comparison <- function(trips){
#   
#   trips %>% 
#     ggplot() +
#     geom_density(aes(x = distance, weight = trips, color = model)) +
#     facet_grid(
#       vars(mode), vars(purpose),
#       switch = "y",
#       scales = "free") +
#     xlim(0,25)
# 
# }
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
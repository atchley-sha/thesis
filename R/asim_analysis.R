get_o_tours <- function(tours, zones){
  tours %>% 
    filter(origin %in% zones)
}

get_trips_from_tours <- function(trips, tours, distances){
  trips %>% 
    filter(tour_id %in% tours$tour_id) %>% 
    left_join(distances, join_by(origin, destination))
}

get_vmt <- function(trips) {
  trips %>% 
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      # primary_purpose = convert_asim_purpose(primary_purpose),
      # purpose = convert_asim_purpose(purpose),
      # home_based = case_when(
      #   primary_purpose == "atwork" ~ FALSE,
      #   purpose == "home" ~ TRUE,
      #   outbound ~ TRUE,
      #   TRUE ~ FALSE
      # ),
      # simple_purpose = case_when(
      #   !home_based ~ "nhb",
      #   primary_purpose == "work" ~ "hbw",
      #   primary_purpose == "other" ~ "hbo",
      #   TRUE ~ "ERROR IN PURPOSE CONVERSION"
      # )
      ) %>% 
    group_by(primary_purpose, trip_mode) %>% 
    summarise(trips = n(), dist = sum(distance, na.rm = TRUE))
}

combine_scenarios <- function(scenarios = list()){
  bind_rows(scenarios, .id = "scenario")
}
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
      primary_purpose = reduce_asim_purposes(primary_purpose),
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

reduce_asim_purposes <- function(purpose){
  case_match(
    purpose,
    c("univ", "school") ~ "school",
    c("othdiscr", "othmaint") ~ "other",
    .default = purpose
    )
}

make_district_od <- function(trips, trans){
  trips %>% 
    left_join(trans, join_by(origin == TAZ)) %>% 
    left_join(trans, join_by(destination == TAZ), suffix = c("_o", "_d")) %>%
    mutate(trip_mode = convert_asim_mode(trip_mode)) %>% 
    count(DISTSML_o, DISTSML_d, primary_purpose, trip_mode)
}

diff_od <- function(scenarios = list()){
  reduce(scenarios, \(x,y) full_join(x,y, join_by(DISTSML_o, DISTSML_d, primary_purpose, trip_mode))) %>% 
    mutate(
      across(contains("n"), \(x) replace_na(x, 0)),
      diff = n.y - n.x
    ) %>% 
    `colnames<-`(c("origin", "destination", "purpose", "mode", names(scenarios), "diff"))
}

get_vmt_dist <- function(trips, distances) {
  trips %>% 
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      primary_purpose = reduce_asim_purposes(primary_purpose),
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
    left_join(distances, join_by(origin, destination)) %>% 
    group_by(origin, destination, primary_purpose, trip_mode) %>% 
    summarise(trips = n(), dist = sum(distance, na.rm = TRUE))
}

get_o_vmt <- function(vmt, trans){
  vmt %>% 
    filter(trip_mode == "auto") %>% 
    left_join(trans, join_by(origin == TAZ)) %>% 
    group_by(DISTSML) %>% 
      summarise(dist = sum(dist))
    
}

make_comp_o_vmt <- function(scen, by, zones) {
  data <- full_join(by, scen, join_by(DISTSML), suffix = c("_by", "_scen")) %>% 
    mutate(diff = dist_scen - dist_by)
  
  full_join(zones, data, join_by(DISTSML)) %>% 
    st_transform(4326) %>% 
    filter(diff < -10) %>% 
    ggplot() +
    annotation_map_tile("cartolight", zoomin = 0) +
    geom_sf(aes(fill = -diff), color = NA) +
    scale_fill_distiller(direction = 1) +
    lims(x = c(-112.2, -111), y = c(39.9,41.5)) +
    labs(fill = "Less VMT by trip origin,\nIncreased WFH\ncompared to Base") +
    theme_bw_map()
    
}

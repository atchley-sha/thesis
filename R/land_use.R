make_lu_vmt_plot <- function(df) {
  df %>% 
    mutate(
      primary_purpose = str_to_title(primary_purpose),
      in_zone = as.character(in_zone)
    ) %>% 
    ggplot(aes(y = trip_mode, x = dist, fill = trip_mode, alpha = in_zone)) +
    geom_col(position = position_stack()) +
    facet_grid(
      rows = vars(primary_purpose),
      switch = "y",
      space = "free_y",
      scales = "free_y") +
    labs(
      x = "Total Distance Traveled (mi.)",
      y = "Tour Purpose",
      fill = "Mode") +
    scale_fill_discrete(
      labels = c(auto = "Auto", transit = "Transit", nonmotor = "Non-motorized")) +
    scale_x_continuous(
      trans = sqrt_trans(),
      breaks = c(0, 10^3, 10^4, 5*10^4, 10^5, 2*10^5, 5*10^5),
      labels = label_comma(),
      expand = expansion(c(0,0.05))) +
    scale_alpha_manual(
      name = "Produced in new\ndevelopment",
      values = c("TRUE" = 1, "FALSE" = 0.6)) +
    guides(alpha = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(0, "lines"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0)
    )
}

get_lu_vmt <- function(trips, lu_tazs) {
  trips %>% 
    mutate(
      trip_mode = convert_asim_mode(trip_mode),
      primary_purpose = reduce_asim_purposes(primary_purpose),
      in_zone = origin %in% lu_tazs | destination %in% lu_tazs,
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
    group_by(primary_purpose, trip_mode, in_zone) %>% 
    summarise(trips = n(), dist = sum(distance, na.rm = TRUE))
}


get_wfrc_trip_diff <- function(trip_gen_file_by,trip_gen_file_scenario){
  
  tripsscen <- trip_gen_file_scenario %>% pivot_longer(!Z, names_to = "Type", values_to = "Trips")
  tripsby <- trip_gen_file_by %>% pivot_longer(!Z, names_to = "Type", values_to = "Trips")
  joined <- left_join(tripsscen,tripsby, join_by(Z,Type), suffix = c('.scen','.by'))
  mutate(joined, Trips.diff = Trips.scen - Trips.by)
  
}

tripgen_scen <- read.csv("data/cube_output/land_use/TripGenprison.csv")
tripgen_by <- read.csv("data/cube_output/base_2019/TripGenBY2019.csv")

#I probably need to make the taz file an input as well??
plot_wfrc_land_use_trip_diff <- function(trips_diff_data, type_of_trip_as_character, taz){
  
  trips_connected_to_taz <- taz %>% left_join(trips_diff_data, join_by("TAZID" == "Z")) %>% 
    filter(TAZID %in% focus_zones & Type == type_of_trip_as_character) %>% 
    select(TAZID, Type, Trips.scen, Trips.by, Trips.diff, geometry)
  
  trips_connected_to_taz %>% 
    st_transform(4326) %>% 
    ggplot() +
    annotation_map_tile("cartolight",zoomin = 0) +
    geom_sf(aes(fill = Trips.diff), 
            color = "black") + 
    scale_fill_gradient(low="#FFFFFF00", high="#002E5D") +
    theme_void()
}



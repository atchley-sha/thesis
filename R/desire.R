make_desire_lines <- function(trips, centroids, lu_tazs, trans){
  od <- trips %>% 
    mutate(
      origin,
      destination,
      mode = convert_asim_mode(trip_mode),
      in_zone = (origin %in% lu_tazs | destination %in% lu_tazs),
      .keep = "none"
    ) %>% 
    left_join(trans, join_by(origin == TAZ)) %>% 
    left_join(trans, join_by(destination == TAZ), suffix = c("_o", "_d")) %>% 
    select(DISTSML_o, DISTSML_d, mode, in_zone) %>% 
    rename(origin = DISTSML_o, destination = DISTSML_d) %>% 
    count(origin, destination, in_zone)
    # pivot_wider(names_from = mode, values_from = n) %>%
    # mutate(across(-c(origin, destination), \(x) replace_na(x, 0)))
  
  od_to_sf(od, centroids)
}

plot_desire_lines <- function(lines, zones, lims){
  nlines <- lines %>% 
    # mutate(
    #   in_zone = if_else(in_zone, "Yes", "No") %>% 
    #     fct_relevel("Yes", "No")
    #   ) %>% 
    arrange(rev(in_zone))
    
    
  ggplot(st_transform(zones, 4326)) +
    annotation_map_tile("cartolight", zoomin = 1) +
    geom_sf(fill = NA, color = "black") +
    geom_sf(aes(linewidth = n, color = in_zone), data = st_transform(nlines, 4326)) +
    scale_linewidth_continuous(range = c(0.1,3), limits = c(NA,1000)) +
    lims(x = lims$x, y = lims$y) +
    labs(color = "Produced in new\ndevelopment", linewidth = "Trips") +
    guides() +
    theme_bw_map()
}

better_desire_lines <- function(od, dist_centroids){
  od_to_sf(od, dist_centroids)
}

better_plot_desire_lines <- function(lines, zones){
  nlines <- lines %>% 
    mutate(diff = -diff) %>% 
    group_by(origin, destination) %>% 
    summarise(n = sum(diff))
    
  ggplot(st_transform(zones, 4326)) +
    annotation_map_tile("cartolight", zoomin = 0) +
    geom_sf(fill = NA, color = "black") +
    geom_sf(aes(linewidth = n), color = "blue", data = st_transform(nlines, 4326)) +
    scale_linewidth_continuous(range = c(0,3), limits = c(0,NA), breaks = c(500,1000,1500)) +
    lims(x = c(-112.2,-111.4), y = c(40.1,41.1)) +
    labs(linewidth = "Fewer trips,\nIncreased WFH\ncompared to Base") +
    theme_bw_map()
}

base_plot_desire_lines <- function(lines, zones){
  nlines <- lines %>% 
    mutate(diff = -diff) %>% 
    group_by(origin, destination) %>% 
    summarise(n = sum(diff))
    
  ggplot(st_transform(zones, 4326)) +
    annotation_map_tile("cartolight", zoomin = 0) +
    geom_sf(fill = NA, color = "black") +
    geom_sf(aes(linewidth = by), color = "blue", data = st_transform(lines, 4326)) +
    scale_linewidth_continuous(range = c(0,3), limits = c(0,NA), breaks = c(10000, 15000, 20000)) +
    lims(x = c(-112.2,-111.4), y = c(40.1,41.1)) +
    labs(linewidth = "Total trips,\nBase scenario") +
    theme_bw_map()
}
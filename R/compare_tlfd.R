#' @param od A named list of OD trip counts (long format)
combine_od <- function(od, distances){
  
  trips <- imap(
    od, \(df, i) rename_with(
      df,
      \(x) paste(x, i, sep = "_"),
      .cols = -c(origin, destination)
    )) %>% 
    reduce(function(x,y) left_join(x, y, join_by(origin, destination)))
  
  combined <- distances %>% 
    rename(distance = HBW) %>% 
    left_join(trips, join_by(origin, destination))
  
  combined
}

plot_wfrc_test <- function(trips, color){

trips %>% 
  pivot_longer(
    -c(origin, destination, distance),
    names_to = c("type", "purpose"),
    names_sep = "_",
    values_to = "trips") %>% 
  ggplot() +
  geom_density(aes(distance, weight = trips, color = .data[[color]])) +
  xlim(0,50)
}

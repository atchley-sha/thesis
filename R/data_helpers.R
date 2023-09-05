# Functions to help with reading in and cleaning data

get_ex_zones <- function(distance_omx){
  read_all_omx(distance_omx, "HBW") %>%
    rename(distance = HBW) %>%
    #the external zones are coded with distance 10000
    filter(distance > 1000) %>% 
    filter(origin == 1) %>% 
    {.$destination} %>% 
    sort()
}

read_distances <- function(distance_omx, external_zones){
  read_all_omx(distance_omx, "HBW") %>% 
    rename(distance = HBW) %>% 
    filter(!origin %in% external_zones, !destination %in% external_zones)
}
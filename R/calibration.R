run_calibration_tour <- function(in_asc, out_asc){
  temp <- targets::tar_read(combined_trips) %>%
    group_by(model, mode, purpose) %>%
    summarise(trips = sum(trips)) %>%
    pivot_wider(names_from = model, values_from = trips)
  
  temp %>%
    arrange(purpose, mode) %>%
    mutate(error = asim/wfrc - 1) %>%
    relocate(purpose)
  
  adjs <- temp %>% 
    filter(mode != "all") %>%
    group_by(purpose) %>% 
    mutate(asim_share = asim/sum(asim), wfrc_share = wfrc/sum(wfrc)) %>%
    ungroup() %>% 
    mutate(adj = log(wfrc_share/asim_share)) %>% 
    select(mode, purpose, adj) %>% 
    filter(purpose == "all", mode != "auto") %>% 
    transmute(name = paste(purpose, mode, sep = "_"), adj) %>% 
    deframe()
  
  coef0 <- read_csv(in_asc)
  
  coef0 %>% 
    mutate(
      value = case_when(
        !str_detect(coefficient_name, "ASC") ~ value,
        str_detect(coefficient_name, "joint") ~ value,
        TRUE ~ case_when(
          str_detect(coefficient_name, "transit|taxi|bus|rail|tnc|ride_hail") ~ value + adjs["all_transit"],
          str_detect(coefficient_name, "walk|bike") ~ value + adjs["all_nonmotor"],
          TRUE ~ value))
    ) %>% 
    write_csv(out_asc)
}

run_calibration_trip <- function(in_asc, out_asc){
  temp <- targets::tar_read(combined_trips) %>%
    group_by(model, mode, purpose) %>%
    summarise(trips = sum(trips)) %>%
    pivot_wider(names_from = model, values_from = trips)
  
  temp %>%
    arrange(purpose, mode) %>%
    mutate(error = asim/wfrc - 1) %>%
    relocate(purpose)
  
  adjs <- temp %>% 
    filter(mode != "all") %>%
    group_by(purpose) %>% 
    mutate(asim_share = asim/sum(asim), wfrc_share = wfrc/sum(wfrc)) %>%
    ungroup() %>% 
    mutate(adj = log(wfrc_share/asim_share)) %>% 
    select(mode, purpose, adj) %>% 
    filter(purpose == "all", mode != "auto") %>% 
    transmute(name = paste(purpose, mode, sep = "_"), adj) %>% 
    deframe()
  
  coef0 <- read_csv(in_asc)
  
  coef0 %>% 
    mutate(
      value = case_when(
        !str_detect(coefficient_name, "ASC") ~ value,
        str_detect(coefficient_name, "joint") ~ value,
        TRUE ~ case_when(
          str_detect(coefficient_name, "transit|taxi|bus|rail|tnc|ride_hail") ~ value + adjs["all_transit"],
          str_detect(coefficient_name, "walk|bike") ~ value + adjs["all_nonmotor"],
          TRUE ~ value))
    ) %>% 
    write_csv(out_asc, na = "")
}
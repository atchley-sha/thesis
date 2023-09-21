run_calibration_tours <- function(in_asc, out_asc, max_error, iteration){

  shares <- get_mode_split(max_error)
  shares
  shares %>% 
    write_csv(paste0("data/base_model_comparison/calibration/it", iteration, ".csv"))
  
  adjs <- get_adjs(shares)
  coef0 <- read_tours_asc(in_asc)
  coef1 <- adjust_asc(coef0, adjs)
  
  coef1 %>% 
    write_csv(out_asc, na = "")
}

run_calibration_trips <- function(in_asc, out_asc, max_error){
  
  shares <- get_mode_split(max_error)
  shares
  
  adjs <- get_adjs(shares)
  coef0 <- read_trips_asc(in_asc)
  coef1 <- adjust_asc(coef0, adjs)
  
  coef1 %>% 
    write_csv(out_asc, na = "")
}

get_mode_split <- function(max_error){
  temp <- targets::tar_read(combined_trips) %>%
    group_by(model, mode, purpose) %>%
    summarise(trips = sum(trips)) %>%
    pivot_wider(names_from = model, values_from = trips)
  
  shares <- temp %>%
    arrange(purpose, mode) %>%
    mutate(error = asim/wfrc - 1) %>%
    relocate(purpose) %>% 
    mutate(to_calibrate = case_when(
      mode %in% c("all", "auto") ~ FALSE, #DA trips are the reference, it's odd to calibrate other auto modes
      purpose == "all" ~ FALSE,
      error > max_error ~ TRUE,
      TRUE ~ FALSE
    ))
  
  shares
}

get_adjs <- function(shares){
  adjs <- shares %>% 
    filter(purpose != "all") %>%
    group_by(purpose) %>% 
    mutate(asim_share = asim/sum(asim), wfrc_share = wfrc/sum(wfrc)) %>%
    ungroup() %>% 
    mutate(adj = log(wfrc_share/asim_share)) %>% 
    mutate(purpose = if_else(purpose == "hbw", "work", "other")) %>% 
    group_by(purpose, mode) %>% 
    summarise(adj = mean(adj), to_calibrate = any(to_calibrate)) %>% 
    ungroup() %>% 
    filter(to_calibrate) %>% 
    select(mode, purpose, adj) %>% 
    transmute(name = paste(purpose, mode, sep = "_"), adj) %>% 
    deframe()
  
  adjs
}

read_tours_asc <- function(in_asc){
  coef0 <- read_csv(in_asc) %>% 
    mutate(
      mode = case_when(
        str_detect(coefficient_name, "^walk_transit|^drive_transit|^taxi|^bus|^rail|^tnc|^ride_hail") ~ "transit",
        str_detect(coefficient_name, "^walk|^bike") ~ "nonmotor",
        str_detect(coefficient_name, "^sr2|^sr3p") ~ "auto"
      ),
      purpose = case_when(
        str_detect(coefficient_name, "atwork") ~ "other",
        str_detect(coefficient_name, "work") ~ "work",
        TRUE ~ "other"
      ),
      adj = paste(purpose, mode, sep = "_"),
      to_calibrate = case_when(
        !str_detect(coefficient_name, "ASC") ~ FALSE,
        str_detect(coefficient_name, "joint") ~ FALSE,
        adj %in% names(adjs) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  coef0
}

read_trips_asc <- function(in_asc){
  coef0 <- read_csv(in_asc) %>% 
    mutate(
      mode = case_when(
        str_detect(coefficient_name, "ASC_walk_transit|ASC_drive_transit|ASC_taxi|ASC_bus|ASC_rail|ASC_tnc|ASC_ride_hail") ~ "transit",
        str_detect(coefficient_name, "ASC_walk|ASC_bike") ~ "nonmotor",
        str_detect(coefficient_name, "ASC_sr2|ASC_sr3p") ~ "auto"
      ),
      purpose = case_when(
        str_detect(coefficient_name, "atwork") ~ "other",
        str_detect(coefficient_name, "work") ~ "work",
        TRUE ~ "other"
      ),
      adj = paste(purpose, mode, sep = "_"),
      to_calibrate = case_when(
        !str_detect(coefficient_name, "ASC") ~ FALSE,
        str_detect(coefficient_name, "joint") ~ FALSE,
        adj %in% names(adjs) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  coef0
}

adjust_asc <- function(coef0, adjs){
  coef1 <- coef0 %>% 
    mutate(value = if_else(to_calibrate, value + adjs[adj], value)) %>% 
    select(coefficient_name, value, constrain)
  
  coef1
}
# Functions to help with reading in and cleaning data

pivot_tbm_edges <- function(edges) {
  
  read_csv(edges) %>% 
    mutate(across(where(is.numeric), ~ ifelse(.x < 0, 0, .x))) %>% 
    pivot_longer(-c(from, to), names_to = "mode", values_to = "number")
  
}

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

read_income_groups <- function(income_groups_file) {
  income_groups_file %>% 
    read_csv(col_types = cols(group = col_double())) %>% 
    mutate(
      across(
        c(low, high),
        \(x) paste0("$", prettyNum(x, big.mark = ",")
        ),
        .names = "{.col}_chr"
      ),
      inc_range = case_when(
        is.na(low) ~ paste("\u2264", high_chr),
        is.na(high) ~ paste("\u2265", low_chr),
        TRUE ~ paste0(low_chr, "\u2013", high_chr)
      ),
      low = replace_na(low, 0),
      inc_range = fct_reorder(inc_range, low)
    )
}

combine_calibration_iters <- function(files) {
  read_csv(files, id = "id") %>% 
    mutate(iter = str_replace(id, "^.*/it(\\d+)\\.csv", "\\1") %>% 
             as.numeric()) %>% 
    relocate(iter) %>% 
    select(-id) %>% 
    group_by(iter, purpose) %>% 
    mutate(
      error = asim/wfrc - 1,
      asim_share = asim/sum(asim)*2,
      wfrc_share = wfrc/sum(wfrc)*2,
      share_error = (asim_share/wfrc_share - 1) %>% round(3)
    )
}

get_wfrc_telecommute <- function(file) {
  table <- file %>% 
    read_csv() %>% 
    pivot_longer(
      -area,
      names_to = c("purpose", "type"),
      names_sep = "_",
      values_to = "trips") %>% 
    pivot_wider(names_from = type, values_from = trips) %>% 
    mutate(tcm_pct = tcm/(tcm+ntcm))
  
  overall_pct <-
    (sum(table$tcm)/sum(table$tcm, table$ntcm)) %>% 
    label_percent(accuracy = 0.1)()
  
  pretty_table <- table %>% 
    mutate(
      area = str_replace(area, "_", " ") %>% 
        str_to_title(),
      purpose = case_match(
        purpose,
        "hbw" ~ "Home-based Work",
        "nhbw" ~ "Non\u2013home-based Work"
      ),
      tcm_pct = label_percent(accuracy = 0.1)(tcm_pct)) %>% 
    rename(
      "Region" = area,
      "Trip Purpose" = purpose,
      "Telecommute Trips" = tcm,
      "Non-telecommute Trips" = ntcm,
      "Telecommute %" = tcm_pct
    )
  
  list(pct = overall_pct, table = pretty_table)
}

sample_trips <- function(combined_trips, prop = 0.1, weight = TRUE){
  grouped <- combined_trips %>% 
    group_by(model, mode, purpose)
  
  if(weight) return(slice_sample(grouped, prop = prop, weight_by = trips))
  if(!weight) return(slice_sample(grouped, prop = prop))
}

make_mode_and_purpose_pretty <- function(x, factors = TRUE) {
  x %>% 
    mutate(
      purpose = case_match(
        as.character(purpose),
        "hbw" ~ "Home-based Work",
        "hbo" ~ "Home-based Other",
        "nhb" ~ "Non\u2013home-based",
        "all" ~ "All"
      ),
      mode = case_match(
        as.character(mode),
        "auto" ~ "Auto",
        "transit" ~ "Transit",
        "nonmotor" ~ "Non-motorized",
        "all" ~ "All"
      )) %>% 
    mutate(
      purpose = factor(purpose, c("All", "Home-based Work", "Home-based Other", "Non\u2013home-based")),
      mode = factor(mode, c("All", "Auto", "Transit", "Non-motorized")),
    )
}

make_model_pretty <- function(x) {
  x %>% 
    mutate(
      model = case_match(
        model,
        "asim" ~ "ActivitySim",
        "wfrc" ~ "WFRC Model"
      )
    )
}

get_taz <- function(taz_file){
  st_read(taz_file) %>% 
    transmute(
      TAZ = TAZID,
      DISTSML
    )
}

get_zone_centroids <- function(zones){
  zones %>% 
    st_centroid()
}

get_districts <- function(taz){
  taz %>% 
    group_by(DISTSML) %>% 
    summarise()
}

read_asim_telecommute_coeffs <- function(file) {
  file %>% 
    read_csv() %>% 
    filter(str_detect(coefficient_name, "coefj")) %>% 
    select(coefficient_name, value) %>% 
    mutate(coefficient_name = str_remove(coefficient_name, "coefj_")) %>% 
    separate(coefficient_name, c("jobcode", "days")) %>% 
    mutate(
      days = case_match(
        days,
        "1day" ~ "1 day",
        "23day" ~ "2\u20133 days",
        "4day" ~ "4 days"
      )
    ) %>% 
    pivot_wider(names_from = days, values_from = value) 
}

read_trip_matrix <- function(omx_file) {
  omx_file %>% 
    read_all_omx(names = c("auto", "transit", "nonmotor")) %>% 
    pivot_longer(-c(origin, destination), names_to = "mode", values_to = "trips")
}


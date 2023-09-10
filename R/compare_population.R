read_asim_population <- function(perfile, hhfile){
  
  per_raw <- read_csv(perfile)
  hh_raw <- read_csv(hhfile)
  
  per <- per_raw %>% 
    # filter(AGEP > 0) %>% 
    group_by(TAZ) %>% 
    summarise(
      num_persons = n(),
      avg_age = mean(AGEP),
      pct_female = table(SEX)[1] / sum(table(SEX)) #I *think* 1 is female
      )
  
  hh <- hh_raw %>% 
    mutate(across(
      c(WIF),
      \(x) if_else(x < 0, NA, x)
    )) %>% 
    ####### THESE INCOME GROUPS ARE WRONG ########
    mutate(
      inc_group = case_when(
        HHINCADJ < 0 ~ "unknown",
        HHINCADJ < 1 ~ "INC1",
        HHINCADJ < 2 ~ "INC2",
        HHINCADJ < 3 ~ "INC3",
        HHINCADJ >= 3 ~ "INC4",
        TRUE ~ "Error in income groups"
      )
    )

  hh_inc <- hh %>% 
    group_by(TAZ, inc_group) %>% 
    summarize(n = n()) %>% 
    pivot_wider(names_from = inc_group, values_from = n, values_fill = 0)
  
  hh_other <- hh %>% 
    group_by(TAZ) %>% 
    summarise(
      num_hh = n(),
      avg_hh_inc = mean(HHINCADJ, na.rm = TRUE),
      hh_size = mean(NP),
      hh_veh = mean(VEH),
      hh_wrk = mean(WIF, na.rm = TRUE)
    ) %>% 
    mutate(across(everything(), \(x) replace_na(x,0)))
  
  hh_full <- full_join(hh_other, hh_inc, join_by(TAZ))
  
  pop <- left_join(per, hh_full, join_by(TAZ))
    # mutate(hh_size_check = num_persons / num_hh, .after = hh_size)
  
  pop
  
}

read_zonal_data <- function(sefile, incomefile){
  
  se_raw <- read_csv(sefile)
  income <- read_csv(incomefile) %>% 
    select(Z, INC1:INC4)
  
  se <- se_raw %>% 
    rename(TAZ = `;TAZID`) %>% 
    select(TAZ, TOTHH, HHPOP, HHSIZE, AVGINCOME) %>% 
    rename(TOTPOP = HHPOP) %>% 
    left_join(income, join_by(TAZ == Z))

  se
}

make_zonal_comparison <- function(pop, se){
  
  comp <- left_join(pop, se, join_by(TAZ)) %>% 
    transmute(
      TAZ,
      hh_diff = num_hh - TOTHH,
      per_diff = num_persons - TOTPOP,
      per_pct_error = num_persons/TOTPOP - 1,
      income_diff = avg_hh_inc - AVGINCOME,
      income_pct_error = avg_hh_inc/AVGINCOME - 1
    )
}

zone_comparison_maps <- function(pop_comp, taz){
  
  data <- taz %>% 
    left_join(pop_comp, join_by(TAZID == TAZ))
  
  per <- data %>%
    ggplot() +
    # annotation_map_tile(type = "stamenwatercolor", zoomin = 1) +
    geom_sf(aes(fill = log2(per_pct_error+1)), color = NA) +
    labs(title = "Number of Persons", fill = "Doublings") +
    scale_fill_fermenter(palette = "RdBu", breaks = -2:2) +
    theme_void()
  
  hh <- data %>% 
    ggplot() +
    # annotation_map_tile(type = "cartodark", zoomin = 1) +
    geom_sf(aes(fill = hh_diff), color = NA) +
    labs(title = "Number of Households", fill = "Diff") +
    scale_fill_fermenter(palette = "RdBu") +
    theme_void()
  
  income <- data %>% 
    ggplot() +
    # annotation_map_tile(type = "cartodark", zoomin = 1) +
    geom_sf(aes(fill = log2(income_pct_error+1)), color = NA) +
    labs(title = "Average Income", fill = "Doublings") +
    scale_fill_fermenter(palette = "RdBu", breaks = -2:2) +
    theme_void()
  
  # income_diff <- data %>% 
  #   ggplot() +
  #   geom_sf(aes(fill = income_diff)) +
  #   labs(title = "Average Income", fill = "Diff") +
  #   scale_fill_fermenter(palette = "RdBu") +
  #   theme_void()
  
  maps = list(hh = hh, persons = per, income = income)
  
  maps
}
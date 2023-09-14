read_asim_population <- function(perfile, hhfile){
  
  per_raw <- read_csv(perfile)
  hh_raw <- read_csv(hhfile)
  
  per <- per_raw %>% 
    filter(AGEP > 0) %>%
    group_by(TAZ) %>% 
    summarise(
      TOTPOP = n(),
      avg_age = mean(AGEP),
      pct_female = table(SEX)[1] / sum(table(SEX)) #I *think* 1 is female
      )
  
  hh <- hh_raw %>% 
    mutate(across(
      c(WIF),
      \(x) if_else(x < 0, NA, x)
    )) %>% 
    ## Check income groups
    mutate(
      inc_group = case_when(
        HHINCADJ < 0 ~ "INCunknown",
        HHINCADJ < 45000 ~ "INC1",
        HHINCADJ < 75000 ~ "INC2",
        HHINCADJ < 125000 ~ "INC3",
        HHINCADJ >= 125000 ~ "INC4",
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
      TOTHH = n(),
      AVGINCOME = mean(HHINCADJ, na.rm = TRUE),
      hh_size = mean(NP),
      hh_veh = mean(VEH),
      hh_wrk = mean(WIF, na.rm = TRUE)
    ) %>% 
    mutate(across(everything(), \(x) replace_na(x,0)))
  
  hh_full <- full_join(hh_other, hh_inc, join_by(TAZ))
  
  pop <- left_join(per, hh_full, join_by(TAZ)) %>% 
    select(
      TAZ,
      TOTHH,
      TOTPOP,
      AVGINCOME,
      contains("INC")
    ) %>% 
    mutate(across(everything(), \(x) if_else(x < 0, NA, x)))
  
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

make_zonal_comparison <- function(asim_pop, se_data, taz_file){
  
  taz <- st_read(taz_file) %>% 
    transmute(TAZ = TAZID)
  
  comp <- asim_pop %>% 
    full_join(se_data, join_by(TAZ), suffix = c("_asim", "_wfrc")) %>% 
    pivot_longer(
      matches("asim|wfrc"),
      names_to = c("metric", "model"),
      names_sep = "_") %>% 
    select(TAZ, model, metric, value) %>% 
    pivot_wider(names_from = model, values_from = value) %>% 
    mutate(
      diff = asim - wfrc,
      error = case_when(
        round(wfrc) == 0 ~ diff,
        TRUE ~ diff / wfrc
      ))

  full_join(taz, comp, join_by(TAZ))
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
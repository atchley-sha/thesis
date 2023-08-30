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
      c(WIF,HHINCADJ),
      \(x) if_else(x < 0, NA, x)
      )) %>% 
    group_by(TAZ) %>% 
    summarise(
      num_hh = n(),
      avg_hh_inc = mean(HHINCADJ, na.rm = TRUE),
      hh_size = mean(NP),
      hh_veh = mean(VEH),
      hh_wrk = mean(WIF, na.rm = TRUE)
    ) %>% 
    mutate(across(everything(), \(x) replace_na(x,0)))
  
  pop <- left_join(per, hh, join_by(TAZ))
    # mutate(hh_size_check = num_persons / num_hh, .after = hh_size)
  
  pop
  
}

read_zonal_data <- function(sefile){
  
  se_raw <- read_csv(sefile)
  
  se <- se_raw %>% 
    rename(TAZ = `;TAZID`) %>% 
    select(TAZ, TOTHH, HHPOP, HHSIZE, AVGINCOME) %>% 
    rename(TOTPOP = HHPOP) %>% 
    mutate(TOTHH = trunc(TOTHH))
  
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
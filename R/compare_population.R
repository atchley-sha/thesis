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
        HHINCADJ < 0 ~ "INCunknown_asim",
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
      MEDINCOME = median(HHINCADJ, na.rm = TRUE),
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
    # select(TAZ, TOTHH, HHPOP, HHSIZE, AVGINCOME) %>% 
    #Not sure if there's another median income metric
    mutate(MEDINCOME = AVGINCOME) %>% 
    rename(TOTPOP = HHPOP) %>% 
    left_join(income, join_by(TAZ == Z))

  se
}

make_zonal_comparison <- function(asim_pop, se_data, taz){
  
  comp <- asim_pop %>% 
    full_join(se_data, join_by(TAZ), suffix = c("_asim", "_wfrc")) %>% 
    pivot_longer(
      matches("asim|wfrc"),
      names_to = c("metric", "model"),
      names_sep = "_") %>% 
    select(TAZ, model, metric, value) %>% 
    pivot_wider(names_from = model, values_from = value) %>% 
    left_join(select(se_data, TAZ, TOTHH), join_by(TAZ)) %>% 
    mutate(
      diff = asim - wfrc,
      # RPD = 2*\frac{y - x}{|y| + |x|}
      rpd = if_else(
        asim == 0 & wfrc == 0,
        0,
        2*diff/(abs(asim) + abs(wfrc))
      ),
      hrpd = rpd/2,
      pct = case_when(
        wfrc == 0 & asim == 0 ~ 0,
        is.na(wfrc) | is.na(asim) ~ NA,
        wfrc == 0 ~ 1,
        TRUE ~ diff / wfrc
      )
      )

  full_join(taz, comp, join_by(TAZ))
}

make_inc_groups_map <- function(pop_comp, income_groups, lims = list(x = c(-112.15,-111.6), y = c(40.2,40.8))) {
  pop_comp %>% 
    filter(str_detect(metric, "^INC")) %>% 
    mutate(inc_group = str_remove(metric, "^INC") %>% as.numeric()) %>% 
    left_join(income_groups, join_by(inc_group == group)) %>% 
    mutate(
      metric = case_when(
        metric == "INC1" ~ "\u2264 $45,000",
        metric == "INC2" ~ "$45,000\u2013$75,000",
        metric == "INC3" ~ "$75,000\u2013$125,000",
        metric == "INC4" ~ "\u2265 $125,000"
      ) %>% 
        fct_reorder(inc_group)
    ) %>% 
    st_transform(4326) %>% 
    ggplot() +
    annotation_map_tile("cartolight", zoomin=0) +
    geom_sf(aes(fill = diff), color = NA) +
    facet_wrap(~metric) +
    scale_fill_gradient2(limits = c(-250, 250), na.value = NA) +
    labs(fill = "Difference in number\nof households,\nPopulationSim compared\nto WFRC") +
    lims(x = lims$x, y = lims$y) +
    theme_bw_map()
}

make_avg_inc_map <- function(pop_comp, lims = list(x = c(-112.15,-111.6), y = c(40.2,40.8))){
  pop_comp %>% 
    filter(metric == "AVGINCOME") %>% 
    st_transform(4326) %>% 
    ggplot() +
    annotation_map_tile("cartolight", zoomin=0) +
    geom_sf(aes(fill = rpd), color = NA) +
    scale_fill_gradient2(limits = c(-2,2), na.value = NA) +
    labs(fill = "Difference in mean income,\nPopulationSim compared to\nWFRC/MAG\n(Relative Percent Difference)") +
    lims(x = lims$x, y = lims$y) +
    theme_bw_map()
}

make_med_inc_map <- function(pop_comp, lims = list(x = c(-112.15,-111.6), y = c(40.2,40.8))){
  pop_comp %>% 
    filter(metric == "MEDINCOME") %>% 
    st_transform(4326) %>% 
    ggplot() +
    annotation_map_tile("cartolight", zoomin=0) +
    geom_sf(aes(fill = pct), color = NA) +
    scale_percent_diff(2, base = 2, sigma = 1.5) +
    labs(fill = "Percent difference in\nTAZ median income,\nPopulationSim compared\nto WFRC") +
    lims(x = lims$x, y = lims$y) +
    theme_bw_map()
}

make_inc_plot <- function(pop_comp, income_groups){
  pop_comp %>% 
    filter(metric %in% c("MEDINCOME")) %>% 
    pivot_longer(c(asim, wfrc), names_to = "model", values_to = "income") %>% 
    mutate(
      model = case_when(
        model == "asim" ~ "PopulationSim",
        model == "wfrc" ~ "WFRC"
      )
      ) %>% 
    ggplot() +
    geom_density(aes(color = model, x = income, weight = TOTHH), linewidth = 1) +
    geom_vline(aes(xintercept = high, lty = "line"), data = income_groups) +
    scale_x_continuous(labels = label_comma(prefix = "$"), limits = c(0,200000), expand = c(0,0)) +
    scale_linetype_manual(values = c(line = "dotted"), labels = c("Income group\nboundaries"), name = NULL) +
    guides(color = guide_legend(order = 1)) +
    theme_density() +
    labs(x = "TAZ Median Income (weighted by number of households)", y = "Kernel density", color = "Model")
}

make_pop_comp_map <- function(pop_comp, lims = list(x = c(-112.15,-111.6), y = c(40.2,40.8))){
  pop_comp %>% 
    filter(metric == "TOTPOP") %>%
    st_transform(4326) %>% 
    ggplot() +
    annotation_map_tile("cartolight", zoomin=0) +
    geom_sf(aes(fill = pct), color = NA) +
    scale_percent_diff(4, base = 3, sigma = 1.5) +
    labs(fill = "Percent difference in TAZ\npopulation, PopulationSim\ncompared to WFRC") +
    lims(x = lims$x, y = lims$y) +
    theme_bw_map()
}

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
    ) %>% 
    select(group, inc_range)
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
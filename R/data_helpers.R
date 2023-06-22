# Functions to help with reading in and cleaning data

pivot_tbm_edges <- function(edges) {
  
  read_csv(edges) %>% 
    mutate(across(where(is.numeric), ~ ifelse(.x < 0, 0, .x))) %>% 
    pivot_longer(-c(from, to), names_to = "mode", values_to = "number")
  
}
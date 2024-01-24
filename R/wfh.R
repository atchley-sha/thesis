compare_tc <- function(wfrc, asim, trans) {
  left_join(wfrc, asim) %>% 
    left_join(trans) %>% 
    relocate(name) %>% 
    select(-jobcode)
}

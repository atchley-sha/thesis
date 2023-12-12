library(tidyverse)

dirs <- c("base_2019", "transit", "landuse", "wfh")

tours <- list()
trips <- list()
persons <- list()
hhs <- list()

sample_pct <- 0.1

for(dir in dirs){
  tours_raw <- read_csv(file.path("data/asim_output", dir, "final_tours.csv.gz")) %>% 
    slice_sample(prop = sample_pct)
  trips_raw <- read_csv(file.path("data/asim_output", dir, "final_trips.csv.gz")) %>% 
    filter(tour_id %in% tours$tour_id)
  persons_raw <- read_csv(file.path("data/asim_output", dir, "final_persons.csv.gz")) %>% 
    filter(person_id %in% tours$person_id)
  hhs_raw <- read_csv(file.path("data/asim_output", dir, "final_households.csv.gz")) %>% 
    filter(household_id %in% persons$household_id)
  
  tours[dir] <- tours_raw
  trips[dir] <- trips_raw
  persons[dir] <- persons_raw
  hhs[dir] <- hhs_raw
}
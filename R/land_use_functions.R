get_asim_new_persons <- function(per_new, per_by, tazs) {
	per_new %>%
		filter(
			home_zone_id %in% tazs,
			!person_id %in% (per_by %>%
				filter(home_zone_id %in% tazs) %>%
					pull(person_id))
		) %>%
		pull(person_id %>% unique())
}

get_asim_lu_new_trips <- function(trips_raw, persons) {
	trips_raw %>%
		filter(person_id %in% persons) %>%
		count_asim_trips()
}

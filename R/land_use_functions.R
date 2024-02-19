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

make_lu_se_table <- function(se, taz_list) {
	se %>%
		select(-c(CO_TAZID, CO_FIPS, CO_NAME)) %>%
		pivot_longer(-TAZ) %>%
		filter(
			TAZ %in% taz_list,
			str_detect(name, "EMP|HH"),
			!name %in% c("ALLEMP", "HHSIZE")) %>%
		select(TAZ, name, value) %>%
		pivot_wider() %>%
		relocate(TAZ, TOTHH, HHPOP) %>%
		relocate(TOTEMP, .after = last_col()) %>%
		arrange(TAZ)
}

combine_se_tables <- function(se = list()) {
	full_join(se[[1]], se[[2]], join_by(TAZ), suffix = paste0("_", names(se)))
}

get_asim_trip_diff_by_person <- function(new_raw_trips, old_raw_trips){
	old_trips_count <- old_raw_trips %>%
		mutate(
			purpose = .asim_purposes_for_transit_scenario(tour_purpose),
			mode = .asim_modes_for_transit_scenario(mode)
		) %>%
		count(person_id, purpose, mode, name = "old_trips")
	new_trips_count <- new_raw_trips %>%
		mutate(
			purpose = .asim_purposes_for_transit_scenario(tour_purpose),
			mode = .asim_modes_for_transit_scenario(mode)
		) %>%
		count(person_id, purpose, mode, name = "new_trips")

	full_join(
		new_trips_count, old_trips_count,
		join_by(person_id, purpose, mode)
	) %>%
		mutate(across(c(new_trips, old_trips), \(x) replace_na(x, 0))) %>%
		group_by(person_id) %>%
		mutate(
			diff = new_trips - old_trips,
			new_person = all(old_trips == 0),
			missing_person = all(new_trips == 0)
		) %>%
		ungroup() %>%
		filter(diff != 0)
}

.asim_modes_for_transit_scenario <- function(mode) {
	case_when(
		mode == "DRIVEALONEFREE" ~ "drivealone",
		mode %in% c("SHARED2FREE", "SHARED3FREE") ~ "carpool",
		TRUE ~ convert_asim_mode(mode)
	)
}

.asim_purposes_for_transit_scenario <- function(purpose) {
	case_match(
		purpose,
		c("eatout", "shopping", "social", "escort") ~ "other",
		.default = keep_asim_purpose(purpose)
	)
}

summarise_asim_mode_switching <- function(table) {
	table %>%
		group_by(purpose, mode) %>%
		summarise(diff = abs(sum(diff))) %>%
		mutate(
			purpose = str_to_title(purpose),
			mode = pretty_mode(mode)
		) %>%
		ungroup()
}

combine_tr_trips_se_for_income_plot <- function(cube, asim) {
	cube_cleaned <- cube %>%
		filter(mode == "transit") %>%
		select(purpose, trips, income = med_income)
	asim_cleaned <- asim %>%
		filter(mode == "transit") %>%
		mutate(purpose, income, trips = 1, .keep = "none")

	bind_rows(cube = cube_cleaned, asim = asim_cleaned, .id = "model")
}

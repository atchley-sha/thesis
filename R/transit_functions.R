OLD_get_asim_trip_diff_by_person <- function(new_raw_trips, old_raw_trips){
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

OLD_summarise_asim_mode_switching <- function(table) {
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
		filter(mode %in% c("local", "crt")) %>%
		select(purpose, mode, trips, income = med_income)
	asim_cleaned <- asim %>%
		filter(mode %in% c("local", "crt")) %>%
		mutate(purpose, mode, income, trips = 1, .keep = "none")

	bind_rows(cube = cube_cleaned, asim = asim_cleaned, .id = "model")
}

get_asim_raw_trip_diff <- function(raw_trips = list()) {
	clean_trips <- imap(raw_trips, \(x, idx) {
		select(x, person_id, tour_purpose, origin, destination, depart, mode) %>%
			mutate(
				purpose = .asim_purposes_for_transit_scenario(tour_purpose),
				mode = .asim_modes_for_transit_scenario(mode),
				# scenario = idx,
				.keep = "unused")
	})

	full_join(
		clean_trips[[1]], clean_trips[[2]],
		join_by(person_id, purpose, origin, destination, depart),
		suffix = paste0("_", names(clean_trips)),
		relationship = "many-to-many"
	)
}

get_asim_mode_switching <- function(trips_list = list()) {
	trips <- trips_list %>%
		map(\(x) {
			x %>%
				mutate(
					person_id, depart, origin, destination,
					tour_purpose, trip_purpose,
					mode = convert_asim_mode_tr(mode),
					.keep = "none") %>%
				distinct(
					person_id, depart, origin, destination,
					tour_purpose, trip_purpose, .keep_all = TRUE)
		}
		)

	base <- trips[[2]]
	scen <- trips[[1]]

	joined <- full_join(
		scen, base,
		# Have to join by everything or else different purposes match up twice
		join_by(person_id, origin, destination, depart, trip_purpose, tour_purpose),
		suffix = paste0("_", names(trips))
	)

	return(joined)
}

get_asim_atwork_mode_switching <- function(trips_list = list(), per) {
	trips <- trips_list %>%
		map(\(x) {
			x %>%
				mutate(
					person_id, depart, origin, destination,
					tour_purpose, trip_purpose,
					mode = convert_asim_mode_tr_atwork(mode),
					.keep = "none") %>%
				distinct(
					person_id, depart, origin, destination,
					tour_purpose, trip_purpose, .keep_all = TRUE)
		}
		)

	base <- trips[[2]]
	scen <- trips[[1]]

	joined <- full_join(
		scen, base,
		# Have to join by everything or else different purposes match up twice
		join_by(person_id, origin, destination, depart, trip_purpose, tour_purpose),
		suffix = paste0("_", names(trips))
	) %>%
		filter(
			person_id %in% per, tour_purpose == "atwork",
			mode_tr != "drive_alone",
			!is.na(mode_tr), !is.na(mode_by))

	return(joined)
}

get_asim_work_switchers <- function(mode_switching) {
	mode_switching %>%
		filter(tour_purpose == "work", mode_by == "drive_alone") %>%
		pull(person_id) %>%
		unique()
}

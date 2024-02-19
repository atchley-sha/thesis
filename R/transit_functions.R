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
		filter(mode == "transit") %>%
		select(purpose, trips, income = med_income)
	asim_cleaned <- asim %>%
		filter(mode == "transit") %>%
		mutate(purpose, income, trips = 1, .keep = "none")

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

get_asim_mode_switching <- function(raw_trip_diff) {
	mode_names <- names(raw_trip_diff)[
		which(str_detect(names(raw_trip_diff), "mode"))]

	raw_trip_diff %>%
		filter(
			.data[[mode_names[1]]] != .data[[mode_names[2]]]) %>%
		group_by(person_id, origin, destination, purpose, depart, mode_by, mode_tr) %>%
		summarise(n = n(), .groups = "drop") %>%
		pivot_wider(
			names_from = c(mode_by, mode_tr), names_sep = "_",
			values_from = n, values_fill = 0) %>%
		mutate(
			carpool_drivealone = carpool_drivealone - drivealone_carpool,
			drivealone_nonmotor = drivealone_nonmotor - nonmotor_drivealone,
			carpool_transit = carpool_transit - transit_carpool,
			carpool_nonmotor = carpool_nonmotor - nonmotor_carpool,
			nonmotor_transit = nonmotor_transit - transit_nonmotor,
			drivealone_transit = drivealone_transit - transit_drivealone,
			.keep = "unused"
		) %>%
		# filter(
		# 	carpool_drivealone != drivealone_carpool |
		# 	drivealone_nonmotor != nonmotor_drivealone |
		# 	carpool_transit != transit_carpool |
		# 	carpool_nonmotor != nonmotor_carpool |
		# 	nonmotor_transit != transit_nonmotor |
		# 	drivealone_transit != transit_drivealone
		# ) %>%
		pivot_longer(-c(person_id:depart), names_to = "switch", values_to = "trips") %>%
		filter(trips != 0) %>%
		separate_wider_delim(switch, "_", names = c("raw_from", "raw_to")) %>%
		mutate(
			from = if_else(trips < 0, raw_to, raw_from),
			to = if_else(trips < 0, raw_from, raw_to),
			trips = abs(trips),
			.keep = "unused")
}

get_asim_atwork_mode_switching <- function(raw_trip_diff, mode_switching) {
	mode_names <- names(raw_trip_diff)[
		which(str_detect(names(raw_trip_diff), "mode"))]

	atwork_switchers <- mode_switching %>%
		filter(purpose == "at-work") %>%
		pull(person_id) %>%
		unique()

	raw_trip_diff %>%
		filter(
			person_id %in% atwork_switchers,
			purpose == "work",
			!is.na(mode_by),
			!is.na(mode_tr)
			) %>%
		mutate(trips = 1) %>%
		rename(from = mode_by, to = mode_tr) %>%
		bind_rows(filter(mode_switching, purpose == "at-work"))
}

get_asim_work_transit_switching <- function(raw_trip_diff, mode_switching) {
	mode_names <- names(raw_trip_diff)[
		which(str_detect(names(raw_trip_diff), "mode"))]

	work_transit_switchers <- mode_switching %>%
		filter(purpose == "work", from == "drivealone") %>%
		pull(person_id) %>%
		unique()

	mode_switching %>%
		filter(purpose == "at-work", person_id %in% work_transit_switchers)

	# raw_trip_diff %>%
	# 	filter(
	# 		person_id %in% work_transit_switchers,
	# 		purpose == "at-work",
	# 		mode_by == "drivealone",
	# 		mode_tr != "drivealone",
	# 		!is.na(mode_by),
	# 		!is.na(mode_tr)
	# 	) %>%
	# 	mutate(trips = 1) %>%
	# 	rename(from = mode_by, to = mode_tr) %>%
	#   bind_rows(filter(mode_switching, purpose == "at-work", person_id %in% work_transit_switchers))
}

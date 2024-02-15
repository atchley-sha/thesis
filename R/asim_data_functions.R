read_asim_trips_file <- function(trips_file) {
	trips_file %>%
		read_csv() %>%
		rename(
			tour_purpose = primary_purpose,
			trip_purpose = purpose,
			mode = trip_mode
		)
}

summarise_asim_hh <- function(asim_hh, income_groups) {
	annotated_hh <- asim_hh %>%
		left_join(
			select(income_groups, group, low, high),
			join_by(between(income, low, high, bounds = "[)"))) %>%
		select(
			household_id, home_zone_id,
			income,
			# income_group = group,
			# income_segment,
			inc_group = income_segment,
			hhsize)

	income_groups_wide <- annotated_hh %>%
		group_by(home_zone_id) %>%
		count(inc_group) %>%
		pivot_wider(
			names_from = inc_group, values_from = n,
			names_prefix = "inc_group_"
		)

	# income_segments <- annotated_hh %>%
	# 	group_by(home_zone_id) %>%
	# 	count(income_segment) %>%
	# 	pivot_wider(
	# 		names_from = income_segment, values_from = n,
	# 		names_prefix = "income_segment_"
	# 	)

	annotated_hh %>%
		group_by(home_zone_id) %>%
		summarise(
			med_income = median(income),
			pop = sum(hhsize),
			hh_size = mean(hhsize),
			num_hh = n()
		) %>%
		full_join(income_groups_wide, join_by(home_zone_id)) %>%
		# full_join(income_segments, join_by(home_zone_id)) %>%
		mutate(across(!where(is.character), \(x) replace_na(x, 0))) %>%
		rename(TAZ = home_zone_id)

}

count_asim_trips <- function(trips) {
	trips %>%
		mutate(
			purpose = make_asim_purpose(
				select(., tour_id, tour_purpose, trip_purpose)
			),
			mode = convert_asim_mode(mode)
		) %>%
		count(origin, destination, purpose, mode, name = "trips")
}

make_asim_purpose <- function(df) {
	df %>%
		group_by(tour_id) %>%
		mutate(
			#If a tour doesn't start at work, assume it starts at home
			trip_purpose = replace(trip_purpose, 1, "home"),
			home_based = case_when(
				tour_purpose == "atwork" ~ FALSE,
				trip_purpose == "home" ~ TRUE,
				TRUE ~ FALSE
			),
			purpose = case_when(
				!home_based ~ "nhb",
				tour_purpose == "work" ~ "hbw",
				TRUE ~ "hbo"
			) %>%
				factor(c("hbw", "hbo", "nhb"))
		) %>%
		pull(purpose)
}

convert_asim_mode <- function(mode){
	case_when(
		mode %in% c("DRIVEALONEFREE", "SHARED2FREE", "SHARED3FREE") ~ "auto",
		str_detect(mode, "COM|EXP|HVY|LOC|LRF|TAXI|TNC") ~ "transit",
		mode %in% c("BIKE", "WALK") ~ "nonmotor",
		TRUE ~ "ERROR IN MODE CONVERSION"
	) %>%
		factor(c("auto", "transit", "nonmotor"))
}

read_asim_telecommute_coefficients <- function(coeffs_file) {
	coeffs_file %>%
		read_csv() %>%
		filter(str_detect(coefficient_name, "coefj")) %>%
		select(coefficient_name, value) %>%
		mutate(coefficient_name = str_remove(coefficient_name, "coefj_")) %>%
		separate(coefficient_name, c("jobcode", "days")) %>%
		mutate(
			days = case_match(
				days,
				"1day" ~ "1 day",
				"23day" ~ "2\u20133 days",
				"4day" ~ "4 days"
			)
		) %>%
		pivot_wider(names_from = days, values_from = value)
}

combine_asim_mode_choice_calibration_iters <- function(iters_files) {
	iters_files %>%
		read_csv(id = "id") %>%
		mutate(
			iter = str_replace(id, "^.*/it(\\d+)\\.csv", "\\1") %>%
				as.numeric()) %>%
		relocate(iter) %>%
		select(-id) %>%
		group_by(iter, purpose) %>%
		mutate(
			error = asim/cube - 1,
			asim_share = asim/sum(asim)*2,
			cube_share = cube/sum(cube)*2,
			share_error = asim_share/cube_share - 1
		)
}

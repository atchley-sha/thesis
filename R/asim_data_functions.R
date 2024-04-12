read_asim_trips_file <- function(trips_file) {
	trips_file %>%
		read_csv() %>%
		rename(
			tour_purpose = primary_purpose,
			trip_purpose = purpose,
			mode = trip_mode
		)
}

read_asim_tours_file <- function(tours_file) {
	tours_file %>%
		read_csv() %>%
		rename(tour_purpose = primary_purpose)
}

count_asim_tours <- function(raw_tours) {

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

count_asim_tr_trips <- function(trips) {
	trips %>%
		mutate(
			purpose = make_asim_purpose(
				select(., tour_id, tour_purpose, trip_purpose)
			),
			mode = convert_asim_mode_tr(mode)
		) %>%
		count(origin, destination, purpose, mode, name = "trips")
}

count_asim_trips_keep_purpose <- function(trips) {
	trips %>%
		mutate(
			purpose = keep_asim_purpose(tour_purpose),
			mode = convert_asim_mode(mode)
		) %>%
		count(origin, destination, purpose, mode, name = "trips")
}

make_asim_purpose <- function(df) {
	# We have to do some hacky stuff since the two models treat
	# trip purposes differently. It's not perfect, but it is
	# good enough for this project. It will miscategorize some
	# trips, but the on the whole it's an okay translation layer.

	df %>%
		group_by(tour_id) %>%
		mutate(
			# `trip_purpose` is essentially the trip destination.
			# Since all (non--at-work) tours start at home, changing
			# the first `trip_purpose` to `home` means that now trips
			# with an origin *or* destination at home have a
			# `trip_purpose` of `home`.
			trip_purpose = replace(trip_purpose, 1, "home"),
			# The above also makes subtours (at-work tours) have a
			# home-based trip, so we filter those out explicitly
			home_based = case_when(
				tour_purpose == "atwork" ~ FALSE,
				trip_purpose == "home" ~ TRUE,
				TRUE ~ FALSE
			),
			purpose = case_when(
				!home_based ~ "nhb",
				# Home-based *trips* on a work *tour* are counted as HBW.
				# This is technically not accurate, as only trips that
				# originate at home and end at work (or vice versa) are
				# HBW. However, trip-based models don't really account
				# for trips on the way to work, and ActivitySim does,
				# so this is close enough.
				tour_purpose == "work" ~ "hbw",
				TRUE ~ "hbo"
			) %>%
				factor(c("hbw", "hbo", "nhb"))
		) %>%
		pull(purpose)
}

keep_asim_purpose <- function(purpose) {
	case_match(
		purpose,
		c("univ", "school") ~ "school",
		c("othdiscr", "othmaint") ~ "other",
		"eatout" ~ "eat out",
		"atwork" ~ "at-work",
		.default = purpose
	)
}

convert_asim_mode <- function(mode) {
	case_when(
		mode == "DRIVEALONEFREE" ~ "drive_alone",
		mode %in% c("SHARED2FREE", "SHARED3FREE") ~ "carpool",
		str_detect(mode, "COM|EXP|HVY|LOC|LRF|TAXI|TNC") ~ "transit",
		mode %in% c("BIKE", "WALK") ~ "nonmotor",
		TRUE ~ "ERROR IN MODE CONVERSION"
	) %>%
		factor(c("drive_alone", "carpool", "transit", "nonmotor"))
}

convert_asim_mode_tr <- function(mode) {
	case_when(
		mode == "DRIVEALONEFREE" ~ "drive_alone",
		# str_detect(mode, "SHARED2") ~ "sr2",
		# str_detect(mode, "SHARED3") ~ "sr3p",
		str_detect(mode, "SHARED2|SHARED3") ~ "carpool",
		str_detect(mode, "COM|HVY") ~ "crt",
		str_detect(mode, "LOC|EXP|LRF") ~ "local",
		str_detect(mode, "TNC|TAXI") ~ "rh",
		str_detect(mode, "BIKE|WALK") ~ "nonmotor"
	)
}

convert_asim_mode_tr_atwork <- function(mode) {
	case_when(
		mode == "DRIVEALONEFREE" ~ "drive_alone",
		str_detect(mode, "SHARED2") ~ "sr2",
		str_detect(mode, "SHARED3") ~ "sr3p",
		# str_detect(mode, "SHARED2|SHARED3") ~ "carpool",
		str_detect(mode, "COM|HVY") ~ "crt",
		str_detect(mode, "LOC|EXP|LRF") ~ "local",
		str_detect(mode, "TNC|TAXI") ~ "rh",
		str_detect(mode, "BIKE|WALK") ~ "nonmotor"
	)
}

read_asim_telecommute_coefficients <- function(coeffs_file, job_code_transl) {
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
		left_join(job_code_transl, join_by(jobcode)) %>%
		select(-jobcode) %>%
		relocate(name) %>%
		filter(!is.na(name))
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

read_mcc_adjustments_files <- function(adj_file) {
	adj_file %>%
		read_csv(id = "path") %>%
		mutate(
			iter = str_extract(path, "\\d+") %>%
				as.numeric()
		) %>%
		relocate(iter) %>%
		select(-path)
}

list_mcc_trips_iters <- function(folder) {
	list.files(
		"data/asim/output",
		recursive = TRUE,
		pattern = "final_trips.csv",
		full.names = TRUE) %>%
		str_subset(paste0("data/asim/output/", folder, "_\\d+/"))
}

# read_mcc_trips <- function(trips_file) {
# 	trips_file %>%
# 		read_csv(id = "path") %>%
# 		rename(
# 			tour_purpose = primary_purpose,
# 			trip_purpose = purpose,
# 			mode = trip_mode
# 		) %>%
# 		mutate(
# 			iter = str_extract(path, "\\d+") %>%
# 				as.numeric()
# 		) %>%
# 		relocate(iter) %>%
# 		select(-path)
# }

read_mcc_coeffs <- function(coeffs_file) {
	coeffs_file %>%
		read_csv(id = "path") %>%
		filter(!str_detect(coefficient_name, "#")) %>%
		mutate(
			iter = str_extract(path, "\\d+") %>%
				as.numeric(),
			constrain = as.logical(constrain),
			value = as.numeric(value)
		) %>%
		mutate(constrain = replace_na(constrain, FALSE)) %>%
		filter(
			str_detect(coefficient_name, "ASC"),
			!str_detect(coefficient_name, "joint"),
			!constrain) %>%
		select(iter, coefficient_name, value)
}

plot_mcc_adjustments <- function(adjustments) {
	adjustments %>%
		select(iter, purpose, mode, asim_share, wfrc_share) %>%
		pivot_longer(c(asim_share, wfrc_share), names_to = "model", values_to = "share") %>%
		mutate(mode_cat = case_match(
			mode,
			c("bike", "walk") ~ "nonmotor",
			c("local_bus", "express_bus") ~ "bus",
			c("lrt", "crt") ~ "rail",
			# c("local_bus", "express_bus", "lrt", "crt") ~ "transit",
			c("sr2", "sr3") ~ "carpool",
			.default = mode
		)) %>%
		filter(mode_cat != "TNC") %>%
		group_by(model, purpose, iter, mode_cat) %>%
		summarise(share = sum(share)) %>%
		ggplot(aes(x = iter, y = share, lty = model, color = mode_cat)) +
		facet_wrap(~purpose) +
		scale_y_continuous(transform = "sqrt") +
		scale_color_brewer(palette = "Set1") +
		geom_line(linewidth = 1)
}

get_asim_trips_se <- function(raw_trips, per, hh) {
	raw_trips %>%
		mutate(
			mode = convert_asim_mode(mode),
			purpose = make_asim_purpose(
				select(., tour_id, tour_purpose, trip_purpose))
		) %>%
		left_join(
			select(per, -c(household_id, PUMA, TRACT, home_zone_id)),
			join_by(person_id)) %>%
		left_join(hh, join_by(household_id))
}

summarise_asim_transit_se <- function(se_trips) {
	se_trips %>%
		filter(mode == "transit") %>%
		group_by(purpose) %>%
		summarise(
			transit_trips = n(),
			across(
				c(income, age, ),
				\(x) median(x, na.rm = TRUE)),
			.groups = "drop"
		)
}


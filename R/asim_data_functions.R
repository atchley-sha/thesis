read_asim_trips_file <- function(trips_file) {
	trips_file %>%
		read_csv() %>%
		rename(
			tour_purpose = primary_purpose,
			trip_purpose = purpose,
			mode = trip_mode
		)
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

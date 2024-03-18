temp_count_asim_trips <- function(trips) {
	trips %>%
		mutate(
			purpose = make_asim_purpose(
				select(., tour_id, tour_purpose, trip_purpose)
			),
			mode = temp_convert_asim_mode(mode)
		) %>%
		count(origin, destination, purpose, mode, name = "trips")
}

temp_convert_asim_mode <- function(mode){
	case_when(
		str_detect(mode, "HVY") ~ "COM",
		str_detect(mode, "WALK_") ~ str_remove(mode, "WALK_"),
		str_detect(mode, "DRIVE_") ~ str_remove(mode, "DRIVE_"),
		str_detect(mode, "TNC") ~ "TNC",
		str_detect(mode, "TAXI") ~ "TNC",
		TRUE ~ mode
	)
}

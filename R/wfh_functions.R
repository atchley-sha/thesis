calculate_trip_and_pmt_diff <- function(trip_diff, distances, model = NULL) {
	trip_diff %>%
		left_join(distances) %>%
		group_by(purpose, mode) %>%
		mutate(across(c(diff, distance), \(x) replace_na(x, 0))) %>%
		summarise(
			across(
				-c(origin, destination, distance),
				list(
					trips = \(x) sum(x),
					pmt = \(x) sum(x*distance)
				)),
			.groups = "drop"
		) %>%
		mutate(
			trips_pct = diff_trips / by_trips,
			pmt_pct = diff_pmt / by_pmt
		) %>%
		select(
			purpose, mode,
			contains("_trips"), contains("trips"),
			contains("_pmt"), contains("pmt"),
			-contains("diff")
		) %>%
		rename_with(\(x) paste0(model, if(!is.null(model)) "_", x), -c(purpose, mode))
}

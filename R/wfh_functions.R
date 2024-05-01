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

calculate_tlfd_diff <- function(trips, distances) {
	trips %>%
		select(model, purpose, origin, destination, mode, wfh, by) %>%
		pivot_longer(c(wfh, by), names_to = "scenario", values_to = "trips") %>%
		left_join(distances, join_by(origin, destination)) %>%
		group_by(model, purpose, mode, scenario) %>%
		filter(trips > 0, distance > 0) %>%
		mutate(weight = trips/sum(trips)) %>%
		group_map(
			function(df, keys) {
				density(df$distance, weights = df$weight, from = 0) %>%
					broom::tidy() %$%
					approx(x, y, xout = seq(0,max(x), 0.01)) %>%
					as_tibble() %>%
					bind_cols(keys)
			}
		) %>%
		bind_rows() %>%
		pivot_wider(names_from = scenario, values_from = y) %>%
		mutate(diff = wfh - by)
}

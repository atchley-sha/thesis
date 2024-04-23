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

plot_tlfd_diff <- function(tlfd_diff) {
	tlfd_diff %>%
		filter(purpose != "nhb") %>%
		mutate(
			model = pretty_model(model),
			purpose = pretty_purpose(purpose),
			mode = pretty_mode(mode)
		) %>%

		ggplot(aes(x = x, y = diff, color = model)) +
		facet_grid(rows = vars(purpose), cols = vars(mode), scales = "free") +
		geom_hline(yintercept = 0) +
		# geom_line() +
		geom_smooth(se = FALSE, method = "loess", span = 0.5) +
		facetted_pos_scales(
			x = list(
				mode %in% c("Drive Alone", "Carpool") ~ scale_x_continuous(limits = c(0,50)),
				mode == "Non-motorized" ~ scale_x_continuous(limits = c(0,15)),
				mode == "Transit" ~ scale_x_continuous(limits = c(0,50))
			)
		) +
		coord_cartesian(ylim = c(-4e-4, 4e-4)) +
		# scale_y_continuous(limits = c(-4e-4, 4e-4)) +
		labs(
			x = "Distance (mi)",
			y = "Difference in kernel density (Remote Work \u2212 Baseline)",
			color = "Model"
		)
}

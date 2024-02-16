summarise_combined_se_data <- function(combined_se, transl = NULL, zones_geom) {
	if(!is.null(transl)) {
		se <- combined_se %>%
			left_join(transl, join_by(TAZ)) %>%
			group_by(model, DIST, name) %>%
			summarise(
				sum = sum(value),
				mean = mean(value),
				median = median(value),
				.groups = "drop"
			) %>%
			mutate(
				value = case_match(
					name,
					c("med_income") ~ median,
					c("hh_size") ~ mean,
					.default = sum
				)
			) %>%
			select(model, DIST, name, value)
	} else {
		se <- combined_se
	}

	se %>%
		left_join(zones_geom) %>%
		st_as_sf() %>%
		pivot_wider(names_from = "model", values_from = "value") %>%
		mutate(across(c(cube, asim), \(x) replace_na(x, 0))) %>%
		mutate(
			diff = asim - cube,
			diff_pct = diff/cube
		)
}

compare_by_mode_split <- function(combined_trips) {
	combined_trips %>%
		group_by(model, mode, purpose) %>%
		summarise(trips = sum(trips), .groups = "drop") %>%
		pivot_wider(names_from = model, values_from = trips) %>%
		arrange(purpose, mode) %>%
		mutate(
			mode = pretty_mode(mode),
			purpose = pretty_purpose(purpose),
			# model = pretty_model(model)
		) %>%
		mutate(
			diff = asim - cube,
			pct_diff = diff/cube,
			label_pct_diff = label_percent(accuracy = 0.1)(pct_diff)
		) %>%
		select(-pct_diff)
}

combine_se_data <- function(se = list()) {
	se %>%
		bind_rows(.id = "model") %>%
		pivot_longer(-c(model, TAZ), names_to = "name", values_to = "value")
}

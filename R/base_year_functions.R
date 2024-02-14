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

compare_mode_split <- function(combined_trips) {
	combined_trips %>%
		group_by(model, mode, purpose) %>%
		summarise(trips = sum(trips)) %>%
		pivot_wider(names_from = model, values_from = trips) %>%
		arrange(purpose, mode) %>%
		mutate(
			mode = pretty_mode(mode),
			purpose = pretty_purpose(purpose),
			model = pretty_model(model)
		) %>%
		mutate(
			diff = asim/wfrc - 1,
			pct_diff = label_percent(accuracy = 0.1)(diff)
		)
}

combine_se_data <- function(se = list()) {
	se %>%
		bind_rows(.id = "model") %>%
		pivot_longer(-c(model, TAZ), names_to = "name", values_to = "value")
}

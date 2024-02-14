plot_combined_tlfd <- function(combined_trips, distances){
	combined_trips %>%
		left_join(distances, join_by(origin, destination)) %>%
		group_by(model, mode, purpose) %>%
		slice_sample(prop = 0.1, weight_by = trips) %>%
		mutate(
			mode = pretty_mode(mode),
			purpose = pretty_purpose(purpose),
			model = pretty_model(model)
		) %>%
		ggplot() +
		geom_density(aes(x = distance, weight = trips, color = model)) +
		facet_grid(
			rows = vars(mode), cols = vars(purpose),
			scales = "free_y") +
		scale_x_continuous(
			limits = c(0,25),
			sec.axis = sec_axis(~ . , name = "Trip Purpose", breaks = NULL, labels = NULL)) +
		scale_y_continuous(
			sec.axis = sec_axis(~ . , name = "Trip Mode", breaks = NULL, labels = NULL)) +
		labs(x = "Trip Distance (miles)", y = "Kernel density", color = "Model") +
		# theme_density() +
		theme(legend.position = "bottom")
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

plot_asim_mode_choice_calibration <- function(calibration_iters) {
	calibration_iters %>%
		select(iter, purpose, mode, asim_share, cube_share) %>%
		rename_with(\(x) str_remove(x, "_share"), c(asim_share, cube_share)) %>%
		pivot_longer(c(asim, cube), names_to = "model", values_to = "share") %>%
		filter(purpose == "all", mode != "all") %>%
		mutate(
			mode = pretty_mode(mode),
			purpose = pretty_purpose(purpose),
			model = pretty_model(model)
		) %>%
		ggplot() +
		geom_path(aes(x = iter, y = share, color = mode, lty = model)) +
		labs(
			x = "Calibration Iteration", y = "Mode Share",
			lty = "Model", color = "Mode"
		) +
		scale_y_continuous(
			limits = c(0,1),
			trans = scales::pseudo_log_trans(0.1),
			breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.75,1),
			labels = scales::percent
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


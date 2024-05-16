plot_wfh_tlfd_diff <- function(combined_trip_diff, distances) {
	combined_trip_diff %>%
		mutate(missing_trips = -diff) %>%
		filter(missing_trips > 0, purpose == "hbw") %>%
		mutate(
			model = pretty_model(model),
			mode = pretty_mode(mode)
		) %>%
		left_join(distances, join_by(origin, destination)) %>%
		ggplot(aes(color = model, x = distance, weight = missing_trips)) +
		facet_wrap(~mode, scales = "free", ncol = 1) +
		facetted_pos_scales(
			x = list(
				mode == "Auto" ~ scale_x_continuous(limits = c(0,30)),
				mode == "Non-motorized" ~ scale_x_continuous(limits = c(0,10)),
				mode == "Transit" ~ scale_x_continuous(limits = c(0,30))
			)
		) +
		geom_density() +
		# theme(legend.position = "bottom") +
		labs(x = "Trip Distance (miles)", y = "Kernel density", color = "Model")
}

# plot_wfh_by_tlfd_diff_comp <- function(combined_trip_diff, which_model, distances) {
# 	trips <- combined_trip_diff %>%
# 		mutate(missing_trips = -diff) %>%
# 		filter(purpose == "hbw", model == which_model) %>%
# 		left_join(distances, join_by(origin, destination)) %>%
# 		select(mode, by, missing_trips, distance) %>%
# 		pivot_longer(c(by, missing_trips), names_to = "scenario", values_to = "trips") %>%
# 		filter(trips > 0)
#
# 	scenario_labels <- c(
# 		by = "Baseline Scenario",
# 		missing_trips = "\"Missing\" trips in\nIncreased WFH Scenario")
#
# 	trips %>%
# 		mutate(mode = pretty_mode(mode)) %>%
# 		ggplot(aes(x = distance, color = scenario, lty = scenario, weight = trips)) +
# 		facet_wrap(~mode, scales = "free", ncol = 1) +
# 		facetted_pos_scales(x = list(
# 			mode == "Auto" ~ scale_x_continuous(limits = c(NA,30)),
# 			mode == "Non-motorized" ~ scale_x_continuous(limits = c(NA,10)),
# 			mode == "Transit" ~ scale_x_continuous(limits = c(NA,20)))) +
# 		geom_density() +
# 		scale_color_manual(
# 			values = c(by = "grey50", missing_trips = "navy"),
# 			labels = scenario_labels) +
# 		scale_linetype_manual(
# 			values = c(by = "dashed", missing_trips = "solid"),
# 			labels = scenario_labels) +
# 		labs(
# 			x = "Trip Distance (miles)", y = "Kernel density",
# 			color = "Scenario", lty = "Scenario")
# }

plot_tlfd_diff <- function(tlfd_diff) {
	tlfd_diff %>%
		# filter(purpose != "nhb") %>%
		filter(purpose == "hbw") %>%
		mutate(
			model = pretty_model(model),
			purpose = pretty_purpose(purpose),
			mode = pretty_mode(mode)
		) %>%

		ggplot(aes(x = x, y = diff, color = model)) +
		# facet_grid(rows = vars(purpose), cols = vars(mode), scales = "free") +
		facet_wrap(~mode, scales = "free") +
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
		# scale_y_continuous(labels = label_number(style_negative = "hyphen")) +
		# coord_cartesian(ylim = c(-4e-4, 4e-4)) +
		labs(
			x = "Distance (mi)",
			y = "Difference in kernel density (Remote Work \u2212 Baseline)",
			color = "Model"
		)
}

plot_cube_remote_work_totals <- function(cube_remote_work_totals) {
	cube_remote_work_totals %>%
		mutate(type = case_match(
			type,
			"tc" ~ "Telecommute",
			"wfh" ~ "Work From Home"
		)) %>%
		ggplot(aes(x = year, y = pct, color = type)) +
		geom_line(linewidth = 1) +
		geom_vline(xintercept = c(2019, 2050), lty = "dotted") +
		scale_y_continuous(transform = "sqrt", labels = label_percent()) +
		scale_x_continuous(breaks = seq(1990,2060,10)) +
		labs(x = "Year", y = "Remote Work Rate Among Workers", color = element_blank())
}

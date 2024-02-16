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

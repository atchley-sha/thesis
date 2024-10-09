# Population ####
.by_population_bg <- function(...) {
	list(
		annotation_map_tile("cartolight", zoomin = 0),
		geom_sf(aes(fill = diff_pct, color = as.character(is.na(diff_pct)))),
		scale_percent_diff(...),
		scale_color_manual(
			values = c("TRUE" = "#00000000", "FALSE" = "black"),
			guide = "none"),
		theme_map(lims = list(x = c(-112.3, -111.4), y = c(39.9,41.3)))
	)
}

make_pop_comparison_map <- function(combined_se_data){
	combined_se_data %>%
		filter(name == "pop") %>%
		ggplot() +
		.by_population_bg(high_val = 4, base = 3, sigma = 1.5) +
		labs(
			fill = "Percent difference in District\npopulation, PopulationSim\ncompared to WF"
		)
}

make_med_income_comparison_map <- function(combined_se_data){
	combined_se_data %>%
		filter(name == "med_income") %>%
		mutate(diff_pct = if_else(asim == 0, NA, diff_pct)) %>%
		ggplot() +
		.by_population_bg(high_val = 2, base = 2, sigma = 1.5) +
		labs(
			fill = "Percent difference in\nTAZ median income,\nPopulationSim compared\nto WF"
		)

}

make_inc_groups_comparison_map <- function(combined_se_data, income_groups) {
	combined_se_data %>%
		filter(str_detect(name, "inc_group_")) %>%
		mutate(group = str_remove(name, "inc_group_") %>% as.numeric()) %>%
		left_join(select(income_groups, group, inc_range)) %>%
		mutate(inc_range = fct_reorder(inc_range, group)) %>%
		ggplot() +
		facet_wrap(~inc_range, nrow = 1) +
		.by_population_bg(high_val = 2, base = 2, sigma = 1.5) +
		labs(
			fill = "Percent difference in\nnumber of households,\nPopulationSim compared\nto WF"
		)

}

make_inc_density_comparison_plot <- function(combined_se_data, income_groups) {
	hh_count <- combined_se_data %>%
		filter(name == "num_hh") %>%
		st_drop_geometry() %>%
		select(1, num_hh = cube)

	combined_se_data %>%
		filter(name == "med_income") %>%
		st_drop_geometry() %>%
		select(1, asim, cube) %>%
		rename(popsim = asim) %>%
		pivot_longer(c(popsim, cube), names_to = "model", values_to = "med_inc") %>%
		left_join(hh_count) %>%
		ggplot(aes(color = model, x = med_inc, weight = num_hh)) +
		geom_density(linewidth = 1) +
		geom_vline(
			aes(xintercept = high, lty = "line"),
			data = income_groups %>% filter(!is.infinite(high))) +
		scale_x_continuous(labels = label_comma(prefix = "$")) +
		# scale_color_brewer(labels = pretty_model, palette = "Paired") +
		scale_color_bright(labels = pretty_model) +
		scale_linetype_manual(
			values = c(line = "dotted"),
			labels = c("Income group\nboundaries"), name = NULL) +
		guides(color = guide_legend(order = 1)) +
		labs(x = "District Median Income (weighted by number of households)", y = "Kernel density", color = "Model")
}

# Mode choice ####
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
			labels = scales::label_percent()
		)
}

# TLFD ####
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
			limits = c(0,20),
			sec.axis = sec_axis(~ . , name = "Trip Purpose", breaks = NULL, labels = NULL)) +
		scale_y_continuous(
			sec.axis = sec_axis(~ . , name = "Trip Mode", breaks = NULL, labels = NULL)) +
		labs(x = "Trip Distance (miles)", y = "Kernel density", color = "Model") +
		# theme_density() +
		theme(legend.position = "bottom")
}

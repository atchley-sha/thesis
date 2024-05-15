OLD_plot_asim_mode_switching<- function(table) {
	added_missing <- table %>%
		select(person_id, purpose, mode, from = old_trips, to = new_trips) %>%
		group_by(person_id, purpose) %>%
		arrange(person_id, purpose, mode) %>%
		mutate(
			missing_from = max(sum(to) - sum(from), 0),
			missing_to = max(sum(from) - sum(to), 0)
		) %>%
		ungroup() %>%
		pivot_wider(
			names_from = mode, values_from = c(from, to),
			names_glue = "{mode}_{.value}"
			) %>%
		pivot_longer(
			-c(person_id, purpose),
			names_to = c("mode", "ft"), names_sep = "_"
			) %>%
		filter(value > 0) %>%
		pivot_wider(names_from = ft, values_fill = 0)

	tot_trips <- added_missing %>%
		group_by(person_id, purpose) %>%
		summarise(trips = sum(to), .groups = "drop")

	switching <- added_missing %>%
		group_by(person_id, purpose) %>%
		mutate(from_pct = from / sum(from), to_pct = to/sum(to)) %>%
		ungroup() %>%
		select(person_id, purpose, mode, from_pct, to_pct) %>%
		{full_join(
			select(., -to_pct), select(., -from_pct),
			join_by(person_id, purpose), suffix = c("_from", "_to"),
			relationship = "many-to-many"
		)} %>%
		mutate(pct = from_pct*to_pct) %>%
		select(person_id, purpose, from = mode_from, to = mode_to, pct) %>%
		filter(pct != 0) %>%
		left_join(tot_trips, join_by(person_id, purpose)) %>%
		mutate(trips = pct*trips) %>%
		group_by(purpose, from, to) %>%
		summarise(trips = sum(trips), .groups = "drop")

	switching %>%
		filter(from != "missing", to != "missing") %>%
		mutate(
			purpose = str_to_title(purpose),
			across(c(from, to), \(x) pretty_mode(x)),
			across(c(from, to), \(x) fct_expand(x, "", after = 0)),
			across(c(from, to), \(x) replace_na(x, ""))
		) %>%
		ggplot(aes(axis1 = from, axis2 = to, y = trips)) +
		facet_wrap(~purpose, scales = "free") +
		scale_x_discrete(limits = c("Original mode", "New mode"), expand = c(.2, .05)) +
		geom_alluvium(aes(fill = from), color = "black") +
		guides(fill = "none") +
		# new_scale_fill() +
		geom_stratum() +
		# scale_fill_manual(values = c("-" = "black")) +
		geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
		labs(y = "Number of Trips")
}

plot_trips_diff_by_district <- function(
		trip_diff, dist_transl, dist_geom, fr_line, fr_stops) {

	trips <- trip_diff %>%
		left_join(dist_transl, join_by(origin == TAZ)) %>%
		group_by(purpose, mode, DIST) %>%
		summarise(diff = sum(diff)) %>%
		mutate(
			mode = pretty_mode(mode),
			purpose = pretty_purpose(purpose)) %>%
		filter(!round(diff) == 0) %>%
		left_join(dist_geom, join_by(DIST)) %>%
		st_as_sf()

	ggplot() +
		facet_grid(rows = vars(purpose), cols = vars(mode)) +
		annotation_map_tile("cartolight", zoom = 10) +
		geom_sf(aes(fill = diff), data = trips) +
		scale_fill_gradient2() +
		# scale_fill_gradient2(limits = c(-50, 50), oob = oob_squish) +
		labs(fill = "Change in trips by\nproduction district") +
		.add_frontrunner_to_plot(fr_line, fr_stops, alpha = 0.4) +
		theme_map(zoom = FALSE) +
		theme(legend.position = "bottom")
}

plot_tr_new_transit_income_dist <- function(combined_transit_se_trips) {
	combined_transit_se_trips %>%
		mutate(
			purpose = pretty_purpose(purpose),
			model = pretty_model(model),
			mode = pretty_mode(mode)
		) %>%
		ggplot(aes(x = income, weight = trips, color = model)) +
		facet_grid(cols = vars(mode), rows = vars(purpose), scales = "free") +
		scale_x_continuous(limits = c(NA,2e5), labels = label_currency()) +
		geom_density() +
		labs(x = "Income", y = "Kernel density", color = "Model")
}

plot_asim_mode_switching <- function(mode_switching) {
	mode_switching %>%
		mutate(
			from = mode_by,
			to = mode_tr,
			tour_purpose = case_match(
				tour_purpose,
				c("work", "school", "univ") ~ tour_purpose,
				c("school", "univ") ~ "school",
				.default = "other"
			)
		) %>%
		count(tour_purpose, from, to) %>%
		filter(from != "rh", to != "rh") %>%
		mutate(
			tour_purpose = factor(
				tour_purpose,
				levels = c("work", "school", "univ", "other"),
				labels = c("Work", "School", "University", "Other")
			),
			across(c(from, to), \(x) pretty_mode(x))) %>%
		ggplot(aes(axis1 = from, axis2 = to, y = n)) +
		list(if(length(unique(mode_switching$tour_purpose)) > 1) facet_wrap(~tour_purpose, scales = "free")) +
		# facet_wrap(~tour_purpose, scales = "free") +
		scale_x_discrete(
			limits = c("From", "To"),
			labels = c("Baseline\nScenario", "Improved\nTransit"),
			expand = c(.2, .1)) +
		geom_alluvium(aes(fill = from), width = 1/2, alpha = 0.8) +
		geom_stratum(width = 1/2) +
		geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
		# scale_fill_brewer(palette = "Accent") +
		scale_fill_bright() +
		labs(x = element_blank(), y = "Trips", fill = "Original mode")
}


make_combined_mode_split_table <- function(df) {
	df %>%
		mutate(
			purpose = pretty_purpose(purpose),
			mode = pretty_mode(mode),
			across(c(cube_tr, cube_by),round),
			across(contains("pct"), label_percent(accuracy = 0.1))
		) %>%
		relocate(purpose, mode, cube_by, cube_tr, cube_diff_pct, asim_by, asim_tr, asim_diff_pct) %>%
		arrange(purpose, mode)
}

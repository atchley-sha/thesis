plot_asim_mode_switching <- function(table) {
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
	format_line <- fr_line %>%
		mutate(Year = factor(Year, levels = c("2019", "2050")))
	format_stops <- fr_stops %>%
		mutate(Year = factor(Year, levels = c("2019", "2050")))
	text_stops <- format_stops %>%
		bind_cols(st_coordinates(.) %>% as_tibble())

	trips <- trip_diff %>%
		left_join(dist_transl, join_by(origin == TAZ)) %>%
		group_by(purpose, mode, DIST) %>%
		summarise(diff = sum(diff)) %>%
		mutate(mode = pretty_mode(mode)) %>%
		filter(!round(diff) == 0) %>%
		left_join(dist_geom, join_by(DIST)) %>%
		st_as_sf()

	ggplot() +
		facet_wrap(~mode) +
		annotation_map_tile("cartolight", zoom = 10) +
		geom_sf(aes(fill = diff), data = trips) +
		scale_fill_gradient2(limits = c(-50, 50), oob = oob_squish) +
		# new_scale_fill() +
		geom_sf(aes(color = Year), linewidth = 1, data = format_line) +
		geom_sf(aes(color = Year), size = 3, data = format_stops) +
		# geom_label_repel(
		# 	aes(label = Name, geometry = geometry, fill = Year),
		# 	box.padding = 0.4, point.padding = 0.5,
		# 	nudge_x = 0.04, show.legend = FALSE,
		# 	stat = "sf_coordinates", data = format_stops) +
		# coord_sf(xlim = c(-112.3, -111.4), crs = st_crs(4326), expand = FALSE) +
		# scale_fill_manual(values = c("2019" = "white", "2050" = "grey90")) +
		scale_color_manual(
			values = c("2019" = "black", "2050" = "coral"),
			labels = c("2019" = "Existing FrontRunner", "2050" = "Improved FrontRunner (addt'l)")) +
		labs(color = element_blank(), fill = "Change in trips by\nproduction district") +
		theme_map(zoom = FALSE)
}

plot_tr_new_transit_income_dist <- function(cube, asim) {
	cube_cleaned <- cube %>%
		filter(mode == "transit") %>%
		select(purpose, trips, income = med_income)
	asim_cleaned <- asim %>%
		mutate(
			mode = convert_asim_mode(mode),
			purpose = make_asim_purpose(
				select(., tour_id, tour_purpose, trip_purpose))
		) %>%
		filter(mode == "transit") %>%
		mutate(purpose, income, trips = 1, .keep = "none")

	bind_rows(cube = cube_cleaned, asim = asim_cleaned, .id = "model") %>%
		mutate(
			purpose = pretty_purpose(purpose),
			mode = pretty_mode(mode),
			model = pretty_model(model)
		) %>%
		ggplot(aes(x = income, weight = trips, color = model)) +
		facet_wrap(~purpose, scales = "free", ncol = 1) +
		geom_density()

}

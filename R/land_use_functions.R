get_asim_new_persons <- function(per_new, per_by, tazs) {
	per_new %>%
		filter(
			home_zone_id %in% tazs,
			!person_id %in% (per_by %>%
				filter(home_zone_id %in% tazs) %>%
					pull(person_id))
		) %>%
		pull(person_id %>% unique())
}

get_asim_lu_new_trips <- function(trips_raw, persons) {
	trips_raw %>%
		filter(person_id %in% persons) %>%
		count_asim_trips()
}

plot_asim_lu_desire_lines <- function(lines, dists_list, dists_geom) {
	lines %>%
		filter(trips != 0) %>%
		mutate(
			in_zone = (origin %in% dists_list | destination %in% dists_list)
		) %>%
		st_transform(4326) %>%
		ggplot(aes(linewidth = trips, color = in_zone)) +
		facet_wrap(~mode) +
		annotation_map_tile("cartolight", zoomin = 1) +
		geom_sf(data = dists_geom, fill = NA, color = "black", inherit.aes = FALSE) +
		geom_sf() +
		scale_linewidth_continuous(range = c(0.1,3), limits = c(NA,1000)) +
		# lims(x = lims$x, y = lims$y) +
		labs(color = "Produced in new\ndevelopment", linewidth = "Trips")
}

plot_cube_lu_desire_lines <- function(lines, dists_geom) {
	lines %>%
		filter(trips != 0) %>%
		st_transform(4326) %>%
		ggplot(aes(linewidth = abs(trips), color = as.character(sign(trips)))) +
		facet_wrap(~mode) +
		annotation_map_tile("cartolight", zoomin = 1) +
		geom_sf(data = dists_geom, fill = NA, color = "black", inherit.aes = FALSE) +
		geom_sf() +
		scale_linewidth_continuous(range = c(0.1,3), limits = c(NA,1000)) +
		scale_color_manual(
			values = c("-1" = "red", "1" = "navy"),
			labels = c("-1" = "Base year", "1" = "Land use")) +
		# lims(x = lims$x, y = lims$y) +
		labs(color = "More trips in:", linewidth = "Trips")
}

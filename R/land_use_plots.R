.lu_desire_bg <- function(dists_geom, max, range = c(0,3)) {
	list(
		facet_wrap(~mode),
		annotation_map_tile("cartolight", zoomin = 0),
		geom_sf(data = dists_geom, fill = NA, color = "black", inherit.aes = FALSE),
		geom_sf(),
		scale_linewidth_continuous(
			range = range, limits = c(NA,max), guide = guide_legend(order = 99)),
		theme_map()
	)
}

plot_asim_lu_desire_lines <- function(desire_lines, dists_list, dists_geom) {
	max <- 400
	desire_lines %>%
		filter(round(trips) > 0) %>%
		mutate(
			in_zone = (origin %in% dists_list | destination %in% dists_list),
			trips = pmin(trips, max),
			mode = pretty_mode(mode)
		) %>%
		ggplot(aes(linewidth = trips, color = in_zone)) +
		.lu_desire_bg(dists_geom, max) +
		guides(color = guide_legend(reverse=TRUE)) +
		labs(color = "Produced in new\ndevelopment", linewidth = "Trips")
}

plot_cube_lu_desire_lines <- function(desire_lines, dists_geom) {
	max <- 400
	desire_lines %>%
		filter(round(diff) > 0) %>%
		mutate(
			mode = pretty_mode(mode),
			diff = pmin(diff, max)
		) %>%
		ggplot(aes(linewidth = diff)) +
		.lu_desire_bg(dists_geom, max) +
		labs(color = "More trips in:", linewidth = "Trips")
}

plot_cube_lu_nhb_desire_lines <- function(desire_lines, dists_geom) {
	max <- 400
	desire_lines %>%
		filter(abs(round(diff)) > 0) %>%
		mutate(
			mode = pretty_mode(mode),
			diff = if_else(diff > 0, pmin(diff, max), pmax(diff, -max))
		) %>%
		# filter(diff < 0) %>%
		ggplot(aes(linewidth = abs(diff), color = as.character(sign(diff)))) +
		.lu_desire_bg(dists_geom, max) +
		scale_color_manual(
			values = c("-1" = "red", "1" = "navy"),
			labels = c("-1" = "Base year", "1" = "Land use")) +
		# scale_color_bright(reverse = TRUE, labels = c("-1" = "Base year", "1" = "Land use")) +
		labs(color = "More trips in:", linewidth = "Trips")
}

plot_cube_lu_new_pmt <- function(trips_diff, distances, lu_tazs) {
	trips_diff %>%
		filter(origin %in% lu_tazs | destination %in% lu_tazs) %>%
		left_join(distances) %>%
		group_by(purpose, mode) %>%
		summarise(pmt = sum(diff*distance), .groups = "drop") %>%
		mutate(
			purpose = pretty_purpose(purpose),
			mode = pretty_mode(mode)) %>%
		ggplot(aes(x = pmt, y = purpose, fill = mode)) +
		geom_col(position = position_dodge2(preserve = "single", reverse = TRUE))+
		labs(
			x = "Total Person-Miles Traveled, Trips Produced in Updated Zones",
			y = "Trip Purpose",
			fill = "Mode") +
		scale_x_continuous(
			trans = sqrt_trans(),
			breaks = c(0, 1e3, 1e4, 5e4, 1e5, 2e5, 5e5),
			labels = label_comma(),
			expand = expansion(c(0,0.05))) +
		scale_y_discrete(expand = expansion(0, 0.5), limits = rev) +
		theme(axis.ticks.y = element_blank())
}

plot_asim_lu_pmt <- function(raw_trips, persons, distances, lu_tazs) {
	raw_trips %>%
		filter(person_id %in% persons) %>%
		count_asim_trips_keep_purpose() %>%
		left_join(distances) %>%
		mutate(in_zone = origin %in% lu_tazs | destination %in% lu_tazs) %>%
		group_by(purpose, mode, in_zone) %>%
		summarise(pmt = sum(trips*distance), .groups = "drop") %>%
		mutate(
			purpose = str_to_title(purpose),
			mode = pretty_mode(mode)) %>%
		ggplot(aes(x = pmt, y = mode, fill = mode, alpha = as.character(in_zone))) +
		facet_grid(
			rows = vars(purpose),
			switch = "y", scales = "free_y", space = "free_y") +
		geom_col(position = position_stack()) +
		labs(
			x = "Total Person-Miles Traveled",
			y = "Tour Purpose",
			fill = "Mode") +
		scale_x_continuous(
			trans = sqrt_trans(),
			breaks = c(0, 1e3, 1e4, 5e4, 1e5, 2e5, 5e5),
			labels = label_comma(),
			expand = expansion(c(0,0.05))) +
		scale_y_discrete(limits = rev) +
		scale_alpha_manual(
			name = "O/D in new\ndevelopment",
			values = c("TRUE" = 1, "FALSE" = 0.4)) +
		guides(alpha = guide_legend(reverse = TRUE)) +
		theme(
			panel.grid.major.y = element_blank(),
			panel.spacing = unit(0, "lines"),
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			strip.background = element_blank(),
			strip.text.y.left = element_text(angle = 0)
		)
}

plot_lu_new_tazs <- function(taz_geom, taz_list) {
	taz_geom %>%
		filter(TAZ %in% taz_list) %>%
		# left_join(se_diff, join_by(TAZ)) %>%
		# filter(name %in% c("TOTHH", "HHPOP", "TOTEMP")) %>%
		ggplot() +
		annotation_map_tile("cartolight", zoomin = 0) +
		annotation_north_arrow(
			style = north_arrow_fancy_orienteering, location = "tl") +
		annotation_scale(
			unit_category = "imperial", location = "tl", pad_y = unit(2, "cm")) +
		geom_sf(fill = NA, linewidth = 2, color = "black") +
		geom_sf_label(aes(label = TAZ)) +
		# coord_sf(
		# 	xlim = c(-111.93, -111.88), ylim = c(40.47, 40.53),
		# 	crs = 4326) +
		# theme_map(zoom = FALSE)
		theme_map(lims = list(x = c(-111.93, -111.88), y = c(40.47, 40.53)))
}

make_draper_prison_site_map <- function(taz, lu_tazs) {
	bbox <- get_draper_prison_site_inset(taz, lu_tazs)
	mainplot <- taz %>%
		filter(TAZ %in% lu_tazs) %>%
		st_union() %>%
		st_transform(4326) %>%
		ggplot() +
		annotation_map_tile("cartolight", zoom = 11) +
		geom_sf(color = "blue", fill = NA, linewidth = 2) +
		# geom_rect(
		# 	xmin = bbox["xmin"], xmax = bbox["xmax"],
		# 	ymin = bbox["ymin"], ymax = bbox["ymax"],
		# 	fill = NA, color = "black", linewidth = 1
		# ) +
		coord_sf(
			xlim = c(-112.1,-111.7), ylim = c(40.4, 40.8),
			expand = FALSE) +
		theme_map(zoom = FALSE)

	mainplot

# 	cowplot::ggdraw(mainplot) +
# 		cowplot::draw_plot(
# 			{mainplot +
# 					coord_sf(
# 						xlim = bbox[c(1,3)],
# 						ylim = bbox[c(2,4)]
# 					)},
# 1.4, .4, .2, .2
# 			# scale = 0.2
# 		)

}

get_draper_prison_site_inset <- function(taz, lu_tazs) {
	taz %>%
		filter(TAZ %in% lu_tazs) %>%
		st_union() %>%
		st_buffer(unit(1e3, "mm")) %>%
		st_transform(4326) %>%
		st_bbox()
}

plot_gateway_and_prison <- function() {
	from <- "49035114000"
	to <- "49035112818"

	st_read("data/census_tracts.geojson") %>%
		mutate(type = case_match(
			TRACTID,
			from ~ "Gateway",
			to ~ "The Point"
		)) %>%
		filter(TRACTID %in% c(from, to)) %>%
		st_transform(4326) %>%
		ggplot() +
		annotation_map_tile("cartolight", zoom = 11) +
		geom_sf(aes(color = type), fill = NA, linewidth = 2) +
		geom_label_repel(
			aes(label = type, geometry = geometry),
			box.padding = 0.4, point.padding = 0.5,
			nudge_x = -0.10, nudge_y = -0.01, show.legend = FALSE,
			stat = "sf_coordinates") +
		coord_sf(
			xlim = c(-112.1,-111.7), ylim = c(40.4, 40.8),
			expand = FALSE) +
		scale_color_manual(values = c("Gateway" = "red", "The Point" = "blue")) +
		guides(color = "none") +
		theme_map(zoom = FALSE)
}

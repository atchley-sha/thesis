#' @export
plot_frontrunner <- function(line, stops) {
	format_line <- line %>%
		mutate(Year = factor(Year, levels = c("2019", "2050")))
	format_stops <- stops %>%
		mutate(Year = factor(Year, levels = c("2019", "2050")))
	text_stops <- format_stops %>%
		bind_cols(st_coordinates(.) %>% as_tibble())

	ggplot() +
		annotation_map_tile("cartolight", zoom = 10) +
		geom_sf(aes(color = Year), linewidth = 1, data = format_line) +
		geom_sf(aes(color = Year), size = 3, data = format_stops) +
		geom_label_repel(
			aes(label = Name, geometry = geometry, fill = Year),
			box.padding = 0.4, point.padding = 0.5,
			nudge_x = 0.04, show.legend = FALSE,
			stat = "sf_coordinates", data = format_stops) +
		coord_sf(
			xlim = c(-112.3, -111.4), ylim = c(39.9,41.3),
			crs = st_crs(4326), expand = FALSE) +
		scale_fill_manual(values = c("2019" = "white", "2050" = "grey90")) +
		scale_color_manual(
			values = c("2019" = "purple4", "2050" = "coral"),
			labels = c("2019" = "Existing", "2050" = "Improved Transit (addt'l)")) +
		labs(color = "Scenario") +
		theme_map(zoom = FALSE)

}

.add_frontrunner_to_plot <- function(fr_line, fr_stops) {
	format_line <- fr_line %>%
		mutate(Year = factor(Year, levels = c("2019", "2050")))
	format_stops <- fr_stops %>%
		mutate(Year = factor(Year, levels = c("2019", "2050")))
	text_stops <- format_stops %>%
		bind_cols(st_coordinates(.) %>% as_tibble())

	list(
		ggnewscale::new_scale_color(),
		ggnewscale::new_scale_fill(),
		geom_sf(aes(color = Year), linewidth = 1, data = format_line),
		geom_sf(aes(color = Year), size = 3, data = format_stops),
		# geom_label_repel(
		# 	aes(label = Name, geometry = geometry, fill = Year),
		# 	box.padding = 0.4, point.padding = 0.5,
		# 	nudge_x = 0.04, show.legend = FALSE,
		# 	stat = "sf_coordinates", data = format_stops),
		scale_fill_manual(values = c("2019" = "white", "2050" = "grey90"), guide = "none"),
		scale_color_manual(
			values = c("2019" = "black", "2050" = "coral"),
			labels = c("2019" = "Existing FrontRunner", "2050" = "Improved FrontRunner (addt'l)"),
			name = NULL)
	)
}

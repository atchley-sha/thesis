library(targets)
tar_source()

tar_make(
	c(
		asim_lu_new_desire_lines,
		lu_distsml,
		distsml,
		asim_lu_raw_trips,
		asim_lu_new_persons,
		taz_distsml_transl,
		distsml_centroids,
		asim_nonres_desire,
	)
)
tar_load(any_of(
	c(
		"asim_lu_new_desire_lines",
		"lu_distsml",
		"distsml",
		"asim_lu_raw_trips",
		"asim_lu_new_persons",
		"taz_distsml_transl",
		"distsml_centroids",
		"asim_nonres_desire",
	)
))

desire_lines_res <- asim_lu_new_desire_lines %>%
	filter(round(trips) > 0) %>%
	group_by(origin, destination) %>%
	summarise(trips = sum(trips)) %>%
	mutate(
		in_zone = (origin %in% lu_distsml | destination %in% lu_distsml),
		trips = pmin(trips, 800),
		resident = TRUE
	) %>%
	filter(!in_zone)

desire_lines_nonres <- asim_nonres_desire %>%
	filter(round(trips) > 0) %>%
	group_by(origin, destination) %>%
	summarise(trips = sum(trips)) %>%
	mutate(
		trips = pmin(trips, 800),
		resident = FALSE
	)

desire_combined <- bind_rows(desire_lines_res, desire_lines_nonres)

desire_combined %>%
	mutate(resident = case_when(
		resident ~ "New Resident Non\u2013Home-Based Trips",
		!resident ~ "Non-Resident Trips to New Development")) %>%
	ggplot(aes(linewidth = trips)) +
	facet_wrap(~resident) +
	annotation_map_tile("cartolight", zoomin = 0) +
	geom_sf(
		data = distsml,
		fill = NA,
		color = "black",
		inherit.aes = FALSE
	) +
	geom_sf() +
	scale_linewidth_continuous(
		range = range,
		limits = c(NA, 800),
		guide = guide_legend(order = 99)
	) +
	theme_map() +
	labs(linewidth = "Trips")


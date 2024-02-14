#### Setup #############################################################

library(targets)
library(tarchetypes)

package_list <- c(
	"qs",
	"tidyverse",
	"scales",
	"sf",
	"omxr",
	"ggspatial",
	"ggalluvial",
	"ggrepel",
	"DiagrammeR",
	"od"
)

tar_option_set(
	packages = package_list
	# memory = "transient",
	# garbage_collection = TRUE,
	# format = "qs",
)

tar_source("R")

tar_seed_set(34985723)

#### List targets ######################################################

# Shared data targets ####
shared_data_targets <- tar_plan(
	tar_file(taz_file, "data/WFRC_TAZ.geojson"),
	taz = read_taz_file(taz_file),
	distsml = get_sml_districts_from_taz_file(taz_file),
	distmed = get_med_districts_from_taz_file(taz_file),
	taz_distsml_transl = make_taz_distsml_transl(taz_file),
	taz_distmed_transl = make_taz_distmed_transl(taz_file),
	taz_centroids = sf::st_centroid(taz),
	distsml_centroids = sf::st_centroid(distsml),
	distmed_centroids = sf::st_centroid(distmed),

	tar_file(income_groups_file, "data/income_groups.csv"),
	income_groups = read_income_groups(income_groups_file),

	tar_file(distance_skims_file, "data/cube/skm_DY_Dist.omx"),
	distances = read_distance_skims(distance_skims_file),


)

# CUBE data targets ####
cube_data_targets <- tar_plan(
	# SE
	tar_file(cube_taz_se_file, "data/cube/TAZ_SE_2019_WFRC.csv"),
	cube_taz_se = read_cube_taz_se_file(cube_taz_se_file),
	tar_file(cube_taz_inc_groups_file, "data/cube/Marginal_Income.csv"),
	cube_taz_inc_groups = read_cube_taz_inc_groups_file(cube_taz_inc_groups_file),

	# Base year
	tar_file(cube_by_hbw_omx, "data/cube/output/base_2019/HBW_trips_allsegs_pkok.omx"),
	cube_by_hbw = read_trip_matrix(cube_by_hbw_omx),
	tar_file(cube_by_hbo_omx, "data/cube/output/base_2019/HBO_trips_allsegs_pkok.omx"),
	cube_by_hbo = read_trip_matrix(cube_by_hbo_omx),
	tar_file(cube_by_nhb_omx, "data/cube/output/base_2019/NHB_trips_allsegs_pkok.omx"),
	cube_by_nhb = read_trip_matrix(cube_by_nhb_omx),
	all_cube_by_trips = dplyr::bind_rows(
		list(
			hbw = cube_by_hbw,
			hbo = cube_by_hbo,
			nhb = cube_by_nhb
		),
		.id = "purpose"
	),

	# Land Use
	tar_file(cube_lu_hbw_omx, "data/cube/output/land_use/HBW_trips_allsegs_pkok.omx"),
	cube_lu_hbw = read_trip_matrix(cube_lu_hbw_omx),
	tar_file(cube_lu_hbo_omx, "data/cube/output/land_use/HBO_trips_allsegs_pkok.omx"),
	cube_lu_hbo = read_trip_matrix(cube_lu_hbo_omx),
	tar_file(cube_lu_nhb_omx, "data/cube/output/land_use/NHB_trips_allsegs_pkok.omx"),
	cube_lu_nhb = read_trip_matrix(cube_lu_nhb_omx),
	all_cube_lu_trips = dplyr::bind_rows(
		list(
			hbw = cube_lu_hbw,
			hbo = cube_lu_hbo,
			nhb = cube_lu_nhb
		),
		.id = "purpose"
	),

	# Transit
	tar_file(cube_tr_hbw_omx, "data/cube/output/transit/HBW_trips_allsegs_pkok.omx"),
	cube_tr_hbw = read_trip_matrix(cube_tr_hbw_omx),
	tar_file(cube_tr_hbo_omx, "data/cube/output/transit/HBO_trips_allsegs_pkok.omx"),
	cube_tr_hbo = read_trip_matrix(cube_tr_hbo_omx),
	tar_file(cube_tr_nhb_omx, "data/cube/output/transit/NHB_trips_allsegs_pkok.omx"),
	cube_tr_nhb = read_trip_matrix(cube_tr_nhb_omx),
	all_cube_tr_trips = dplyr::bind_rows(
		list(
			hbw = cube_tr_hbw,
			hbo = cube_tr_hbo,
			nhb = cube_tr_nhb
		),
		.id = "purpose"
	),

	# WFH
	tar_file(cube_wfh_hbw_omx, "data/cube/output/wfh/HBW_trips_allsegs_pkok.omx"),
	cube_wfh_hbw = read_trip_matrix(cube_wfh_hbw_omx),
	tar_file(cube_wfh_hbo_omx, "data/cube/output/wfh/HBO_trips_allsegs_pkok.omx"),
	cube_wfh_hbo = read_trip_matrix(cube_wfh_hbo_omx),
	tar_file(cube_wfh_nhb_omx, "data/cube/output/wfh/NHB_trips_allsegs_pkok.omx"),
	cube_wfh_nhb = read_trip_matrix(cube_wfh_nhb_omx),
	all_cube_wfh_trips = dplyr::bind_rows(
		list(
			hbw = cube_wfh_hbw,
			hbo = cube_wfh_hbo,
			nhb = cube_wfh_nhb
		),
		.id = "purpose"
	),
)

# ASIM data targets ####
asim_data_targets <- tar_plan(
	tar_file(asim_by_trips_file, "data/asim/output/base_2019/final_trips.csv.gz"),
	tar_file(asim_by_tours_file, "data/asim/output/base_2019/final_tours.csv.gz"),
	tar_file(asim_by_per_file, "data/asim/output/base_2019/final_persons.csv.gz"),
	tar_file(asim_by_hh_file, "data/asim/output/base_2019/final_households.csv.gz"),
)

#### Run all targets ###################################################

tar_plan(
	shared_data_targets,
	cube_data_targets,
	asim_data_targets,
)

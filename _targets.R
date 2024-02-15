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
	packages = package_list,
	# memory = "transient",
	# garbage_collection = TRUE,
	format = "qs",
)

tar_source("R")

tar_seed_set(34985723)

#### List targets ######################################################

# Misc ####
misc_targets <- tar_plan(
	map_lims_slc = list(x = c(-112.15,-111.6), y = c(40.2,40.8)),
	lu_tazs = c(2138, 2140, 2141, 2149, 2170),
	lu_distsml = get_dist_from_tazs(lu_tazs, taz_distsml_transl),
	lu_distmed = get_dist_from_tazs(lu_tazs, taz_distmed_transl),
)

# FrontRunner ####
frontrunner_targets <- tar_plan(
	tar_file(frontrunner_line_file, "data/frontrunner_line.geojson"),
	tar_file(frontrunner_stops_file, "data/frontrunner_stops.geojson"),
	frontrunner_line = sf::st_read(frontrunner_line_file),
	frontrunner_stops = sf::st_read(frontrunner_stops_file),
	frontrunner_plot = plot_frontrunner(frontrunner_line, frontrunner_stops),
)

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

	tar_file(job_code_translation_file, "data/job_code_translation.csv"),
	job_code_translation = readr::read_csv(job_code_translation_file),
)

# CUBE data targets ####
cube_data_targets <- tar_plan(
	# SE
	tar_file(cube_by_taz_se_file, "data/cube/TAZ_SE_2019_WFRC.csv"),
	cube_by_taz_se = read_cube_taz_se_file(cube_by_taz_se_file),
	tar_file(cube_lu_taz_se_file, "data/cube/SE_prison.csv"),
	cube_lu_taz_se = read_cube_taz_se_file(cube_lu_taz_se_file),
	tar_file(cube_by_taz_inc_groups_file, "data/cube/Marginal_Income.csv"),
	cube_by_taz_inc_groups = read_cube_taz_inc_groups_file(cube_by_taz_inc_groups_file),
	cube_by_combined_se = combine_cube_se(cube_by_taz_se, cube_by_taz_inc_groups),

	# Calibration
	tar_file(
		cube_telecommute_percentages_file,
		"data/cube/telecommute_jobtype.csv"),
	cube_telecommute_percentages = readr::read_csv(cube_telecommute_percentages_file),

	# Base year
	tar_file(cube_by_hbw_omx, "data/cube/output/base_2019/HBW_trips_allsegs_pkok.omx"),
	cube_by_hbw = read_trip_matrix(cube_by_hbw_omx),
	tar_file(cube_by_hbo_omx, "data/cube/output/base_2019/HBO_trips_allsegs_pkok.omx"),
	cube_by_hbo = read_trip_matrix(cube_by_hbo_omx),
	tar_file(cube_by_nhb_omx, "data/cube/output/base_2019/NHB_trips_allsegs_pkok.omx"),
	cube_by_nhb = read_trip_matrix(cube_by_nhb_omx),
	cube_by_trips = dplyr::bind_rows(
		list(
			hbw = cube_by_hbw,
			hbo = cube_by_hbo,
			nhb = cube_by_nhb),
		.id = "purpose"),

	# Land Use
	tar_file(cube_lu_hbw_omx, "data/cube/output/land_use/HBW_trips_allsegs_pkok.omx"),
	cube_lu_hbw = read_trip_matrix(cube_lu_hbw_omx),
	tar_file(cube_lu_hbo_omx, "data/cube/output/land_use/HBO_trips_allsegs_pkok.omx"),
	cube_lu_hbo = read_trip_matrix(cube_lu_hbo_omx),
	tar_file(cube_lu_nhb_omx, "data/cube/output/land_use/NHB_trips_allsegs_pkok.omx"),
	cube_lu_nhb = read_trip_matrix(cube_lu_nhb_omx),
	cube_lu_trips = dplyr::bind_rows(
		list(
			hbw = cube_lu_hbw,
			hbo = cube_lu_hbo,
			nhb = cube_lu_nhb),
		.id = "purpose"),

	# Transit
	tar_file(cube_tr_hbw_omx, "data/cube/output/transit/HBW_trips_allsegs_pkok.omx"),
	cube_tr_hbw = read_trip_matrix(cube_tr_hbw_omx),
	tar_file(cube_tr_hbo_omx, "data/cube/output/transit/HBO_trips_allsegs_pkok.omx"),
	cube_tr_hbo = read_trip_matrix(cube_tr_hbo_omx),
	tar_file(cube_tr_nhb_omx, "data/cube/output/transit/NHB_trips_allsegs_pkok.omx"),
	cube_tr_nhb = read_trip_matrix(cube_tr_nhb_omx),
	cube_tr_trips = dplyr::bind_rows(
		list(
			hbw = cube_tr_hbw,
			hbo = cube_tr_hbo,
			nhb = cube_tr_nhb),
		.id = "purpose"),

	# WFH
	tar_file(cube_wfh_hbw_omx, "data/cube/output/wfh/HBW_trips_allsegs_pkok.omx"),
	cube_wfh_hbw = read_trip_matrix(cube_wfh_hbw_omx),
	tar_file(cube_wfh_hbo_omx, "data/cube/output/wfh/HBO_trips_allsegs_pkok.omx"),
	cube_wfh_hbo = read_trip_matrix(cube_wfh_hbo_omx),
	tar_file(cube_wfh_nhb_omx, "data/cube/output/wfh/NHB_trips_allsegs_pkok.omx"),
	cube_wfh_nhb = read_trip_matrix(cube_wfh_nhb_omx),
	cube_wfh_trips = dplyr::bind_rows(
		list(
			hbw = cube_wfh_hbw,
			hbo = cube_wfh_hbo,
			nhb = cube_wfh_nhb),
		.id = "purpose"),
)

# ASIM data targets ####
asim_data_targets <- tar_plan(
	# Calibration
	tar_files(
		asim_mode_choice_calibration_iters_files,
		list.files(
			"data/calibration/mode_choice",
			full.names = TRUE, pattern = ".*\\.csv")),
	tar_target(
		asim_mode_choice_calibration_iters,
		combine_asim_mode_choice_calibration_iters(
			asim_mode_choice_calibration_iters_files),
		pattern = map(asim_mode_choice_calibration_iters_files)),
	tar_file(
		asim_by_telecommute_coefficients_file,
		"data/asim/asim_tc_coeffs_2019.csv"
	),
	asim_by_telecommute_coefficients = read_asim_telecommute_coefficients(
		asim_by_telecommute_coefficients_file
	),

	# Base year
	tar_file(asim_by_trips_file, "data/asim/output/base_2019/final_trips.csv.gz"),
	tar_file(asim_by_tours_file, "data/asim/output/base_2019/final_tours.csv.gz"),
	tar_file(asim_by_per_file, "data/asim/output/base_2019/final_persons.csv.gz"),
	tar_file(asim_by_hh_file, "data/asim/output/base_2019/final_households.csv.gz"),

	asim_by_raw_trips = read_asim_trips_file(asim_by_trips_file),
	asim_by_trips = count_asim_trips(asim_by_raw_trips),
	asim_by_per = readr::read_csv(asim_by_per_file),
	asim_by_raw_hh = readr::read_csv(asim_by_hh_file),
	asim_by_hh_taz = summarise_asim_hh(asim_by_raw_hh, income_groups),

	# Land use
	tar_file(asim_lu_trips_file, "data/asim/output/land_use/final_trips.csv.gz"),
	tar_file(asim_lu_tours_file, "data/asim/output/land_use/final_tours.csv.gz"),
	tar_file(asim_lu_per_file, "data/asim/output/land_use/final_persons.csv.gz"),
	tar_file(asim_lu_hh_file, "data/asim/output/land_use/final_households.csv.gz"),

	asim_lu_raw_trips = read_asim_trips_file(asim_lu_trips_file),
	asim_lu_trips = count_asim_trips(asim_lu_raw_trips),
	asim_lu_per = readr::read_csv(asim_lu_per_file),

	# Transit
	tar_file(asim_tr_trips_file, "data/asim/output/transit/final_trips.csv.gz"),
	tar_file(asim_tr_tours_file, "data/asim/output/transit/final_tours.csv.gz"),
	tar_file(asim_tr_per_file, "data/asim/output/transit/final_persons.csv.gz"),
	tar_file(asim_tr_hh_file, "data/asim/output/transit/final_households.csv.gz"),

	asim_tr_raw_trips = read_asim_trips_file(asim_tr_trips_file),
	asim_tr_trips = count_asim_trips(asim_tr_raw_trips),

	# WFH
	tar_file(asim_wfh_trips_file, "data/asim/output/wfh/final_trips.csv.gz"),
	tar_file(asim_wfh_tours_file, "data/asim/output/wfh/final_tours.csv.gz"),
	tar_file(asim_wfh_per_file, "data/asim/output/wfh/final_persons.csv.gz"),
	tar_file(asim_wfh_hh_file, "data/asim/output/wfh/final_households.csv.gz"),

	asim_wfh_raw_trips = read_asim_trips_file(asim_wfh_trips_file),
	asim_wfh_trips = count_asim_trips(asim_wfh_raw_trips),
)

# Base year ####
base_year_targets <- tar_plan(
	# Data
	combined_by_trips = dplyr::bind_rows(
		list(asim = asim_by_trips, cube = cube_by_trips),
		.id = "model"),
	combined_by_trips_sampled = sample_trips(
		combined_by_trips, prop = 0.1, weight = FALSE),

	combined_by_se_data = combine_se_data(
		list(cube = cube_by_combined_se, asim = asim_by_hh_taz)),
	combined_by_se_data_distsml = summarise_combined_se_data(
		combined_by_se_data, taz_distsml_transl, distsml
	),

	# Population
	comparison_pop_map = make_pop_comparison_map(
		combined_by_se_data_distsml),
	comparison_med_income_map = make_med_income_comparison_map(
		combined_by_se_data_distsml),
	comparison_inc_groups_map = make_inc_groups_comparison_map(
		combined_by_se_data_distsml, income_groups),
	comparison_inc_density_plot = make_inc_density_comparison_plot(
		combined_by_se_data_distsml, income_groups),

	# Mode choice
	by_mode_split_comparison = compare_mode_split(combined_by_trips),
	asim_mode_choice_calibration_plot = plot_asim_mode_choice_calibration(
		asim_mode_choice_calibration_iters),

	# TLFD
	combined_by_tlfd_plot = plot_combined_tlfd(
		combined_by_trips_sampled, distances),

	# WFH
	comparison_by_telecommute_coeffs = compare_telecommute(
		cube_telecommute_percentages[c("jobcode", "wfrc_2019")],
		asim_by_telecommute_coefficients,
		job_code_translation
	),
)

# Land use ####
land_use_targets <- tar_plan(
	# PMT/VMT



	# Desire lines
	cube_lu_nhb_diff_distsml = get_trip_diff(list(
		lu = sum_trips_by_district(cube_lu_trips, taz_distsml_transl),
		by = sum_trips_by_district(cube_by_trips, taz_distsml_transl))),
	cube_lu_nhb_diff_desire = od::od_to_sf(
		cube_lu_nhb_diff_distsml, distsml_centroids),
	cube_lu_nhb_diff_desire_map = plot_cube_lu_desire_lines(
		cube_lu_nhb_diff_desire, distsml),

	asim_lu_new_persons = get_asim_new_persons(asim_lu_per, asim_by_per, lu_tazs),
	asim_lu_new_trips = get_asim_lu_new_trips(asim_lu_raw_trips, asim_lu_new_persons),
	asim_lu_new_trips_distsml = sum_trips_by_district(
		asim_lu_new_trips, taz_distsml_transl),
	asim_lu_new_desire_lines = od::od_to_sf(
		asim_lu_new_trips_distsml, distsml_centroids),
	asim_lu_new_desire_map = plot_asim_lu_desire_lines(
		asim_lu_new_desire_lines, lu_distsml, distsml),
)

# Transit ####
transit_targets <- tar_plan(

)

# WFH ####
wfh_targets <- tar_plan(

)

#### Run all targets ###################################################

tar_plan(
	misc_targets,
	frontrunner_targets,
	shared_data_targets,
	cube_data_targets,
	asim_data_targets,

	base_year_targets,
	land_use_targets,
	transit_targets,
	wfh_targets,
)

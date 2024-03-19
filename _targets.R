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
	"ggh4x",
	"ggnewscale",
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
ggplot2::theme_set(ggplot2::theme_bw())

#### List targets ######################################################
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

	# tar_file(cube_time_spent_table_file, "data/cube_time_spent.csv"),
	# cube_time_spent_table = readr::read_csv(cube_time_spent_table_file),
	tar_file(asim_time_spent_table_file, "data/asim_time_spent.csv"),
	asim_time_spent_table = readr::read_csv(asim_time_spent_table_file),
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

	tar_file(cube_remote_work_totals_file, "data/cube/ControlTotal_WorkAtHome.csv"),
	cube_remote_work_totals = read_cube_remote_work_totals(
		cube_remote_work_totals_file),
	cube_remote_work_totals_plot = plot_cube_remote_work_totals(
		cube_remote_work_totals),

	# Calibration
	tar_file(
		cube_remote_work_percentages_file,
		"data/cube/remote_work_jobtype.csv"),
	cube_remote_work_percentages = read_cube_rw_percentages(
		cube_remote_work_percentages_file, job_code_translation),
	cube_by_telecommute_percentages = dplyr::filter(
		cube_remote_work_percentages, year == 2019, type == "tc"),
	cube_wfh_telecommute_percentages = dplyr::filter(
		cube_remote_work_percentages, year == 2050, type == "tc"),

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
		"data/asim/asim_tc_coeffs_2019.csv"),
	asim_by_telecommute_coefficients = read_asim_telecommute_coefficients(
		asim_by_telecommute_coefficients_file, job_code_translation),
	tar_file(
		asim_wfh_telecommute_coefficients_file,
		"data/asim/asim_tc_coeffs_2050.csv"),
	asim_wfh_telecommute_coefficients = read_asim_telecommute_coefficients(
		asim_wfh_telecommute_coefficients_file, job_code_translation),

	# Remote work model coefficients
	tar_file(asim_wfh_model_coeffs_file, "data/asim/work_from_home_coeffs.csv"),
	asim_wfh_model_coeffs = readr::read_csv(asim_wfh_model_coeffs_file),
	tar_file(asim_telecommute_model_coeffs_file, "data/asim/telecommute_frequency_coeffs.csv"),
	asim_telecommute_model_coeffs = readr::read_csv(asim_telecommute_model_coeffs_file),

	# Base year
	tar_file(asim_by_trips_file, "data/asim/output/base_2019_temp_mc/final_trips.csv.gz"),
	tar_file(asim_by_tours_file, "data/asim/output/base_2019_temp_mc/final_tours.csv.gz"),
	tar_file(asim_by_per_file, "data/asim/output/base_2019_temp_mc/final_persons.csv.gz"),
	tar_file(asim_by_hh_file, "data/asim/output/base_2019_temp_mc/final_households.csv.gz"),

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
	tar_file(asim_tr_trips_file, "data/asim/output/transit_temp_mc/final_trips.csv.gz"),
	tar_file(asim_tr_tours_file, "data/asim/output/transit_temp_mc/final_tours.csv.gz"),
	tar_file(asim_tr_per_file, "data/asim/output/transit_temp_mc/final_persons.csv.gz"),
	tar_file(asim_tr_hh_file, "data/asim/output/transit_temp_mc/final_households.csv.gz"),

	asim_tr_raw_trips = read_asim_trips_file(asim_tr_trips_file),
	asim_tr_trips = count_asim_trips(asim_tr_raw_trips),
	asim_tr_per = readr::read_csv(asim_tr_per_file),
	asim_tr_hh = readr::read_csv(asim_tr_hh_file),

	# WFH
	tar_file(asim_wfh_trips_file, "data/asim/output/wfh/final_trips.csv.gz"),
	tar_file(asim_wfh_tours_file, "data/asim/output/wfh/final_tours.csv.gz"),
	tar_file(asim_wfh_per_file, "data/asim/output/wfh/final_persons.csv.gz"),
	tar_file(asim_wfh_hh_file, "data/asim/output/wfh/final_households.csv.gz"),

	asim_wfh_raw_trips = read_asim_trips_file(asim_wfh_trips_file),
	asim_wfh_trips = count_asim_trips(asim_wfh_raw_trips),
)

# MCC targets ####
mc_calibration_targets <- tar_plan(
	# Temp for mc calibration
	tar_file(mcc_cube_by_hbw_omx, "data/cube/output/base_2019/HBW_trips_allsegs_pkok.omx"),
	mcc_cube_by_hbw = mcc_read_trip_matrix(mcc_cube_by_hbw_omx),
	# test_mcc_cube_by_hbw = test_mcc_read_trip_matrix(mcc_cube_by_hbw_omx),
	tar_file(mcc_cube_by_hbo_omx, "data/cube/output/base_2019/HBO_trips_allsegs_pkok.omx"),
	mcc_cube_by_hbo = mcc_read_trip_matrix(mcc_cube_by_hbo_omx),
	tar_file(mcc_cube_by_nhb_omx, "data/cube/output/base_2019/NHB_trips_allsegs_pkok.omx"),
	mcc_cube_by_nhb = mcc_read_trip_matrix(mcc_cube_by_nhb_omx),
	mcc_cube_by_trips = dplyr::bind_rows(
		list(
			hbw = mcc_cube_by_hbw,
			hbo = mcc_cube_by_hbo,
			nhb = mcc_cube_by_nhb),
		.id = "purpose"),
	mcc_cube_targets = dplyr::summarise(
		mcc_cube_by_trips,
		wfrc_trips = sum(trips),
		.by = c(purpose, mode)
	),
	mcc_cube_shares = dplyr::mutate(
		mcc_cube_targets,
		wfrc_share = wfrc_trips/sum(wfrc_trips),
		.by = c(purpose)
	),
	write_mcc_cube_shares = readr::write_csv(
		mcc_cube_shares,
		"data/calibration/mode_choice/mc_targets.csv"),

	tar_file(asim_mc_0_file, "data/asim/output/calibrate_mc_0/final_trips.csv"),
	asim_mc_0_raw_trips = read_asim_trips_file(asim_mc_0_file),
	asim_mc_0_trips = count_asim_trips(asim_mc_0_raw_trips),
	tar_file(asim_mc_1_file, "data/asim/output/calibrate_mc_1/final_trips.csv"),
	asim_mc_1_raw_trips = read_asim_trips_file(asim_mc_1_file),
	asim_mc_1_trips = count_asim_trips(asim_mc_1_raw_trips),
	tar_file(asim_mc_2_file, "data/asim/output/calibrate_mc_2/final_trips.csv"),
	asim_mc_2_raw_trips = read_asim_trips_file(asim_mc_2_file),
	asim_mc_2_trips = count_asim_trips(asim_mc_2_raw_trips),
	tar_file(asim_mc_3_file, "data/asim/output/calibrate_mc_3/final_trips.csv"),
	asim_mc_3_raw_trips = read_asim_trips_file(asim_mc_3_file),
	asim_mc_3_trips = count_asim_trips(asim_mc_3_raw_trips),
	tar_file(asim_mc_4_file, "data/asim/output/calibrate_mc_4/final_trips.csv"),
	asim_mc_4_raw_trips = read_asim_trips_file(asim_mc_4_file),
	asim_mc_4_trips = count_asim_trips(asim_mc_4_raw_trips),
	tar_file(asim_mc_5_file, "data/asim/output/calibrate_mc_5/final_trips.csv"),
	asim_mc_5_raw_trips = read_asim_trips_file(asim_mc_5_file),
	asim_mc_5_trips = count_asim_trips(asim_mc_5_raw_trips),
	tar_file(asim_mc_6_file, "data/asim/output/calibrate_mc_6/final_trips.csv"),
	asim_mc_6_raw_trips = read_asim_trips_file(asim_mc_6_file),
	asim_mc_6_trips = count_asim_trips(asim_mc_6_raw_trips),
	# tar_file(asim_mc_7_file, "data/asim/output/calibrate_mc_7/final_trips.csv"),
	# asim_mc_7_raw_trips = read_asim_trips_file(asim_mc_7_file),
	# asim_mc_7_trips = count_asim_trips(asim_mc_7_raw_trips),
	# tar_file(asim_mc_8_file, "data/asim/output/calibrate_mc_8/final_trips.csv"),
	# asim_mc_8_raw_trips = read_asim_trips_file(asim_mc_8_file),
	# asim_mc_8_trips = count_asim_trips(asim_mc_8_raw_trips),
	# tar_file(asim_mc_9_file, "data/asim/output/calibrate_mc_9/final_trips.csv"),
	# asim_mc_9_raw_trips = read_asim_trips_file(asim_mc_9_file),
	# asim_mc_9_trips = count_asim_trips(asim_mc_9_raw_trips),

	tar_file(asim_mc_0_tours_file, "data/asim/output/calibrate_mc_0/final_tours.csv"),
	asim_mc_0_raw_tours = read_asim_tours_file(asim_mc_0_tours_file),
	asim_mc_0_tours = count_asim_tours(asim_mc_0_raw_tours),
	tar_file(asim_mc_1_tours_file, "data/asim/output/calibrate_mc_1/final_tours.csv"),
	asim_mc_1_raw_tours = read_asim_tours_file(asim_mc_1_tours_file),
	asim_mc_1_tours = count_asim_tours(asim_mc_1_raw_tours),
	tar_file(asim_mc_2_tours_file, "data/asim/output/calibrate_mc_2/final_tours.csv"),
	asim_mc_2_raw_tours = read_asim_tours_file(asim_mc_2_tours_file),
	asim_mc_2_tours = count_asim_tours(asim_mc_2_raw_tours),
	tar_file(asim_mc_3_tours_file, "data/asim/output/calibrate_mc_3/final_tours.csv"),
	asim_mc_3_raw_tours = read_asim_tours_file(asim_mc_3_tours_file),
	asim_mc_3_tours = count_asim_tours(asim_mc_3_raw_tours),
	tar_file(asim_mc_4_tours_file, "data/asim/output/calibrate_mc_4/final_tours.csv"),
	asim_mc_4_raw_tours = read_asim_tours_file(asim_mc_4_tours_file),
	asim_mc_4_tours = count_asim_tours(asim_mc_4_raw_tours),
	tar_file(asim_mc_5_tours_file, "data/asim/output/calibrate_mc_5/final_tours.csv"),
	asim_mc_5_raw_tours = read_asim_tours_file(asim_mc_5_tours_file),
	asim_mc_5_tours = count_asim_tours(asim_mc_5_raw_tours),
	tar_file(asim_mc_6_tours_file, "data/asim/output/calibrate_mc_6/final_tours.csv"),
	asim_mc_6_raw_tours = read_asim_tours_file(asim_mc_6_tours_file),
	asim_mc_6_tours = count_asim_tours(asim_mc_6_raw_tours),
	# tar_file(asim_mc_7_tours_file, "data/asim/output/calibrate_mc_7/final_tours.csv"),
	# asim_mc_7_raw_tours = read_asim_tours_file(asim_mc_7_tours_file),
	# asim_mc_7_tours = count_asim_tours(asim_mc_7_raw_tours),
	# tar_file(asim_mc_8_tours_file, "data/asim/output/calibrate_mc_8/final_tours.csv"),
	# asim_mc_8_raw_tours = read_asim_tours_file(asim_mc_8_tours_file),
	# asim_mc_8_tours = count_asim_tours(asim_mc_8_raw_tours),
	# tar_file(asim_mc_9_tours_file, "data/asim/output/calibrate_mc_9/final_tours.csv"),
	# asim_mc_9_raw_tours = read_asim_tours_file(asim_mc_9_tours_file),
	# asim_mc_9_tours = count_asim_tours(asim_mc_9_raw_tours),


	tar_file(asim_tr_0_file, "data/asim/output/transit_0/final_trips.csv"),
	asim_tr_0_raw_trips = read_asim_trips_file(asim_tr_0_file),
	asim_tr_0_trips = count_asim_trips(asim_tr_0_raw_trips),
	tar_file(asim_tr_1_file, "data/asim/output/transit_1/final_trips.csv"),
	asim_tr_1_raw_trips = read_asim_trips_file(asim_tr_1_file),
	asim_tr_1_trips = count_asim_trips(asim_tr_1_raw_trips),
	tar_file(asim_tr_2_file, "data/asim/output/transit_2/final_trips.csv"),
	asim_tr_2_raw_trips = read_asim_trips_file(asim_tr_2_file),
	asim_tr_2_trips = count_asim_trips(asim_tr_2_raw_trips),
	tar_file(asim_tr_3_file, "data/asim/output/transit_3/final_trips.csv"),
	asim_tr_3_raw_trips = read_asim_trips_file(asim_tr_3_file),
	asim_tr_3_trips = count_asim_trips(asim_tr_3_raw_trips),
	tar_file(asim_tr_4_file, "data/asim/output/transit_4/final_trips.csv"),
	asim_tr_4_raw_trips = read_asim_trips_file(asim_tr_4_file),
	asim_tr_4_trips = count_asim_trips(asim_tr_4_raw_trips),
	tar_file(asim_tr_5_file, "data/asim/output/transit_5/final_trips.csv"),
	asim_tr_5_raw_trips = read_asim_trips_file(asim_tr_5_file),
	asim_tr_5_trips = count_asim_trips(asim_tr_5_raw_trips),

	combined_by_trips_0 = dplyr::bind_rows(list(asim = asim_mc_0_trips, cube = cube_by_trips), .id = "model"),
	combined_by_trips_1 = dplyr::bind_rows(list(asim = asim_mc_1_trips, cube = cube_by_trips), .id = "model"),
	combined_by_trips_2 = dplyr::bind_rows(list(asim = asim_mc_2_trips, cube = cube_by_trips), .id = "model"),
	combined_by_trips_3 = dplyr::bind_rows(list(asim = asim_mc_3_trips, cube = cube_by_trips), .id = "model"),
	combined_by_trips_4 = dplyr::bind_rows(list(asim = asim_mc_4_trips, cube = cube_by_trips), .id = "model"),
	combined_by_trips_5 = dplyr::bind_rows(list(asim = asim_mc_5_trips, cube = cube_by_trips), .id = "model"),
	combined_by_trips_6 = dplyr::bind_rows(list(asim = asim_mc_6_trips, cube = cube_by_trips), .id = "model"),
	# combined_by_trips_7 = dplyr::bind_rows(list(asim = asim_mc_7_trips, cube = cube_by_trips), .id = "model"),
	# combined_by_trips_8 = dplyr::bind_rows(list(asim = asim_mc_8_trips, cube = cube_by_trips), .id = "model"),
	# combined_by_trips_9 = dplyr::bind_rows(list(asim = asim_mc_9_trips, cube = cube_by_trips), .id = "model"),

	by_mode_split_comparison_0 = compare_by_mode_split(combined_by_trips_0),
	by_mode_split_comparison_1 = compare_by_mode_split(combined_by_trips_1),
	by_mode_split_comparison_2 = compare_by_mode_split(combined_by_trips_2),
	by_mode_split_comparison_3 = compare_by_mode_split(combined_by_trips_3),
	by_mode_split_comparison_4 = compare_by_mode_split(combined_by_trips_4),
	by_mode_split_comparison_5 = compare_by_mode_split(combined_by_trips_5),
	by_mode_split_comparison_6 = compare_by_mode_split(combined_by_trips_6),
	# by_mode_split_comparison_7 = compare_by_mode_split(combined_by_trips_7),
	# by_mode_split_comparison_8 = compare_by_mode_split(combined_by_trips_8),
	# by_mode_split_comparison_9 = compare_by_mode_split(combined_by_trips_9),

	asim_tr_all_trips_diff_0 = get_trip_diff(list(tr = asim_tr_0_trips, by = asim_mc_0_trips)),
	asim_tr_all_trips_diff_1 = get_trip_diff(list(tr = asim_tr_1_trips, by = asim_mc_1_trips)),
	asim_tr_all_trips_diff_2 = get_trip_diff(list(tr = asim_tr_2_trips, by = asim_mc_2_trips)),
	asim_tr_all_trips_diff_3 = get_trip_diff(list(tr = asim_tr_3_trips, by = asim_mc_3_trips)),
	asim_tr_all_trips_diff_4 = get_trip_diff(list(tr = asim_tr_4_trips, by = asim_mc_4_trips)),
	asim_tr_all_trips_diff_5 = get_trip_diff(list(tr = asim_tr_5_trips, by = asim_mc_5_trips)),

	asim_tr_diff_by_district_plot_0 = plot_trips_diff_by_district(
		asim_tr_all_trips_diff_0, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),
	asim_tr_diff_by_district_plot_1 = plot_trips_diff_by_district(
		asim_tr_all_trips_diff_1, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),
	asim_tr_diff_by_district_plot_2 = plot_trips_diff_by_district(
		asim_tr_all_trips_diff_2, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),
	asim_tr_diff_by_district_plot_3 = plot_trips_diff_by_district(
		asim_tr_all_trips_diff_3, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),
	asim_tr_diff_by_district_plot_4 = plot_trips_diff_by_district(
		asim_tr_all_trips_diff_4, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),
	asim_tr_diff_by_district_plot_5 = plot_trips_diff_by_district(
		asim_tr_all_trips_diff_5, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),

	tar_file(asim_tour_mc_coeffs_file, "data/calibration/mode_choice/tour_mode_choice_coefficients.csv"),
	mode_choice_calibration_asim_coeffs_summary = dplyr::summarise(
		read_csv(asim_tour_mc_coeffs_file),
		dplyr::across(-c(coefficient_name), mean),
		.by = tour_mode
	),

	mode_choice_calibration_shares = make_mc_shares_df(list(
		by_mode_split_comparison_0,
		by_mode_split_comparison_1,
		by_mode_split_comparison_2,
		by_mode_split_comparison_3,
		by_mode_split_comparison_4,
		by_mode_split_comparison_5,
		by_mode_split_comparison_6
		# by_mode_split_comparison_7,
		# by_mode_split_comparison_8,
		# by_mode_split_comparison_9
	)),
	mode_choice_calibration_shares_plot = plot_mc_shares(
		mode_choice_calibration_shares
	),

	quick_make_mc_comparison = list(
		mode_choice_calibration_shares_plot,
		asim_tr_diff_by_district_plot_0,
		asim_tr_diff_by_district_plot_1,
		asim_tr_diff_by_district_plot_2,
		asim_tr_diff_by_district_plot_3,
		asim_tr_diff_by_district_plot_4,
		asim_tr_diff_by_district_plot_5,
		# asim_tr_diff_by_district_plot_6,
		# asim_tr_diff_by_district_plot_7,
		# asim_tr_diff_by_district_plot_8,
		# asim_tr_diff_by_district_plot_9,
		mode_choice_calibration_asim_coeffs_summary
	),
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
		combined_by_se_data, taz_distsml_transl, distsml),

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
	by_mode_split_comparison = compare_by_mode_split(combined_by_trips),
	asim_mode_choice_calibration_plot = plot_asim_mode_choice_calibration(
		asim_mode_choice_calibration_iters),

	# TLFD
	combined_by_tlfd_plot = plot_combined_tlfd(
		combined_by_trips_sampled, distances),

	# WFH
	comparison_by_telecommute_coeffs = compare_telecommute(
		cube_by_telecommute_percentages,
		asim_by_telecommute_coefficients),
)

# Land use ####
land_use_targets <- tar_plan(
	# Data
	lu_tazs = c(2138, 2140, 2141, 2149, 2170),
	lu_distsml = get_dist_from_tazs(lu_tazs, taz_distsml_transl),
	lu_distmed = get_dist_from_tazs(lu_tazs, taz_distmed_transl),
	lu_plot_new_tazs = plot_lu_new_tazs(taz, lu_tazs, cube_lu_se_diff),
	lu_se_by_table = make_lu_se_table(cube_by_taz_se, lu_tazs),
	lu_se_lu_table = make_lu_se_table(cube_lu_taz_se, lu_tazs),
	lu_combined_se_table = combine_se_tables(list(by = lu_se_by_table, lu = lu_se_lu_table)),

	# cube_lu_by_se_diff = get_cube_se_diff(list(lu = cube_lu_taz_se, by = cube_by_taz_se)),
	# lu_se_diff_table = make_lu_se_table(cube_lu_by_se_diff, lu_tazs),

	cube_lu_all_diff = get_trip_diff(list(lu = cube_lu_trips,	by = cube_by_trips)),
	cube_lu_all_diff_distsml = get_trip_diff(list(
		lu = sum_trips_by_district(cube_lu_trips, taz_distsml_transl),
		by = sum_trips_by_district(cube_by_trips, taz_distsml_transl))),
	cube_lu_nhb_diff_distsml = dplyr::filter(
		cube_lu_all_diff_distsml, purpose == "nhb"),
	cube_lu_new_productions_distsml = dplyr::filter(
		cube_lu_all_diff_distsml, origin %in% lu_distsml, purpose != "nhb"),

	asim_lu_new_persons = get_asim_new_persons(asim_lu_per, asim_by_per, lu_tazs),
	asim_lu_new_trips = get_asim_lu_new_trips(asim_lu_raw_trips, asim_lu_new_persons),
	asim_lu_new_trips_distsml = sum_trips_by_district(
		asim_lu_new_trips, taz_distsml_transl),

	# PMT/VMT
	cube_lu_new_pmt_plot = plot_cube_lu_new_pmt(
		cube_lu_all_diff, distances, lu_tazs),
	asim_lu_new_pmt_plot = plot_asim_lu_pmt(
		asim_lu_raw_trips, asim_lu_new_persons, distances, lu_tazs),

	# Desire lines
	cube_lu_new_productions_desire = od::od_to_sf(
		cube_lu_new_productions_distsml, distsml_centroids),
	cube_lu_new_productions_desire_map = plot_cube_lu_desire_lines(
		cube_lu_new_productions_desire, distsml),
	cube_lu_nhb_diff_desire = od::od_to_sf(
		cube_lu_nhb_diff_distsml, distsml_centroids),
	cube_lu_nhb_diff_desire_map = plot_cube_lu_nhb_desire_lines(
		cube_lu_nhb_diff_desire, distsml),

	asim_lu_new_desire_lines = od::od_to_sf(
		asim_lu_new_trips_distsml, distsml_centroids),
	asim_lu_new_desire_map = plot_asim_lu_desire_lines(
		asim_lu_new_desire_lines, lu_distsml, distsml),
)

# Transit ####
transit_targets <- tar_plan(
	# Data
	cube_tr_all_trips_diff = get_trip_diff(list(
		tr = cube_tr_trips, by = cube_by_trips)),
	asim_tr_all_trips_diff = get_trip_diff(list(
		tr = asim_tr_trips, by = asim_by_trips)),

	asim_tr_raw_trip_diff = get_asim_raw_trip_diff(list(
		tr = asim_tr_raw_trips, by = asim_by_raw_trips)),

	asim_tr_mode_switching = get_asim_mode_switching(asim_tr_raw_trip_diff),
	asim_tr_atwork_mode_switching = get_asim_atwork_mode_switching(
		asim_tr_raw_trip_diff, asim_tr_mode_switching),
	asim_tr_work_transit_switching = get_asim_work_transit_switching(
		asim_tr_raw_trip_diff, asim_tr_mode_switching),

	# Mode split
	combined_tr_mode_split_diff = dplyr::full_join(
		calculate_mode_split_diff_pct(cube_tr_all_trips_diff, "cube"),
		calculate_mode_split_diff_pct(asim_tr_all_trips_diff, "asim"),
		join_by(purpose, mode)),

	# Mode switching
	asim_tr_mode_switching_plot = plot_asim_mode_switching(
		dplyr::filter(asim_tr_mode_switching, purpose != "at-work")),
	asim_tr_atwork_mode_switching_plot = plot_asim_mode_switching(asim_tr_atwork_mode_switching),
	asim_tr_work_transit_switching_plot = plot_asim_mode_switching(asim_tr_work_transit_switching),

	# SE comparison
	cube_tr_productions_se = get_cube_production_se(cube_tr_trips, cube_by_taz_se),
	cube_tr_productions_se_summary = summarise_cube_transit_se(
		cube_tr_productions_se),
	asim_tr_trips_se = get_asim_trips_se(
		asim_tr_raw_trips, asim_tr_per, asim_tr_hh),
	asim_tr_trips_se_summary = summarise_asim_transit_se(
		asim_tr_trips_se),
	combined_tr_trips_se_for_income_plot = combine_tr_trips_se_for_income_plot(
		cube = cube_tr_productions_se, asim = asim_tr_trips_se),
	combined_tr_new_transit_income_plot = plot_tr_new_transit_income_dist(
		combined_tr_trips_se_for_income_plot),

	# New transit trip productions
	cube_tr_diff_by_district_plot = plot_trips_diff_by_district(
		cube_tr_all_trips_diff, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),
	asim_tr_diff_by_district_plot = plot_trips_diff_by_district(
		asim_tr_all_trips_diff, taz_distsml_transl, distsml,
		frontrunner_line, frontrunner_stops),

)

# WFH ####
wfh_targets <- tar_plan(
	# Data
	cube_wfh_all_trips_diff = get_trip_diff(list(
		wfh = cube_wfh_trips, by = cube_by_trips)),
	asim_wfh_all_trips_diff = get_trip_diff(list(
		wfh = asim_wfh_trips, by = asim_by_trips)),
	combined_wfh_trips_diff = dplyr::bind_rows(list(
		cube = cube_wfh_all_trips_diff, asim = asim_wfh_all_trips_diff),
		.id = "model"),

	cube_wfh_by_remote_work_pct_comparison = tidyr::pivot_wider(
		cube_remote_work_percentages,
		names_from = c(year, type), values_from = pct),

	comparison_wfh_telecommute_coeffs = compare_telecommute(
		cube_wfh_telecommute_percentages,
		asim_wfh_telecommute_coefficients),

	# Mode split
	combined_wfh_mode_split_diff = dplyr::full_join(
		calculate_mode_split_diff_pct(cube_wfh_all_trips_diff, "cube"),
		calculate_mode_split_diff_pct(asim_wfh_all_trips_diff, "asim"),
		join_by(purpose, mode)),

	# Trip and PMT diff
	cube_wfh_trip_pmt_diff = calculate_trip_and_pmt_diff(
		cube_wfh_all_trips_diff, distances),
	asim_wfh_trip_pmt_diff = calculate_trip_and_pmt_diff(
		asim_wfh_all_trips_diff, distances),

	# TLFD
	combined_wfh_tlfd_diff_plot = plot_wfh_tlfd_diff(
		combined_wfh_trips_diff, distances),
	cube_wfh_tlfd_diff_comp_plot = plot_wfh_by_tlfd_diff_comp(
		combined_wfh_trips_diff, which_model = "cube", distances),
	asim_wfh_tlfd_diff_comp_plot = plot_wfh_by_tlfd_diff_comp(
		combined_wfh_trips_diff, which_model = "asim", distances),

)

#### Run all targets ###################################################

tar_plan(
	frontrunner_targets,
	shared_data_targets,
	cube_data_targets,
	asim_data_targets,
	# mc_calibration_targets,

	base_year_targets,
	land_use_targets,
	transit_targets,
	wfh_targets,
)

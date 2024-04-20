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

	tar_file(cube_time_spent_table_file, "data/cube_time_spent.csv"),
	cube_time_spent_table = readr::read_csv(cube_time_spent_table_file),
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
	# tar_files(
	# 	asim_mode_choice_calibration_iters_files,
	# 	list.files(
	# 		"data/calibration/mode_choice",
	# 		full.names = TRUE, pattern = ".*\\.csv")),
	# tar_target(
	# 	asim_mode_choice_calibration_iters,
	# 	combine_asim_mode_choice_calibration_iters(
	# 		asim_mode_choice_calibration_iters_files),
	# 	pattern = map(asim_mode_choice_calibration_iters_files)),

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
	tar_file(asim_tc_dap_coeffs_file, "data/asim/remote_work_DAP_coeffs.csv"),
	asim_tc_dap_coeffs = readr::read_csv(asim_tc_dap_coeffs_file),

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
	tar_file(asim_lu_trips_file, "data/asim/output/landuse/final_trips.csv.gz"),
	tar_file(asim_lu_tours_file, "data/asim/output/landuse/final_tours.csv.gz"),
	tar_file(asim_lu_per_file, "data/asim/output/landuse/final_persons.csv.gz"),
	tar_file(asim_lu_hh_file, "data/asim/output/landuse/final_households.csv.gz"),

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

	tar_files(mcc_by_trips_files, list_mcc_trips_iters("calibrate_mc")),
	tar_target(
		mcc_by_trips_raw,
		read_asim_trips_file(mcc_by_trips_files),
		pattern = map(mcc_by_trips_files),
		iteration = "list"
	),
	tar_target(
		mcc_by_trips,
		count_asim_trips(mcc_by_trips_raw),
		pattern = map(mcc_by_trips_raw),
		# pattern = head(map(mcc_by_trips_raw)),
		iteration = "list"
	),

	tar_files(mcc_tr_trips_files, list_mcc_trips_iters("transit")),
	tar_target(
		mcc_tr_trips_raw,
		read_asim_trips_file(mcc_tr_trips_files),
		pattern = map(mcc_tr_trips_files),
		iteration = "list"
	),
	tar_target(
		mcc_tr_trips,
		count_asim_trips(mcc_tr_trips_raw),
		pattern = map(mcc_tr_trips_raw),
		# pattern = head(map(mcc_tr_trips_raw)),
		iteration = "list"
	),

	tar_target(
		mcc_tr_trip_diff,
		get_trip_diff(list(tr = mcc_tr_trips, by = mcc_by_trips)),
		pattern = map(mcc_tr_trips, mcc_by_trips),
		iteration = "list"
	),

	tar_target(
		mcc_tr_diff_plot,
		plot_trips_diff_by_district(
			mcc_tr_trip_diff, taz_distsml_transl, distsml,
			frontrunner_line, frontrunner_stops),
		pattern = map(mcc_tr_trip_diff),
		iteration = "list"
	),

	tar_file(mode_crosswalk_file, "data/calibration/mode_crosswalk.csv"),
	mode_crosswalk = readr::read_csv(mode_crosswalk_file),
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
	tar_files(
		mcc_adjustments_files,
		list.files(
			"data/calibration/mode_choice/coeffs",
			full.names = TRUE, pattern = "\\d+.*adjustments\\.csv")),
	tar_target(
		mcc_adjustments,
		read_mcc_adjustments_files(mcc_adjustments_files),
		pattern = map(mcc_adjustments_files)
	),
	mcc_adjustments_plot = plot_mcc_adjustments(mcc_adjustments),
	## Mode choice coefficients
	tar_files(
		mcc_tour_coeffs_files,
		list.files(
			"data/calibration/mode_choice/coeffs",
			full.names = TRUE, pattern = "\\d+_tour_mode_choice_coefficients.csv")
	),
	tar_target(
		mcc_tour_coeffs,
		read_mcc_coeffs(mcc_tour_coeffs_files),
		pattern = map(mcc_tour_coeffs_files)
	),
	tar_target(
		mcc_tour_coeffs_combined,
		tidyr::pivot_wider(mcc_tour_coeffs, names_from = iter, values_from = value)
	),

	tar_files(
		mcc_trip_coeffs_files,
		list.files(
			"data/calibration/mode_choice/coeffs",
			full.names = TRUE, pattern = "\\d+_trip_mode_choice_coefficients.csv")
	),
	tar_target(
		mcc_trip_coeffs,
		read_mcc_coeffs(mcc_trip_coeffs_files),
		pattern = map(mcc_trip_coeffs_files)
	),
	tar_target(
		mcc_trip_coeffs_combined,
		tidyr::pivot_wider(mcc_trip_coeffs, names_from = iter, values_from = value)
	),

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
	draper_prison_site_map = make_draper_prison_site_map(taz, lu_tazs),
	gateway_and_prison_map = plot_gateway_and_prison(),
	# draper_prison_site_inset = get_draper_prison_site_inset(taz, lu_tazs),

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
	cube_tr_by_hbw = read_tr_trip_matrix(cube_by_hbw_omx),
	cube_tr_by_hbo = read_tr_trip_matrix(cube_by_hbo_omx),
	cube_tr_by_nhb = read_tr_trip_matrix(cube_by_nhb_omx),
	cube_tr_by_trips = dplyr::bind_rows(
		list(
			hbw = cube_tr_by_hbw,
			hbo = cube_tr_by_hbo,
			nhb = cube_tr_by_nhb),
		.id = "purpose"),
	cube_tr_tr_hbw = read_tr_trip_matrix(cube_tr_hbw_omx),
	cube_tr_tr_hbo = read_tr_trip_matrix(cube_tr_hbo_omx),
	cube_tr_tr_nhb = read_tr_trip_matrix(cube_tr_nhb_omx),
	cube_tr_tr_trips = dplyr::bind_rows(
		list(
			hbw = cube_tr_tr_hbw,
			hbo = cube_tr_tr_hbo,
			nhb = cube_tr_tr_nhb),
		.id = "purpose"),

	asim_by_tr_trips = count_asim_tr_trips(asim_by_raw_trips),
	asim_tr_tr_trips = count_asim_tr_trips(asim_tr_raw_trips),

	cube_tr_all_trips_diff = get_trip_diff(
		list(tr = cube_tr_tr_trips, by = cube_tr_by_trips)),
	asim_tr_all_trips_diff = get_trip_diff(
		list(tr = asim_tr_tr_trips, by = asim_by_tr_trips)),

	# Mode split
	combined_tr_mode_split_diff = dplyr::full_join(
		calculate_mode_split_diff_pct(cube_tr_all_trips_diff, "cube"),
		calculate_mode_split_diff_pct(asim_tr_all_trips_diff, "asim"),
		join_by(purpose, mode)),
	combined_tr_mode_split_table = make_combined_mode_split_table(
		combined_tr_mode_split_diff),

	# Mode switching
	asim_tr_mode_switching_raw = get_asim_mode_switching(list(
		tr = asim_tr_raw_trips, by = asim_by_raw_trips)),
	asim_tr_mode_switching = dplyr::filter(asim_tr_mode_switching_raw, mode_tr != mode_by),
	asim_tr_mode_switching_plot = plot_asim_mode_switching(asim_tr_mode_switching),

	asim_tr_work_switchers = get_asim_work_switchers(asim_tr_mode_switching),
	asim_tr_atwork_switching = get_asim_atwork_mode_switching(
		list(tr = asim_tr_raw_trips, by = asim_by_raw_trips),
		asim_tr_work_switchers),
	asim_tr_atwork_switching_plot = plot_asim_mode_switching(asim_tr_atwork_switching),

	# SE comparison
	cube_tr_productions_se = get_cube_production_se(cube_tr_tr_trips, cube_by_taz_se),
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

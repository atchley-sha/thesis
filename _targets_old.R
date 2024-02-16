# ABM/TBM flowchart diagram ####
abm_tbm_flowchart <- tar_plan(
	# Data
	tar_file(
		ex_nodes_file,
		"data/example_flowchart_comparison/nodes.csv"
	),
	tar_file(
		ex_trip_file,
		"data/example_flowchart_comparison/trip-based.csv"
	),
	tar_file(
		ex_tour_file,
		"data/example_flowchart_comparison/tour-based.csv"
	),
	tar_file(
		ex_tbm_nodes_file,
		"data/example_flowchart_comparison/tbm_zones/tbm_nodes.csv"
	),
	tar_file(
		ex_tbm_edges_file,
		"data/example_flowchart_comparison/tbm_zones/tbm_edges.csv"
	),
	ex_tbm_edges = pivot_tbm_edges(ex_tbm_edges_file),
	tar_file(
		ex_aggregate_file,
		"data/example_flowchart_comparison/information_pipelines/aggregate.dot"
	),
	tar_file(
		ex_synthetic_file,
		"data/example_flowchart_comparison/information_pipelines/synthetic.dot"
	),

	# Viz
	trip_ex = make_ex_dap_viz(
		ex_nodes_file,
		ex_trip_file,
		dot_file = "output/example_flowchart_comparison/trip.dot",
		image_file = "output/example_flowchart_comparison/trip.png"
	),
	tour_ex = make_ex_dap_viz(
		ex_nodes_file,
		ex_tour_file,
		dot_file = "output/example_flowchart_comparison/tour.dot",
		image_file = "output/example_flowchart_comparison/tour.png"
	),
	tbm_ex = make_ex_tbm_viz(
		ex_tbm_nodes_file,
		ex_tbm_edges,
		dot_file = "output/example_flowchart_comparison/tbm.dot",
		image_file = "output/example_flowchart_comparison/tbm.png"
	),
	ex_aggregate = render_dot_graph(dot_file = ex_aggregate_file, image_file = "output/example_flowchart_comparison/aggregate.png"),
	ex_synthetic = render_dot_graph(dot_file = ex_synthetic_file, image_file = "output/example_flowchart_comparison/synthetic.png"),
)



# Base outputs comparison (TLFD/mode choice/WFH) ####


# Base output ####
base_outputs <- tar_plan(
		simple_by_se_data = get_se_data_for_point_zones(by_se_data)
)

# Land use ####
land_use_outputs <- tar_plan(
	by_lu_vmt = get_vmt(by_lu_trp),

	lu_new_vmt = get_lu_vmt(lu_new_trips, lu_tazs),
	lu_vmt_plot = make_lu_vmt_plot(lu_new_vmt),


	# WFRC
	tar_file(
		trip_gen_lu_wfrc_file,
		"data/cube_output/land_use/TripGenprison.csv"
	),


	lu_trip_gen_wfrc = readr::read_csv(trip_gen_lu_wfrc_file),
	lu_se_data = readr::read_csv(land_use_se_file),


	lu_show_location = plot_land_use_location(taz),
	simple_lu_se_data = get_se_data_for_point_zones(lu_se_data),


)

# Transit ####
transit_outputs <- tar_plan(# ASIM


	transit_comparison_map = compare_transit_trips(by_trp, tr_trp))

# WFH ####
wfh_outputs <- tar_plan(
	# ASIM


	wfrc_wfh_trip_diff = get_trip_diff(list(by = all_wfrc_by_trips,
																					wfh = all_wfrc_wfh_trips)),

	asim_wfh_trip_diff = get_trip_diff(list(by = all_asim_by_trips,
																					wfh = all_asim_wfh_trips)),

	wfrc_wfh_diff_summary = summ_trip_diff(wfrc_wfh_trip_diff),
	asim_wfh_diff_summary = summ_trip_diff(asim_wfh_trip_diff),

	wfh_trip_count = count_trips(wfh_trp, taz_dist_trans),
	wfh_trip_diff = get_trip_difference(wfh_trip_count, by_trip_count),

	wfh_trip_diff_dist = add_taz_distances(wfh_trip_diff, distances),

	wfh_trip_dist_summary = summarise_trip_diff(wfh_trip_diff_dist),


	wfh_abm_purpose_histogram = plot_wfh_trip_diff_by_purpose(wfh_trip_diff),

	all_asim_trips_for_wfh = make_all_asim_tlfd_trips(by_trip_count, wfh_trip_diff_dist, distances),
	hbw_asim_trips_for_wfh = dplyr::filter(all_asim_trips_for_wfh, purpose == "hbw"),
	wfh_by_tlfd_plot = plot_wfh_vs_by_tlfd(hbw_asim_trips_for_wfh),

	wfrc_trips_for_wfh = make_all_wfrc_tlfd_trips(wfrc_wfh_hbw, wfrc_by_hbw, distances),
	wfrc_wfh_tlfd_plot = plot_wfh_vs_by_tlfd(wfrc_trips_for_wfh),

	wfh_by_mode_plot = plot_wfh_vs_by_mode(hbw_asim_trips_for_wfh),

	wfh_vmt = get_vmt_dist(wfh_trp, distances),
	wfh_o_vmt = get_o_vmt(wfh_vmt, taz_dist_trans),
	comp_wfh_o_vmt = make_comp_o_vmt(wfh_o_vmt, by_o_vmt, districts),
)

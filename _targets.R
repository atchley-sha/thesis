#### Setup #####################################################################

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("ggalluvial", "tidyverse", "DiagrammeR", "sf", "ggspatial", "omxr", "qs", "wesanderson", "ggspatial", "scales", "od"),
  memory = "transient",
  garbage_collection = TRUE,
  format = "qs",
  )

r_files <- list.files("R", full.names = TRUE)
sapply(r_files, source)


#### List targets ##############################################################

# Misc ####
misc_targets <- tar_plan(
  plot_lims = list(x = c(-112.15,-111.6), y = c(40.2,40.8)),
)

# ABM/TBM flowchart diagram ####
abm_tbm_flowchart <- tar_plan(
  # Data
  tar_file(ex_nodes_file, "data/example_flowchart_comparison/nodes.csv"),
  tar_file(ex_trip_file, "data/example_flowchart_comparison/trip-based.csv"),
  tar_file(ex_tour_file, "data/example_flowchart_comparison/tour-based.csv"),
  tar_file(ex_tbm_nodes_file, "data/example_flowchart_comparison/tbm_zones/tbm_nodes.csv"),
  tar_file(ex_tbm_edges_file, "data/example_flowchart_comparison/tbm_zones/tbm_edges.csv"),
  ex_tbm_edges = pivot_tbm_edges(ex_tbm_edges_file),
  tar_file(ex_aggregate_file, "data/example_flowchart_comparison/information_pipelines/aggregate.dot"),
  tar_file(ex_synthetic_file, "data/example_flowchart_comparison/information_pipelines/synthetic.dot"),
  
  # Viz
  trip_ex = make_ex_dap_viz(ex_nodes_file, ex_trip_file, dot_file = "output/example_flowchart_comparison/trip.dot", image_file = "output/example_flowchart_comparison/trip.png"),
  tour_ex = make_ex_dap_viz(ex_nodes_file, ex_tour_file, dot_file = "output/example_flowchart_comparison/tour.dot", image_file = "output/example_flowchart_comparison/tour.png"),
  tbm_ex = make_ex_tbm_viz(ex_tbm_nodes_file, ex_tbm_edges, dot_file = "output/example_flowchart_comparison/tbm.dot", image_file = "output/example_flowchart_comparison/tbm.png"),
  ex_aggregate = render_dot_graph(dot_file = ex_aggregate_file, image_file = "output/example_flowchart_comparison/aggregate.png"),
  ex_synthetic = render_dot_graph(dot_file = ex_synthetic_file, image_file = "output/example_flowchart_comparison/synthetic.png"),
)

# Synthetic population comparison ####
synth_pop_comparison <- tar_plan(
  # Data
  tar_file(synth_per_file, "data/base_model_comparison/asim/synthetic_persons.csv.gz"),
  tar_file(synth_hh_file, "data/base_model_comparison/asim/synthetic_households.csv.gz"),
  tar_file(zonal_se_file, "data/base_model_comparison/wfrc/TAZ_SE_2019_WFRC.csv"),
  tar_file(zonal_income_groups_file, "data/base_model_comparison/wfrc/Marginal_Income.csv"),
  tar_file(taz_file, "data/WFRC_TAZ.geojson"),
  tar_file(income_groups_file, "data/income_groups.csv"),
  
  taz = get_taz(taz_file),
  taz_centroids = get_zone_centroids(taz),
  districts = get_districts(taz),
  dist_centroids = get_zone_centroids(districts),
  taz_dist_trans = sf::st_drop_geometry(taz),

  # Analysis
  asim_pop = read_asim_population(synth_per_file, synth_hh_file),
  se_data = read_zonal_data(zonal_se_file, zonal_income_groups_file),
  pop_comp = make_zonal_comparison(asim_pop, se_data, taz),  
  income_groups = read_income_groups(income_groups_file),
  
  # Viz
  inc_groups_map = make_inc_groups_map(pop_comp, income_groups),
  avg_inc_map = make_avg_inc_map(pop_comp),
  med_inc_map = make_med_inc_map(pop_comp),
  inc_comp_plot = make_inc_plot(pop_comp),
  pop_comp_map = make_pop_comp_map(pop_comp),

)

# Base outputs comparison (TLFD/mode choice/WFH) ####
base_outputs_comparison <- tar_plan(
  # Trips
  tar_file(distance_skims, "data/base_model_comparison/wfrc/skm_DY_Dist.omx"),
  #some zones have a very high distance and are external; we don't want them
  external_zones = get_ex_zones(distance_skims),
  distances = read_distances(distance_skims, external_zones),
  
  tar_file(wfrc_hbw_trips_od, "data/base_model_comparison/wfrc/trips/HBW_trips_allsegs_pkok.omx"),
  tar_file(wfrc_hbo_trips_od, "data/base_model_comparison/wfrc/trips/HBO_trips_allsegs_pkok.omx"),
  tar_file(wfrc_nhb_trips_od, "data/base_model_comparison/wfrc/trips/NHB_trips_allsegs_pkok.omx"),
  wfrc_hbw_trips = omxr::read_all_omx(wfrc_hbw_trips_od, c("auto", "transit", "nonmotor")),
  wfrc_hbo_trips = omxr::read_all_omx(wfrc_hbo_trips_od, c("auto", "transit", "nonmotor")),
  wfrc_nhb_trips = omxr::read_all_omx(wfrc_nhb_trips_od, c("auto", "transit", "nonmotor")),
  wfrc_trips_od = list(hbw = wfrc_hbw_trips, hbo = wfrc_hbo_trips, nhb = wfrc_nhb_trips),
  
  tar_file(asim_trips_file, "data/base_model_comparison/asim/final_trips.csv.gz"),
  tar_file(asim_tours_file, "data/base_model_comparison/asim/final_tours.csv.gz"),
  
  wfrc_trips = combine_wfrc_od(wfrc_trips_od, external_zones),
  asim_trips = get_asim_od(asim_trips_file, asim_tours_file, external_zones),
  combined_trips = combine_all_od(wfrc_trips, asim_trips, distances),
  sampled_trips = sample_trips(combined_trips, prop = 0.1, weight = FALSE),
  tlfd_comp_plot = make_tlfd_comp_plot(sampled_trips),
  
  # Mode choice
  tar_files(
    calibration_iters_files,
    list.files("data/calibration", full.names = TRUE, pattern = ".*\\.csv")),
  tar_target(
    calibration_iters,
    combine_calibration_iters(calibration_iters_files),
    pattern = map(calibration_iters_files)),
  calibration_plot = plot_calibration(calibration_iters),
  comp_modes = compare_mode_split(combined_trips),
  mode_split_comp = make_mode_split_comp(comp_modes),
  
  # WFH
  tar_file(wfrc_telecommute_base_file, "data/base_model_comparison/wfrc/telecommute_base.csv"),
  wfrc_telecommute_base = get_wfrc_telecommute(wfrc_telecommute_base_file),
  wfrc_telecommute_pct = wfrc_telecommute_base$pct,
  FAKE_wfrc_telecommute_table = wfrc_telecommute_base$table,
  tar_file(wfrc_telecommute_table_file, "data/base_model_comparison/wfrc/telecommute_jobtype.csv"),
  wfrc_telecommute_table = readr::read_csv(wfrc_telecommute_table_file),
  
  wfrc_hbj_base = make_wfrc_hbj(se_data),
  wfrc_hbj_base_pct = wfrc_hbj_base$pct,
  wfrc_hbj_base_plot = wfrc_hbj_base$plot,
)

## Analysis #####################

# Base output ####
base_outputs <- tar_plan(
  # ASIM
  tar_file(by_trips, "data/asim_output/base_2019/final_trips.csv.gz"),
  tar_file(by_tours, "data/asim_output/base_2019/final_tours.csv.gz"),
  tar_file(by_persons, "data/asim_output/base_2019/final_persons.csv.gz"),
  tar_file(by_households, "data/asim_output/base_2019/final_households.csv.gz"),
  
  by_trp = readr::read_csv(by_trips),
  by_tor = readr::read_csv(by_tours),
  by_per = readr::read_csv(by_persons),
  by_hh = readr::read_csv(by_households),
  
  by_od = make_district_od(by_trp, taz_dist_trans),
  by_desire = better_desire_lines(by_od, dist_centroids),
  by_desire_plot = base_plot_desire_lines(wfh_desire, districts),
  
  by_vmt = get_vmt_dist(by_trp, distances),
  by_o_vmt = get_o_vmt(by_vmt, taz_dist_trans),
  
  
  # WFRC
  tar_file(trip_gen_by_wfrc_file, "data/cube_output/base_2019/TripGenBY2019.csv"),
  trip_gen_by_wfrc = readr::read_csv(trip_gen_by_wfrc_file)
  
)

# Land use ####
land_use_outputs <- tar_plan(
  # ASIM 
  tar_file(lu_trips, "data/asim_output/landuse/final_trips.csv.gz"),
  tar_file(lu_tours, "data/asim_output/landuse/final_tours.csv.gz"),
  tar_file(lu_persons, "data/asim_output/landuse/final_persons.csv.gz"),
  tar_file(lu_households, "data/asim_output/landuse/final_households.csv.gz"),
  
  lu_trp = readr::read_csv(lu_trips),
  lu_tor = readr::read_csv(lu_tours),
  lu_per = readr::read_csv(lu_persons),
  lu_hh = readr::read_csv(lu_households),
  
  by_lu_tor = get_o_tours(by_tor, lu_tazs),
  by_lu_trp = get_trips_from_tours(by_trp, by_lu_tor, distances),
  by_lu_vmt = get_vmt(by_lu_trp),
  
  lu_tazs = c(2138, 2140, 2141, 2149, 2170),
  
  lu_new_tours = get_o_tours(lu_tor, lu_tazs),
  lu_new_trips = get_trips_from_tours(lu_trp, lu_new_tours, distances),
  lu_new_vmt = get_lu_vmt(lu_new_trips, lu_tazs),
  # lu_cf_vmt = combine_scenarios(list(base = by_lu_vmt, land_use = lu_new_vmt)),
  lu_vmt_plot = make_lu_vmt_plot(lu_new_vmt),
  
  lu_desire_lines = make_desire_lines(lu_new_trips, dist_centroids, lu_tazs, taz_dist_trans),
  lu_desire_map = plot_desire_lines(lu_desire_lines, districts, plot_lims)
  
)

# Transit ####
transit_outputs <- tar_plan(
  # ASIM
  tar_file(tr_trips, "data/asim_output/transit/final_trips.csv.gz"),
  tar_file(tr_tours, "data/asim_output/transit/final_tours.csv.gz"),
  tar_file(tr_persons, "data/asim_output/transit/final_persons.csv.gz"),
  tar_file(tr_households, "data/asim_output/transit/final_households.csv.gz"),
  
  tr_trp = readr::read_csv(tr_trips),
  tr_tor = readr::read_csv(tr_tours),
  tr_per = readr::read_csv(tr_persons),
  tr_hh = readr::read_csv(tr_households),
  
  transit_comparison_map = compare_transit_trips(by_trp, tr_trp)
)

# WFH ####
wfh_outputs <- tar_plan(
  # ASIM
  tar_file(wfh_trips, "data/asim_output/wfh/final_trips.csv.gz"),
  tar_file(wfh_tours, "data/asim_output/wfh/final_tours.csv.gz"),
  tar_file(wfh_persons, "data/asim_output/wfh/final_persons.csv.gz"),
  tar_file(wfh_households, "data/asim_output/wfh/final_households.csv.gz"),
  
  wfh_trp = readr::read_csv(wfh_trips),
  wfh_tor = readr::read_csv(wfh_tours),
  wfh_per = readr::read_csv(wfh_persons),
  wfh_hh = readr::read_csv(wfh_households),
  
  wfh_od = make_district_od(wfh_trp, taz_dist_trans),
  wfh_od_diff = diff_od(list(by = by_od, wfh = wfh_od)),
  wfh_desire = better_desire_lines(wfh_od_diff, dist_centroids),
  wfh_desire_plot = better_plot_desire_lines(wfh_desire, districts),
  
  wfh_vmt = get_vmt_dist(wfh_trp, distances),
  wfh_o_vmt = get_o_vmt(wfh_vmt, taz_dist_trans),
  comp_wfh_o_vmt = make_comp_o_vmt(wfh_o_vmt, by_o_vmt, districts),
)


#### Run all targets ###########################################################

tar_plan(
  misc_targets,
  abm_tbm_flowchart,
  synth_pop_comparison,
  base_outputs_comparison,
  base_outputs,
  land_use_outputs,
  transit_outputs,
  wfh_outputs,
)
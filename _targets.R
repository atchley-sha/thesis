#### Setup #####################################################################

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tidyverse", "DiagrammeR", "sf", "ggspatial", "omxr", "qs", "wesanderson", "ggspatial", "scales"),
  memory = "transient",
  garbage_collection = TRUE,
  format = "qs",
  )

r_files <- list.files("R", full.names = TRUE)
sapply(r_files, source)


#### List targets ##############################################################

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

  # Analysis
  asim_pop = read_asim_population(synth_per_file, synth_hh_file),
  se_data = read_zonal_data(zonal_se_file, zonal_income_groups_file),
  pop_comp = make_zonal_comparison(asim_pop, se_data, taz_file),  
  income_groups = read_income_groups(income_groups_file),
  
  # Viz
  inc_groups_map = make_inc_groups_map(pop_comp, income_groups),
  avg_inc_map = make_avg_inc_map(pop_comp),
  inc_comp_plot = make_inc_plot(pop_comp),
  pop_comp_map = make_pop_comp_map(pop_comp),

)

# Base outputs comparison (TLFD/mode choice) ####
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
  tar_files(calibration_iters_files, list.files("data/calibration", full.names = TRUE)),
  tar_target(
    calibration_iters,
    combine_calibration_iters(calibration_iters_files),
    pattern = map(calibration_iters_files)
  ),
  calibration_plot = plot_calibration(calibration_iters),
  comp_modes = compare_mode_split(combined_trips),
  mode_split_comp = make_mode_split_comp(comp_modes),
  
  # WFH
  tar_file(wfrc_telecommute_base_file, "data/base_model_comparison/wfrc/telecommute_base.csv"),
  wfrc_telecommute_base = get_wfrc_telecommute(wfrc_telecommute_base_file),
  wfrc_telecommute_pct = wfrc_telecommute_base$pct,
  wfrc_telecommute_table = wfrc_telecommute_base$table,
  
  wfrc_hbj_base = make_wfrc_hbj(se_data),
  wfrc_hbj_base_pct = wfrc_hbj_base$pct,
  wfrc_hbj_base_plot = wfrc_hbj_base$plot,
)


#### Run all targets ###########################################################

tar_plan(
  abm_tbm_flowchart,
  synth_pop_comparison,
  base_outputs_comparison,
)
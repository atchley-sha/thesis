#### Setup #####################################################################

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("tidyverse", "DiagrammeR", "sf", "ggspatial", "omxr", "qs", "wesanderson"),
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
  tar_target(ex_nodes_file, "data/example_flowchart_comparison/nodes.csv", format = "file"),
  tar_target(ex_trip_file, "data/example_flowchart_comparison/trip-based.csv", format = "file"),
  tar_target(ex_tour_file, "data/example_flowchart_comparison/tour-based.csv", format = "file"),
  tar_target(ex_tbm_nodes_file, "data/example_flowchart_comparison/tbm_zones/tbm_nodes.csv", format = "file"),
  tar_target(ex_tbm_edges_file, "data/example_flowchart_comparison/tbm_zones/tbm_edges.csv", format = "file"),
  ex_tbm_edges = pivot_tbm_edges(ex_tbm_edges_file),
  tar_target(ex_aggregate_file, "data/example_flowchart_comparison/information_pipelines/aggregate.dot", format = "file"),
  tar_target(ex_synthetic_file, "data/example_flowchart_comparison/information_pipelines/synthetic.dot", format = "file"),
  
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
  tar_target(synth_per_file, "data/base_model_comparison/asim/synthetic_persons.csv", format = "file"),
  tar_target(synth_hh_file, "data/base_model_comparison/asim/synthetic_households.csv", format = "file"),
  tar_target(zonal_se_file, "data/base_model_comparison/wfrc/TAZ_SE_2019_WFRC.csv", format = "file"),
  tar_target(zonal_income_groups_file, "data/base_model_comparison/wfrc/Marginal_Income.csv", format = "file"),
  tar_target(taz_file, "data/WFRC_TAZ.geojson", format = "file"),

  # Analysis
  asim_pop = read_asim_population(synth_per_file, synth_hh_file),
  se_data = read_zonal_data(zonal_se_file, zonal_income_groups_file),
  pop_comp = make_zonal_comparison(asim_pop, se_data, taz_file),  

)

# Base outputs comparison (TLFD/mode choice) ####
base_outputs_comparison <- tar_plan(
  # Data
  tar_target(distance_skims, "data/base_model_comparison/wfrc/skm_DY_Dist.omx", format = "file"),
  #some zones have a very high distance and are external; we don't want them
  external_zones = get_ex_zones(distance_skims),
  distances = read_distances(distance_skims, external_zones),
  
  tar_target(wfrc_hbw_trips_od, "data/base_model_comparison/wfrc/trips/HBW_trips_allsegs_pkok.omx", format = "file"),
  tar_target(wfrc_hbo_trips_od, "data/base_model_comparison/wfrc/trips/HBO_trips_allsegs_pkok.omx", format = "file"),
  tar_target(wfrc_nhb_trips_od, "data/base_model_comparison/wfrc/trips/NHB_trips_allsegs_pkok.omx", format = "file"),
  wfrc_hbw_trips = omxr::read_all_omx(wfrc_hbw_trips_od, c("auto", "transit", "nonmotor")),
  wfrc_hbo_trips = omxr::read_all_omx(wfrc_hbo_trips_od, c("auto", "transit", "nonmotor")),
  wfrc_nhb_trips = omxr::read_all_omx(wfrc_nhb_trips_od, c("auto", "transit", "nonmotor")),
  wfrc_trips_od = list(hbw = wfrc_hbw_trips, hbo = wfrc_hbo_trips, nhb = wfrc_nhb_trips),
  
  tar_target(asim_trips_file, "data/base_model_comparison/asim/final_trips.csv", format = "file"),
  tar_target(asim_tours_file, "data/base_model_comparison/asim/final_tours.csv", format = "file"),
  
  wfrc_trips = combine_wfrc_od(wfrc_trips_od, external_zones),
  asim_trips = get_asim_od(asim_trips_file, asim_tours_file, external_zones),
  combined_trips = combine_all_od(wfrc_trips, asim_trips, distances),
)


#### Run all targets ###########################################################

tar_plan(
  abm_tbm_flowchart,
  synth_pop_comparison,
  base_outputs_comparison,
)
library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse", "DiagrammeR"))

r_files <- list.files("R", full.names = TRUE)

sapply(r_files, source)



#### List targets ####

# Data ####
data_targets_flowchart <- tar_plan(
  tar_target(
    ex_nodes_file,
    "data/example_flowchart_comparison/nodes.csv",
    format = "file"
  ),
  tar_target(
    ex_trip_file,
    "data/example_flowchart_comparison/trip-based.csv",
    format = "file"
  ),
  tar_target(
    ex_tour_file,
    "data/example_flowchart_comparison/tour-based.csv",
    format = "file"
  ),
  tar_target(
    ex_tbm_nodes_file,
    "data/example_flowchart_comparison/tbm_zones/tbm_nodes.csv",
    format = "file"
  ),
  tar_target(
    ex_tbm_edges_file,
    "data/example_flowchart_comparison/tbm_zones/tbm_edges.csv",
    format = "file"
  ),
  
  ex_tbm_edges = pivot_tbm_edges(ex_tbm_edges_file),
  
  tar_target(
    ex_aggregate_file,
    "data/example_flowchart_comparison/information_pipelines/aggregate.dot",
    format = "file"
  ),
  tar_target(
    ex_synthetic_file,
    "data/example_flowchart_comparison/information_pipelines/synthetic.dot",
    format = "file"
  )
  
)

data_targets_calibration <- tar_plan(
  tar_target(
    distance_skims,
    "data/base_model_comparison/wfrc/skm_DY_Dist.omx",
    format = "file"
  ),
  tar_target(
    wfrc_trips_skims,
    "data/base_model_comparison/wfrc/trips/AllTrips_pkok.omx",
    format = "file"
  ),
  tar_target(
    asim_trips_file,
    "data/base_model_comparison/asim/final_trips.csv",
    format = "file"
  ),
  tar_target(
    asim_tours_file,
    "data/base_model_comparison/asim/final_tours.csv",
    format = "file"
  ),
  
  tar_target(
    synth_per_file,
    "data/base_model_comparison/asim/synthetic_persons.csv",
    format = "file"
  ),
  tar_target(
    synth_hh_file,
    "data/base_model_comparison/asim/synthetic_households.csv",
    format = "file"
  ),
  tar_target(
    zonal_se_file,
    "data/base_model_comparison/wfrc/TAZ_SE_2019_WFRC.csv",
    format = "file"
  )
)

# Analysis ####
analysis_targets <- tar_plan(
  asim_pop = read_asim_population(synth_per_file, synth_hh_file),
  se_data = read_zonal_data(zonal_se_file),
  pop_comp = make_zonal_comparison(asim_pop, se_data)
)

# Visualization ####
viz_targets <- tar_plan(
  trip_ex = make_ex_dap_viz(
    nodes = ex_nodes_file,
    edges = ex_trip_file,
    dot_file = "output/example_flowchart_comparison/trip.dot",
    image_file = "output/example_flowchart_comparison/trip.png"
  ),
  tour_ex = make_ex_dap_viz(
    nodes = ex_nodes_file,
    edges = ex_tour_file,
    dot_file = "output/example_flowchart_comparison/tour.dot",
    image_file = "output/example_flowchart_comparison/tour.png"
  ),
  tbm_ex = make_ex_tbm_viz(
    nodes = ex_tbm_nodes_file,
    edges = ex_tbm_edges,
    dot_file = "output/example_flowchart_comparison/tbm.dot",
    image_file = "output/example_flowchart_comparison/tbm.png"
  ),
  
  ex_aggregate = render_dot_graph(
    dot_file = ex_aggregate_file,
    image_file = "output/example_flowchart_comparison/aggregate.png"
  ),
  ex_synthetic = render_dot_graph(
    dot_file = ex_synthetic_file,
    image_file = "output/example_flowchart_comparison/synthetic.png"
  )
  
)


#### Run all targets ######################################

tar_plan(
  data_targets_flowchart,
  data_targets_calibration,
  analysis_targets,
  viz_targets
)
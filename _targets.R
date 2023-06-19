library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse", "DiagrammeR"))

source("R/flowchart_examples.R")

data_targets <- tar_plan(
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
    ex_tbm_nodes,
    "data/example_flowchart_comparison/tbm_zones/tbm_nodes.csv",
    format = "file"
  ),
  tar_target(
    ex_tbm_edges,
    "data/example_flowchart_comparison/tbm_zones/tbm_edges.csv",
    format = "file"
  )
  
)


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
    nodes = ex_tbm_nodes,
    edges = ex_tbm_edges,
    dot_file = "output/example_flowchart_comparison/tbm.dot",
    image_file = "output/example_flowchart_comparison/tbm.png"
  )
  
)


#### Run all targets ######################################

tar_plan(
  data_targets,
  viz_targets
  )
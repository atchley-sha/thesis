library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse", "DiagrammeR"))

r_files <- list.files("R", full.names = TRUE)

sapply(r_files, source)

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
  ),
  
  #legends for the network assignment figures
  legend_nodes = tibble(
    where = c("b1", "b2", "d1", "d2", "w1", "w2"),
    x = rep(c(3.3,4.5), 3),
    y = c(rep(-0.1,2), rep(0.3, 2), rep(-0.5,2)),
    shape = "none",
    width = 0.4
  ),
  legend_edges = tibble(
    from = c("b1", "d1", "w1"),
    to = c("b2", "d2", "w2"),
    mode = c("transit", "drive", "walk"),
    taillabel = str_to_title(mode)
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
  data_targets,
  viz_targets
)
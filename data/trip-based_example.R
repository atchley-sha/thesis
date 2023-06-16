library(tidyverse)
library(DiagrammeR)

create_graph() %>% 
  add_nodes_from_table("data/nodes.csv", label_col = where) %>% 
  add_edges_from_table("data/trip-based.csv", from, to, from_to_map = label) %>% 
  mutate_edge_attrs(label = str_to_title(mode)) %>% 
  colorize_edge_attrs(mode, color, palette = "Dark2") %>% 
  render_graph()

create_graph(attr_theme = NULL) %>% 
  add_nodes_from_table("data/nodes.csv", label_col = where) %>% 
  add_edges_from_table("data/tour-based.csv", from, to, from_to_map = label) %>% 
  mutate_edge_attrs(label = str_to_title(mode)) %>%
  colorize_edge_attrs(mode, color, palette = "Dark2") %>% 
  render_graph()

graph <- create_graph(attr_theme = NULL) %>% 
  add_nodes_from_table(nodes, label_col = where) %>% 
  add_edges_from_table(edges, from, to, from_to_map = label) %>% 
  colorize_edge_attrs(mode, color, palette = "Dark2")

render_graph(graph)

generate_dot(graph) %>% 
  gsub("\'", "\"", .) %T>%
  writeLines() %>% 
  write_file("test.dot")

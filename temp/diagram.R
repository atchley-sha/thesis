library(DiagrammeR)
library(tidyverse)

test <- create_graph() %>% 
  add_nodes_from_table("data/tbm_nodes.csv", label_col = label) %>% 
  add_edges_from_table("data/tbm_edges.csv", from_col = from, to_col = to, from_to_map = id_external) %>% 
  set_node_attrs(shape, "box") %>% 
  set_node_attrs(fixedsize, "false") %>% 
  set_node_position(1,1,1) %>% 
  set_node_position(2,2,2) %>% 
  set_node_position(22,3,2)
  
render_graph(test)

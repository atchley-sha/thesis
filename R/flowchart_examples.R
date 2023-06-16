# Make example DAP network assignment
make_ex_dap_viz <- function(
    nodes,
    edges,
    dot_file,
    image_file){
  graph <- create_graph(attr_theme = NULL) %>% 
    add_nodes_from_table(nodes) %>% 
    add_edges_from_table(edges, from, to, from_to_map = where) %>% 
    set_node_attrs(shape, "none") %>% 
    mutate_edge_attrs(label = str_to_title(mode)) %>% 
    colorize_edge_attrs(mode, color, palette = "Dark2")
  
    # Turn graph into DOT code since DiagrammeR has issues with images?
    generate_dot(graph) %>% 
      #switch ' to " since that's what DOT needs
      str_replace_all("\'", "\"") %>%
      write_file(dot_file)
    
    # Render DOT file to .png
    system2("dot", args = c("-Tpng", dot_file, "-o", image_file))
    
}
# Make example DAP network assignment
make_ex_dap_viz <- function(
    nodes,
    edges,
    dot_file,
    image_file) {
  
  legend_nodes <- tibble(
      where = c("b1", "b2", "d1", "d2", "w1", "w2"),
      x = rep(c(3.3,4.5), 3),
      y = c(rep(-0.1,2), rep(0.3, 2), rep(-0.5,2)),
      shape = "none",
      width = 0.4
    )
  legend_edges <- tibble(
      from = c("b1", "d1", "w1"),
      to = c("b2", "d2", "w2"),
      mode = c("transit", "drive", "walk"),
      taillabel = str_to_title(mode)
    )
  
  graph <- create_graph(attr_theme = NULL) %>%
    add_nodes_from_table(nodes) %>%
    add_edges_from_table(edges, from, to, from_to_map = where) %>%
    mutate_edge_attrs(label = " ") %>%
    set_node_attrs(shape, "circle") %>%
    add_nodes_from_table(legend_nodes) %>% 
    set_node_attrs(label, "") %>%
    add_edges_from_table(legend_edges, from, to, from_to_map = where) %>%
    set_edge_attrs(penwidth, 4) %>% 
    colorize_edge_attrs(mode, color, palette = "Dark2")
  
  # Turn graph into DOT code since DiagrammeR has issues with images?
  generate_dot(graph) %>%
    #switch ' to " since that's what DOT needs
    str_replace_all("\'", "\"") %>%
    write_file(dot_file)
  
  render_dot_graph(dot_file, image_file, "fdp")
  
  graph
  
}

# Make example TBM network assignment
make_ex_tbm_viz <- function(
    nodes,
    edges,
    dot_file,
    image_file) {
  
  legend_nodes <- tibble(
    where = c("b1", "b2", "d1", "d2", "w1", "w2"),
    x = rep(c(2.3,2.8), 3),
    y = c(rep(-0.2,2), rep(0.0, 2), rep(-0.4,2)),
    shape = "none",
    width = 0.4
  )
  legend_edges <- tibble(
    from = c("b1", "d1", "w1"),
    to = c("b2", "d2", "w2"),
    mode = c("transit", "drive", "walk"),
    taillabel = str_to_title(mode),
    penwidth = 4
  )
  
  graph <- create_graph(attr_theme = NULL) %>%
    add_nodes_from_table(nodes) %>%
    add_edges_from_table(edges, from, to, from_to_map = where) %>%
    # mutate_edge_attrs(label = number) %>% 
    copy_edge_attrs(number, penwidth) %>% 
    rescale_edge_attrs(penwidth, 2, 10) %>% 
    set_node_attrs(shape, "circle") %>%
    add_nodes_from_table(legend_nodes) %>% 
    set_node_attrs(label, "") %>%
    add_edges_from_table(legend_edges, from, to, from_to_map = where) %>%
    # set_edge_attrs(penwidth, 4) %>% 
    colorize_edge_attrs(mode, color, palette = "Dark2")
  
  # Turn graph into DOT code since DiagrammeR has issues with images?
  generate_dot(graph) %>%
    #switch ' to " since that's what DOT needs
    str_replace_all("\'", "\"") %>%
    write_file(dot_file)
  
  render_dot_graph(dot_file, image_file, "fdp")
  
  graph
  
}


# Render DOT graph to .png
render_dot_graph <- function(
    dot_file,
    image_file,
    engine = "dot") {
  
  # Render DOT file to .png
  dot_status <-
    system2(engine, args = c("-Tpng", dot_file, "-o", image_file))
  
  if (dot_status != 0) {
    stop(
      "\n\nCould not create DOT graph. Make sure Graphviz (https://graphviz.org)\nis installed and you can run the `dot` command in the default shell.\n"
    )
  }
  
}
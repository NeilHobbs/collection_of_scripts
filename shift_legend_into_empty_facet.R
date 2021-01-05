#https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
#Function written by Artem Sokolov
shift_legend_into_empty_facet = function(your.graph) {
  pnls = cowplot::plot_to_gtable(your.graph) %>% 
    gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% 
    purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}
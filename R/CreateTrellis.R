#' @export
createTrellis <- function(ggplotArray, maxColumns) {
  # Load required packages
  requireNamespace("ggplot2")
  requireNamespace("patchwork")
  
  # Initialize the combined plot
  combinedPlot <- NULL
  
  # Loop through the ggplot objects
  for (i in seq_along(ggplotArray)) {
    # Add the plot to the combined plot
    if (is.null(combinedPlot)) {
      combinedPlot <- ggplotArray[[i]]
    } else {
      combinedPlot <- combinedPlot + ggplotArray[[i]]
    }
    
    # Arrange the plots in a grid with shared axes
    if (i %% maxColumns == 0 || i == length(ggplotArray)) {
      combinedPlot <- combinedPlot +
        patchwork::plot_layout(ncol = min(maxColumns, length(ggplotArray)),
                               guides = "collect") &
        patchwork::plot_annotation(tag_levels = 'A')
    }
  }
  
  return(combinedPlot)
}

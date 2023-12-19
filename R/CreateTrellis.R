#' Create a Combined Trellis Plot from a List of ggplot Objects
#'
#' This function takes an array of ggplot objects and combines them into a single trellis plot.
#' The plots are arranged in a grid with a specified maximum number of columns.
#'
#' @param ggplotArray A list of ggplot objects to be combined into a trellis plot.
#' @param maxColumns The maximum number of columns to use in the trellis layout.
#' @return A combined ggplot object representing the trellis plot.
#' @export
#' @examples
#' # Assuming you have a list of ggplot objects named myPlots
#' combinedPlot <- createTrellis(myPlots, 2)
#' print(combinedPlot)
createTrellis <- function(ggplotArray, maxColumns) {
  # Load required packages
  requireNamespace("ggplot2")
  requireNamespace("patchwork")
  requireNamespace("checkmate")
  
  # Check input validity
  checkmate::assertList(
    ggplotArray,
    min.len = 1,
    check.names = FALSE,
    null.ok = FALSE,
    .var.name = "ggplotArray"
  )
  checkmate::assertInt(maxColumns, lower = 1,
                       .var.name = "maxColumns")
  
  # Check if all elements in ggplotArray are ggplot objects
  for (i in seq_along(ggplotArray)) {
    checkmate::assertClass(ggplotArray[[i]],
                           "ggplot",
                           .var.name = paste0("ggplotArray[[", i, "]]"))
  }
  
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

#' @export
# Function to arrange ggplot objects with shared axes and labels using patchwork package
arrangeGgplots <-
  function(ggplotList,
           layoutInstruction,
           shareX = FALSE,
           shareY = FALSE) {
    # Construct the combined plot using the layout instruction
    combinedPlot <-
      eval(parse(
        text = paste0(
          "patchwork::wrap_plots(ggplotList[[1]], ",
          layoutInstruction,
          ")"
        )
      ))
    
    # Apply shared axes and labels if requested
    if (shareX || shareY) {
      combinedPlot <-
        combinedPlot & patchwork::plot_layout(guides = 'collect')
      if (shareX) {
        combinedPlot <-
          combinedPlot & patchwork::plot_layout(scales = 'free_x')
      }
      if (shareY) {
        combinedPlot <-
          combinedPlot & patchwork::plot_layout(scales = 'free_y')
      }
    }
    
    # Return the combined plot
    return(combinedPlot)
  }

#' Arrange Multiple ggplot Objects with Shared Axes and Labels
#'
#' This function arranges multiple ggplot objects into a single plot layout,
#' with options to share X and Y axes as well as labels.
#' It uses the `patchwork` package to create a composite plot.
#'
#' @param ggplotList A list of ggplot objects to be arranged.
#' @param layoutInstruction A string representing the layout instruction for the `patchwork::wrap_plots` function.
#' @param shareX Logical; if `TRUE`, the X axes will be shared across plots. Defaults to `FALSE`.
#' @param shareY Logical; if `TRUE`, the Y axes will be shared across plots. Defaults to `FALSE`.
#'
#' @return A ggplot object representing the combined layout of the input plots.
#'
#' @examples
#' # Example usage:
#' # arrangeGgplots(list(ggplot1, ggplot2), layoutInstruction = "1|2", shareX = TRUE)
#' @export
arrangeGgplots <-
  function(ggplotList,
           layoutInstruction,
           shareX = FALSE,
           shareY = FALSE) {
    # Validate inputs
    checkmate::assertList(ggplotList, min.len = 1, types = "ggplot")
    checkmate::assertCharacter(layoutInstruction, len = 1)
    checkmate::assertLogical(shareX, len = 1)
    checkmate::assertLogical(shareY, len = 1)
    
    # Construct the combined plot using the layout instruction
    command <- paste0(
      "patchwork::wrap_plots(",
      "list(",
      paste("ggplotList[[", seq_along(ggplotList), "]]", collapse = ", "),
      "), ",
      "'",
      layoutInstruction,
      "'",
      ")"
    )
    combinedPlot <- eval(parse(text = command))
    
    # Apply shared axes and labels if requested
    if (shareX || shareY) {
      combinedPlot <-
        combinedPlot & patchwork::plot_layout(guides = 'collect')
    }
    
    # Optionally, adjust axes display
    # combinedPlot <- combinedPlot & patchwork::plot_layout(axes = "free")
    
    # Return the combined plot
    return(combinedPlot)
  }

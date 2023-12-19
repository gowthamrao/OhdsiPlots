#' Plot Weighted Data
#'
#' Creates a weighted plot (histogram or bar plot) based on the provided data and weights.
#' The function allows customization of various plot aspects such as title, labels, color, and plot type.
#'
#' @param df A data.frame or a vector. If a vector is provided, it's converted into a data.frame with default weights of 1.
#' @param x The name of the variable to be plotted on the x-axis. If data is a vector, this defaults to "value".
#' @param y The name of the variable representing weights. If data is a vector, this defaults to "weight".
#' @param mainTitle The main title of the plot.
#' @param xLabel The label for the x-axis.
#' @param yLabel The label for the y-axis.
#' @param plotColor The color to be used for plotting.
#' @param plotType The type of plot to create, either "histogram" or "bar".
#' @param binwidth The bin width for the histogram. If NULL, it is automatically determined.
#' @param useScinot A logical indicating whether to use scientific notation or not.
#' @param plotTheme The ggplot2 theme to be applied to the plot.
#' @return A ggplot object representing the weighted plot.
#' @export
#' @examples
#' plotWeighted(mtcars, x = "mpg", y = "wt", mainTitle = "Car Weight by MPG", plotType = "bar")
plotWeighted <- function(df,
                         x = "value",
                         y = "weight",
                         mainTitle = "Weighted Plot",
                         xLabel = "Values",
                         yLabel = "Weights",
                         plotColor = "blue",
                         plotType = "histogram",
                         binwidth = NULL,
                         useScinot = FALSE,
                         plotTheme = "theme_minimal") {
  # Checking inputs using checkmate
  checkmate::assertDataFrame(df, null.ok = TRUE)
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertCharacter(mainTitle, len = 1)
  checkmate::assertCharacter(xLabel, len = 1)
  checkmate::assertCharacter(yLabel, len = 1)
  checkmate::assertCharacter(plotColor, len = 1)
  checkmate::assertChoice(plotType, choices = c("histogram", "bar"))
  checkmate::assertNumeric(binwidth, null.ok = TRUE)
  checkmate::assertLogical(useScinot, len = 1)
  checkmate::assertCharacter(plotTheme, len = 1)
  
  if (!useScinot) {
    # Temporarily increase the penalty for scientific notation
    oldScipen <- getOption("scipen")
    options(scipen = 999)
  }
  
  if (!is.data.frame(df)) {
    df <- dplyr::tibble(value = df,
                        weight = 1)
    x <- "value"
    y <- "weight"
  }
  
  if (!x %in% names(df)) {
    stop("Variable for x-axis is not found in the data.")
  }
  
  if (!y %in% names(df)) {
    stop("Variable for weight is not found in the data.")
  }
  
  if (plotType == "histogram") {
    plot <-
      ggplot2::ggplot(df, ggplot2::aes_string(x = x, weight = y)) +
      ggplot2::geom_histogram(fill = plotColor, binwidth = binwidth) +
      ggplot2::ggtitle(mainTitle) +
      ggplot2::xlab(xLabel) +
      ggplot2::ylab(yLabel) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      do.call(plotTheme, list())
  } else if (plotType == "bar") {
    dataSummarised <- df |>
      dplyr::group_by(.data[[x]]) |>
      dplyr::summarise(weight = sum(.data[[y]]))
    
    plot <-
      ggplot2::ggplot(dataSummarised, ggplot2::aes(!!rlang::sym(x), y = weight)) +
      ggplot2::geom_bar(stat = "identity",
                        fill = plotColor) +
      ggplot2::ggtitle(mainTitle) +
      ggplot2::xlab(xLabel) +
      ggplot2::ylab(yLabel) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      do.call(plotTheme, list())
  } else {
    stop("Unsupported plot type. Supported types are 'histogram' and 'bar'.")
  }
  
  if (!useScinot) {
    # Reset the scientific notation penalty
    options(scipen = oldScipen)
  }
  
  return(plot)
}

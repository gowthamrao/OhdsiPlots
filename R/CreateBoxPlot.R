#' Create a Box Plot
#'
#' This function creates a box plot using ggplot2, given a specific data frame structure.
#' It requires certain columns in the data frame and adds annotations for statistical
#' measures like mean and standard deviation.
#'
#' @param df A data frame containing the required columns for plotting.
#' @param title The title of the plot.
#' @param yLabel The label for the y-axis.
#' @param xLabel The label for the x-axis.
#' @return A ggplot object representing the box plot.
#' @examples
#' # Example usage:
#' # createBoxPlot(df = yourDataFrame, title = "Your Title", yLabel = "Y-axis Label", xLabel = "X-axis Label")
#' @export
createBoxPlot <- function(df, title, yLabel, xLabel) {
  # Use checkmate for more informative messages
  checkmate::assertDataFrame(df, min.ncol = 11)
  checkmate::assertCharacter(title, len = 1)
  checkmate::assertCharacter(yLabel, len = 1)
  checkmate::assertCharacter(xLabel, len = 1)
  
  requiredCols <- c(
    "databaseId",
    "countValue",
    "minValue",
    "maxValue",
    "mean",
    "sd",
    "medianValue",
    "p10Value",
    "p25Value",
    "p75Value",
    "p90Value"
  )
  checkmate::assertNames(df, subset.of = requiredCols, add = FALSE)
  
  numericCols <- c(
    "minValue",
    "maxValue",
    "mean",
    "sd",
    "medianValue",
    "p10Value",
    "p25Value",
    "p75Value",
    "p90Value"
  )
  df[numericCols] <- lapply(df[numericCols], round, 1)
  
  nPos <-
    min(df$minValue) - (max(df$maxValue) - min(df$minValue)) * 0.1
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = factor(databaseId),
      ymin = p10Value,
      lower = p25Value,
      middle = medianValue,
      upper = p75Value,
      ymax = p90Value
    )
  ) +
    ggplot2::geom_boxplot(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(mean, " +/- ", sd), y = mean), vjust = -0.5) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(
      "(n = ", format(countValue, big.mark = ","), ")"
    ), y = nPos), vjust = 1.5) +
    ggplot2::scale_y_continuous(yLabel) +
    ggplot2::labs(x = xLabel, title = title) +
    ggplot2::theme_minimal()
}

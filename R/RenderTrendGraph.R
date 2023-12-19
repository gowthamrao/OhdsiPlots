#' Render Trend Graph
#'
#' This function creates a trend graph based on the given data frame. It allows customization of various aspects
#' of the graph such as axis labels, transformations, and grouping.
#'
#' @param df A data frame containing the data to be plotted. Must contain columns 'date' and 'count'.
#' @param xAxisLabel A string specifying the label for the x-axis. Default is "Date".
#' @param yAxisLabel A string specifying the label for the y-axis. Default is "Volume".
#' @param yScaleTransform A string specifying the transformation for the y-axis scale. Can be "identity", "log", "sqrt", etc.
#' @param ggplotTheme A ggplot2 theme object. Used to customize the appearance of the graph.
#' @param showRawValues Logical, whether to show raw values as points on the graph.
#' @param groupBy A string specifying the column name in 'df' to group the data by. NULL means no grouping.
#' @param smoothLines Logical, whether to use smooth lines instead of regular lines.
#' @param labelGroup Logical, whether to label the groups if grouping is used.
#' @export
renderTrendGraph <- function(df,
                             xAxisLabel = "Date",
                             yAxisLabel = "Volume",
                             yScaleTransform = "identity",
                             ggplotTheme = ggplot2::theme_minimal(),
                             showRawValues = FALSE,
                             groupBy = NULL,
                             smoothLines = FALSE,
                             labelGroup = FALSE) {
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but is not installed.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required but is not installed.")
  }
  if (!requireNamespace("checkmate", quietly = TRUE)) {
    stop("Package 'checkmate' is required but is not installed.")
  }
  
  # Validate inputs using checkmate
  checkmate::assertDataFrame(df, cols = c("date", "count"))
  checkmate::assertCharacter(xAxisLabel, len = 1)
  checkmate::assertCharacter(yAxisLabel, len = 1)
  checkmate::assertCharacter(yScaleTransform, len = 1)
  checkmate::assertFlag(showRawValues)
  if (!is.null(groupBy)) {
    checkmate::assertChoice(groupBy, choices = names(df))
  }
  checkmate::assertFlag(smoothLines)
  checkmate::assertFlag(labelGroup)
  
  # Determine the appropriate date breaks
  dateRange <- range(df$date)
  numDays <- difftime(dateRange[2], dateRange[1], units = "days")
  dateBreaks <- if (numDays > 730) {
    "2 years"
  } else if (numDays > 365) {
    "1 year"
  } else if (numDays > 30) {
    "1 month"
  } else {
    "1 week"
  }
  
  # Custom function for date formatting
  formatDate <- function(x) {
    format(x, format = "%Y-%m-%d")
  }
  
  # Create the base plot with conditional aesthetics
  if (!is.null(groupBy)) {
    aes_group <- ggplot2::aes_string(group = groupBy, color = groupBy)
  } else {
    aes_group <- ggplot2::aes()
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = count)) +
    aes_group +
    (if (smoothLines)
      ggplot2::geom_smooth()
     else
       ggplot2::geom_line()) +
    ggplot2::scale_x_date(date_breaks = dateBreaks, labels = formatDate) +
    ggplot2::xlab(xAxisLabel) +
    ggplot2::ylab(yAxisLabel) +
    ggplot2::scale_y_continuous(trans = yScaleTransform) +
    ggplotTheme
  
  # Add points for raw values if requested
  if (showRawValues) {
    p <- p + ggplot2::geom_point()
  }
  
  # Add labels for groups if requested
  if (labelGroup && !is.null(groupBy)) {
    p <-
      p + ggplot2::geom_label(
        ggplot2::aes_string(label = groupBy),
        hjust = 1,
        vjust = 1,
        position = ggplot2::position_nudge(y = 10)
      )
  }
  
  # Return the plot
  return(p)
}

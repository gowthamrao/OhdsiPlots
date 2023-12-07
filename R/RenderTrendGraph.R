#' @export
renderTrendGraph <- function(dataFrame, 
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
  
  # Determine the appropriate date breaks
  dateRange <- range(dataFrame$date)
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
  p <- ggplot2::ggplot(dataFrame, ggplot2::aes(x = date, y = count)) +
    aes_group +
    (if (smoothLines) ggplot2::geom_smooth() else ggplot2::geom_line()) +
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
    p <- p + ggplot2::geom_label(ggplot2::aes_string(label = groupBy), hjust = 1, vjust = 1, position = ggplot2::position_nudge(y = 10))
  }
  
  # Return the plot
  return(p)
}

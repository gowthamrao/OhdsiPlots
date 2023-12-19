#' Plot a Trend Curve
#'
#' This function creates a trend curve plot from a given data tibble. It allows for various customizations including smoothing methods, showing standard error, cumulative data, raw values, and more.
#'
#' @param df A tibble containing the data to be plotted.
#' @param dateColumn A string specifying the column name in `df` that contains date values.
#' @param smoothingMethod A string indicating the smoothing method to be used. "auto" by default.
#' @param showStandardError Logical indicating whether to show standard error in the plot. Defaults to TRUE.
#' @param outputFilename A string specifying the path to save the plot image. If NULL, the plot is not saved. Defaults to NULL.
#' @param plotTitle A string for the plot title. Defaults to "Trend Curve".
#' @param xAxisLabel A string for the x-axis label. Defaults to "Date".
#' @param yAxisLabel A string for the y-axis label. Defaults to "Volume".
#' @param yScaleTransform A string specifying the transformation for the y-axis scale. Defaults to "identity".
#' @param ggplotTheme A `ggplot2` theme object. Defaults to `theme_minimal()`.
#' @param dateLabelDf A tibble with columns 'date' and 'label' for annotating specific dates. NULL by default.
#' @param cumulative Logical indicating if the plot should display cumulative values. Defaults to FALSE.
#' @param showRawValues Logical indicating if raw data points should be displayed on the plot. Defaults to FALSE.
#' @param showCumulativeValues Logical indicating if cumulative values should be annotated on the plot. Defaults to FALSE.
#' @param cumulativeYPosition Numeric indicating the Y-axis position for cumulative value annotations. Defaults to -10.
#' @param cumulativeJitter Numeric indicating the amount of jitter to apply to cumulative annotations to avoid overlap. Defaults to 3.
#' @param dateRounding A string specifying the unit for rounding dates. "none" by default.
#'
#' @return None
#' @export
plotTrendCurve <- function(df,
                           dateColumn,
                           smoothingMethod = "auto",
                           showStandardError = TRUE,
                           outputFilename = NULL,
                           plotTitle = "Trend Curve",
                           xAxisLabel = "Date",
                           yAxisLabel = "Volume",
                           yScaleTransform = "identity",
                           ggplotTheme = ggplot2::theme_minimal(),
                           dateLabelDf = NULL,
                           cumulative = FALSE,
                           showRawValues = FALSE,
                           showCumulativeValues = FALSE,
                           cumulativeYPosition = -10,
                           cumulativeJitter = 3,
                           dateRounding = "none") {
  # Check if required packages are installed
  checkmate::assert_require_namespaces(c("ggplot2", "dplyr", "lubridate"))
  
  # Validate the df to make sure it has the required dateColumn
  checkmate::assert_data_frame(df)
  checkmate::assert_subset(dateColumn, choices = names(df), add = "dateColumn must be a column in df")
  
  # Validate the dateLabelDf if provided
  if (!is.null(dateLabelDf)) {
    checkmate::assert_data_frame(dateLabelDf)
    checkmate::assert_subset(c("date", "label"),
                             choices = names(dateLabelDf),
                             add = "dateLabelDf must have columns 'date' and 'label'")
  }
  
  # Validate other arguments
  checkmate::assert_character(smoothingMethod, any.missing = FALSE)
  checkmate::assert_logical(showStandardError)
  checkmate::assert_logical(cumulative)
  checkmate::assert_logical(showRawValues)
  checkmate::assert_logical(showCumulativeValues)
  checkmate::assert_numeric(cumulativeYPosition)
  checkmate::assert_numeric(cumulativeJitter)
  checkmate::assert_character(dateRounding, any.missing = FALSE)
  if (!is.null(outputFilename))
    checkmate::assert_character(outputFilename, any.missing = FALSE)
  
  # Round the date if requested
  if (dateRounding != "none") {
    df <- df |>
      dplyr::mutate(
        !!rlang::sym(dateColumn) := lubridate::floor_date(!!rlang::sym(dateColumn), unit = dateRounding)
      )
  }
  
  # Pre-aggregate data to get the counts for each date
  aggregatedData <-
    dplyr::count(df,!!rlang::sym(dateColumn), name = "volume")
  
  # Prepare the cumulative data, regardless of the `cumulative` setting, if `showCumulativeValues` is TRUE
  if (showCumulativeValues) {
    cumulativeData <- aggregatedData |>
      dplyr::arrange(!!rlang::sym(dateColumn)) |>
      dplyr::mutate(volume = cumsum(.data$volume))
  }
  
  # If the plot itself should be cumulative, replace aggregatedData with its cumulative version
  if (cumulative) {
    aggregatedData <- cumulativeData
  }
  
  # Create the base ggplot2 object
  plot <-
    ggplot2::ggplot(aggregatedData,
                    ggplot2::aes_string(x = dateColumn, y = "volume")) +
    ggplot2::scale_y_continuous(trans = yScaleTransform) +
    ggplot2::labs(title = plotTitle,
                  x = xAxisLabel,
                  y = yAxisLabel) +
    ggplotTheme
  
  # Add raw values as light gray dots if requested
  if (showRawValues) {
    plot <- plot + ggplot2::geom_point(color = "lightgray")
  }
  
  # Add either the smoothing layer or the line layer based on smoothingMethod
  if (!is.null(smoothingMethod)) {
    plot <-
      plot + ggplot2::geom_smooth(method = smoothingMethod, se = showStandardError)
  } else {
    plot <- plot + ggplot2::geom_line()
  }
  
  # Add date annotations if dateLabelDf is provided
  if (!is.null(dateLabelDf)) {
    maxY <- max(aggregatedData$volume, na.rm = TRUE)
    plot <- plot + ggplot2::geom_text(
      data = dateLabelDf,
      ggplot2::aes(x = date, y = maxY, label = label),
      nudge_y = 0.1,
      check_overlap = TRUE
    )
  }
  
  # Add cumulative count annotations below the x-axis if requested
  if (showCumulativeValues) {
    # Extract the last date of each month
    lastDates <- cumulativeData |>
      dplyr::mutate(month = lubridate::floor_date(!!rlang::sym(dateColumn), "month")) |>
      dplyr::group_by(month) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
    
    # Add jitter to the y-position to avoid overlap
    jitteredYPosition <-
      cumulativeYPosition + runif(nrow(lastDates),-cumulativeJitter, cumulativeJitter)
    
    # Add the annotations
    plot <- plot + ggplot2::annotate(
      "text",
      x = lastDates[[dateColumn]],
      y = jitteredYPosition,
      label = scales::comma(lastDates$volume),
      size = 3,
      color = "lightgray"
    )
  }
  
  # Display the plot
  print(plot)
  
  # Save the plot to a file if outputFilename is provided
  if (!is.null(outputFilename)) {
    ggplot2::ggsave(outputFilename, plot)
  }
}

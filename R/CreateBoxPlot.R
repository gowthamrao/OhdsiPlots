#' @export
createBoxPlot <- function(df,
                          title,
                          yLabel,
                          xLabel) {
  # Check if required columns are present in the dataframe
  requiredCols <-
    c(
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
  if (!all(requiredCols %in% names(df))) {
    stop("Data frame does not have all required columns")
  }
  
  # Apply rounding to one decimal place
  numericCols <-
    c(
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
  
  # Determine the position for displaying 'n ='
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

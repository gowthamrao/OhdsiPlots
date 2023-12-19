#' Plot Temporal Compare Standardized Difference
#'
#' This function generates a static plot to compare standardized differences over time.
#' It allows for optional faceting based on a specified column and includes various parameter checks.
#'
#' @param df Data frame containing the data to plot.
#' @param domain Domain to filter on, defaults to "all".
#' @param threshold Threshold for row count filtering, defaults to 1000.
#' @param thresholdValue Value threshold for filtering data, defaults to 0.01.
#' @param facetOn (Optional) Column name to use for faceting.
#' @param plotTitle Title of the plot, defaults to "Temporal Comparison of Standardized Differences".
#' @param xAxisLabel Label for the X-axis, defaults to "Prevalence in Target Cohort".
#' @param yAxisLabel Label for the Y-axis, defaults to "Prevalence in Comparator Cohort".
#' @return A ggplot object representing the plot.
#' @examples
#' # Example usage:
#' plot <- plotCompareStandardizedDifference(df, domain = "all")#'
#' @export
plotCompareStandardizedDifference <- function(df,
                                              domain = "all",
                                              threshold = 1000,
                                              thresholdValue = 0.01,
                                              facetOn = NULL,
                                              plotTitle = "Comparison of Standardized Differences",
                                              xAxisLabel = "Prevalence in Target Cohort",
                                              yAxisLabel = "Prevalence in Comparator Cohort") {
  # Load checkmate for assertions
  requireNamespace("checkmate", quietly = TRUE)
  
  # Assertions
  checkmate::assertDataFrame(
    df,
    col.names = c("mean1", "mean2", "sumValue1", "sumValue2", "domainId"),
    col.types = c(
      "numeric",
      "numeric",
      "integerish",
      "integerish",
      "character"
    ),
    any.missing = FALSE,
    add = FALSE
  )
  
  checkmate::assertCharacter(domain, min.len = 1)
  checkmate::assertNumeric(threshold, lower = 1)
  checkmate::assertNumeric(thresholdValue, lower = 0)
  checkmate::assertCharacter(facetOn, min.len = 1, null.ok = TRUE)
  checkmate::assertCharacter(plotTitle, min.len = 1)
  checkmate::assertCharacter(xAxisLabel, min.len = 1)
  checkmate::assertCharacter(yAxisLabel, min.len = 1)
  
  # Additional checks specific to domain and facetOn
  if (!is.null(facetOn) && !facetOn %in% names(df)) {
    stop("The specified field for faceting does not exist in the dataset.")
  }
  
  # Define domains
  domains <- c(
    "Condition",
    "Device",
    "Drug",
    "Measurement",
    "Observation",
    "Procedure",
    "Demographics"
  )
  
  df$domainId[!df$domainId %in% domains] <- "Other"
  if (domain != "all") {
    df <- df |> dplyr::filter(.data$domainId == domain)
  }
  
  # Apply filtering based on the threshold and thresholdValue
  if (nrow(df) > threshold) {
    df <- df |>
      dplyr::filter(.data$mean1 > thresholdValue |
                      .data$mean2 > thresholdValue) |>
      dplyr::filter(.data$sumValue1 > 0 & .data$sumValue2 > 0)
  }
  
  # Define color palette
  colors <- setNames(
    c(
      "#1B9E77",
      "#D95F02",
      "#7570B3",
      "#E7298A",
      "#66A61E",
      "#E6AB02",
      "#444444"
    ),
    c(domains, "Other")
  )
  
  df$domainId <- factor(df$domainId, levels = c(domains, "Other"))
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  # Create the ggplot
  p <-
    ggplot2::ggplot(df, ggplot2::aes(x = mean1, y = mean2, color = domainId)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = plotTitle, x = xAxisLabel, y = yAxisLabel) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "black"
    )
  
  # Add faceting if the field is provided and exists
  if (!is.null(facetOn)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste0("~ ", facetOn)))
  }
  
  return(p)
}

#' Plot Incidence Rates Stratified by Age, Gender, and Calendar Year
#'
#' This function creates a line plot of incidence rates stratified by age groups, gender, and calendar years.
#' The data is presented in a facet grid format allowing comparisons across different demographics.
#'
#' @param df A data frame containing the variables `calendarYear`, `incidenceRate`, `gender`, `ageGroup`.
#' @param y1Label A string specifying the label for the Y-axis. Default is "Incidence Rate per 1k PY".
#' @param x1Label A string specifying the label for the X-axis. Default is "Calendar Year".
#' @param scaleYaxis A logical indicating whether to scale the Y-axis independently for each facet. Default is TRUE.
#' @return A ggplot object representing the line plot.
#' @examples
#' # Example usage:
#' # plotCohortDiagnosticsIncidencePlotStratifiedByAgeGenderCalendarYear(myDataFrame)
#' @export
plotCohortDiagnosticsIncidencePlotStratifiedByAgeGenderCalendarYear <-
  function(df,
           y1Label = "Incidence Rate per 1k PY",
           x1Label = "Calendar Year",
           scaleYaxis = TRUE) {
    # Load required libraries
    require(ggplot2)
    
    # Check the inputs
    checkmate::assertDataFrame(df, min.rows = 1, min.cols = 4)
    checkmate::assertCharacter(y1Label, len = 1)
    checkmate::assertCharacter(x1Label, len = 1)
    checkmate::assertLogical(scaleYaxis, len = 1)
    
    # Convert ageGroup to a factor sorted by the first integer
    df$ageGroup <-
      factor(df$ageGroup, levels = unique(df$ageGroup[order(as.numeric(sapply(strsplit(df$ageGroup, "-"), `[`, 1)))]))
    
    # Determine scales argument based on scaleYaxis
    scales_arg <- ifelse(scaleYaxis, "free_x", "free")
    
    # Create the plot
    ggplot(df,
           aes(
             x = calendarYear,
             y = incidenceRate,
             group = interaction(gender, ageGroup),
             color = gender
           )) +
      geom_line() +
      scale_color_manual(values = c("Male" = "#1F77B4", "Female" = "#FFC0CB")) +
      facet_grid(
        database ~ ageGroup,
        scales = scales_arg,
        space = "free_x",
        labeller = label_wrap_gen(width = 12)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = y1Label, x = x1Label) +
      guides(color = guide_legend("Gender"))
  }

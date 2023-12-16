#' @export
plotCohortDiagnosticsIncidencePlotStratifiedByAgeGenderCalendarYear <-
  function(dataFrame,
           y1Label = "Incidence Rate per 1k PY",
           x1Label = "Calendar Year",
           scaleYaxis = TRUE) {
    # New parameter
    # Load required libraries
    require(ggplot2)
    
    # Convert ageGroup to a factor sorted by the first integer
    dataFrame$ageGroup <-
      factor(dataFrame$ageGroup, levels = unique(dataFrame$ageGroup[order(as.numeric(sapply(
        strsplit(dataFrame$ageGroup, "-"), `[`, 1
      )))]))
    
    # Determine scales argument based on scaleYaxis
    scales_arg <- ifelse(scaleYaxis, "free_x", "free")
    
    # Create the plot
    ggplot(
      dataFrame,
      aes(
        x = calendarYear,
        y = incidenceRate,
        group = interaction(gender, ageGroup),
        color = gender
      )
    ) +
      geom_line() +
      scale_color_manual(values = c("Male" = "#1F77B4", "Female" = "#FFC0CB")) +
      facet_grid(
        database ~ ageGroup,
        scales = scales_arg,
        # Use conditional scales argument
        space = "free_x",
        labeller = label_wrap_gen(width = 12)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = y1Label, x = x1Label) +
      guides(color = guide_legend("Gender"))
  }

#' @export
# Function to create a population pyramid bar chart
createPopulationPyramid <-
  function(data,
           ageGroupCol = "ageGroup",
           genderCol = "sex",
           populationCol = "count",
           title = "Population Pyramid",
           maleColor = "#336B91",
           femaleColor = "#EB6622") {
    # Load necessary libraries
    ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
    checkmate <- requireNamespace("checkmate", quietly = TRUE)
    
    # Check for data conformity using checkmate
    checkmate::assertDataFrame(data, min.cols = 3)
    checkmate::assertCharacter(ageGroupCol, len = 1)
    checkmate::assertCharacter(genderCol, len = 1)
    checkmate::assertCharacter(populationCol, len = 1)
    checkmate::assertCharacter(title, len = 1)
    
    # Extract the first integer value from the ageGroupCol
    data$ageGroupNumeric <-
      as.numeric(gsub(".*?(\\d+).*", "\\1", data[[ageGroupCol]]))
    
    # Sort data based on extracted numeric age groups for proper y-axis ordering
    data <- data[order(data$ageGroupNumeric),]
    
    # Create a ggplot bar chart
    plot <- ggplot2::ggplot(data,
                            ggplot2::aes(
                              x = !!rlang::sym(ageGroupCol),
                              fill = !!rlang::sym(genderCol),
                              y = ifelse(
                                test = data[[genderCol]] == "Male",
                                yes = -data[[populationCol]],
                                no = data[[populationCol]]
                              )
                            )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_y_continuous(
        labels = function(x)
          format(abs(x), big.mark = ",", scientific = FALSE),
        limits = max(data[[populationCol]], na.rm = TRUE) * c(-1, 1)
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = c(Male = maleColor, Female = femaleColor)) +
      ggplot2::labs(
        x = "Age Group",
        y = "Population",
        fill = "Gender",
        title = title
      )
    
    return(plot)
  }

# Sample usage with default parameters
# plot <- createPopulationPyramid(data)

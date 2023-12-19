#' Create a Population Pyramid Bar Chart
#'
#' This function creates a population pyramid bar chart using ggplot2.
#' The data must contain age groups, gender, and population counts.
#'
#' @param data A data frame containing the population data.
#' @param ageGroupCol A string specifying the column name for age groups.
#' @param genderCol A string specifying the column name for gender.
#' @param populationCol A string specifying the column name for population counts.
#' @param title A string specifying the title of the chart.
#' @param maleColor A string specifying the color for male population bars.
#' @param femaleColor A string specifying the color for female population bars.
#' @return A ggplot object representing the population pyramid.
#'
#' @examples
#' # Sample usage with default parameters
#' # plot <- createPopulationPyramid(data)
#' @export
createPopulationPyramid <-
  function(df,
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
    checkmate::assertDataFrame(df, min.cols = 3, add = "Data must be a data frame with at least 3 columns.")
    checkmate::assertCharacter(ageGroupCol, len = 1, add = "Age group column name must be a single character string.")
    checkmate::assertCharacter(genderCol, len = 1, add = "Gender column name must be a single character string.")
    checkmate::assertNumeric(df[[populationCol]],
                             any.missing = FALSE,
                             lower = 0,
                             add = "Population column must contain non-negative numeric values.")
    checkmate::assertCharacter(title, len = 1, add = "Title must be a single character string.")
    
    # Extract the starting age from each age group and sort the data
    df$startAge <-
      as.numeric(gsub(".*?(\\d+).*", "\\1", df[[ageGroupCol]]))
    ageGroupsOrdered <- df |>
      dplyr::arrange(startAge) |>
      dplyr::pull(ageGroupCol) |>
      unique()
    
    df[[ageGroupCol]] <-
      factor(df[[ageGroupCol]], levels = ageGroupsOrdered)
    
    # Create a ggplot bar chart
    plot <- ggplot2::ggplot(df,
                            ggplot2::aes(
                              x = !!rlang::sym(ageGroupCol),
                              fill = !!rlang::sym(genderCol),
                              y = ifelse(
                                test = .data[[genderCol]] == "Male",
                                yes = -as.numeric(.data[[populationCol]]),
                                no = as.numeric(.data[[populationCol]])
                              )
                            )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_x_discrete() +
      ggplot2::scale_y_continuous(
        labels = function(x)
          format(abs(x), big.mark = ",", scientific = FALSE),
        limits = max(abs(df[[populationCol]]), na.rm = TRUE) * c(-1, 1)
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

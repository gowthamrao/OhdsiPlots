#' #' Plot Incidence Rate plot shown in Cohort Diagnostics
#' #'
#' #' @description
#' #' Plot Incidence Rate plot shown in Cohort Diagnostics
#' #'
#' #' @param plotData    Plot data.
#' #'
#' #' @param stratifyByAgeGroup    Do you want to stratify by Age group?
#' #'
#' #' @param stratifyByGender    Do you want to stratify by gender?
#' #'
#' #' @param stratifyByCalendarYear    Do you want to stratify by calendar year?
#' #'
#' #' @param yscaleFixed    Do you want to fix y scale?
#' #'
#' #' @return
#' #' A ggplot object
#' #'
#' #' @export
#' plotStratifiedIncidenceRate <- function(plotData,
#'                                         stratifyByAgeGroup = TRUE,
#'                                         stratifyByGender = TRUE,
#'                                         stratifyByCalendarYear = TRUE,
#'                                         yscaleFixed = FALSE) {
#'   errorMessage <- checkmate::makeAssertCollection()
#'   checkmate::assertTibble(
#'     x = plotData,
#'     any.missing = TRUE,
#'     min.rows = 1,
#'     min.cols = 5,
#'     null.ok = FALSE,
#'     add = errorMessage
#'   )
#'   checkmate::assertLogical(
#'     x = stratifyByAgeGroup,
#'     any.missing = FALSE,
#'     min.len = 1,
#'     max.len = 1,
#'     null.ok = FALSE,
#'     add = errorMessage
#'   )
#'   checkmate::assertLogical(
#'     x = stratifyByGender,
#'     any.missing = FALSE,
#'     min.len = 1,
#'     max.len = 1,
#'     null.ok = FALSE,
#'     add = errorMessage
#'   )
#'   checkmate::assertLogical(
#'     x = stratifyByCalendarYear,
#'     any.missing = FALSE,
#'     min.len = 1,
#'     max.len = 1,
#'     null.ok = FALSE,
#'     add = errorMessage
#'   )
#'   checkmate::assertLogical(
#'     x = yscaleFixed,
#'     any.missing = FALSE,
#'     min.len = 1,
#'     max.len = 1,
#'     null.ok = FALSE,
#'     add = errorMessage
#'   )
#'   checkmate::assertDouble(
#'     x = plotData$incidenceRate,
#'     lower = 0,
#'     any.missing = FALSE,
#'     null.ok = FALSE,
#'     min.len = 1,
#'     add = errorMessage
#'   )
#'   checkmate::assertDouble(
#'     x = plotData$cohortId,
#'     lower = 0,
#'     any.missing = FALSE,
#'     null.ok = FALSE,
#'     min.len = 1,
#'     add = errorMessage
#'   )
#'   checkmate::assertCharacter(
#'     x = plotData$databaseId,
#'     any.missing = FALSE,
#'     null.ok = FALSE,
#'     min.len = 1,
#'     add = errorMessage
#'   )
#'   checkmate::assertCharacter(
#'     x = plotData$ageGroup,
#'     any.missing = TRUE,
#'     null.ok = TRUE,
#'     min.len = 1,
#'     add = errorMessage
#'   )
#'   checkmate::assertCharacter(
#'     x = plotData$gender,
#'     any.missing = TRUE,
#'     null.ok = TRUE,
#'     min.len = 1,
#'     add = errorMessage
#'   )
#'   checkmate::assertInteger(
#'     x = plotData$calendarYear,
#'     lower = 0,
#'     any.missing = TRUE,
#'     null.ok = TRUE,
#'     min.len = 1,
#'     add = errorMessage
#'   )
#'   checkmate::reportAssertions(collection = errorMessage)
#'   
#'   plotData <- plotData |>
#'     dplyr::mutate(cohortId = as.character(paste0("C", cohortId))) |>
#'     dplyr::mutate(incidenceRate = round(.data$incidenceRate, digits = 3)) |>
#'     dplyr::mutate(
#'       strataGender = !is.na(.data$gender),
#'       strataAgeGroup = !is.na(.data$ageGroup),
#'       strataCalendarYear = !is.na(.data$calendarYear)
#'     ) |>
#'     dplyr::filter(
#'       .data$strataGender %in% !!stratifyByGender &
#'         .data$strataAgeGroup %in% !!stratifyByAgeGroup &
#'         .data$strataCalendarYear %in% !!stratifyByCalendarYear
#'     ) |>
#'     dplyr::select(-dplyr::starts_with("strata"))
#'   
#'   aesthetics <- list(y = "incidenceRate")
#'   if (stratifyByCalendarYear) {
#'     aesthetics$x <- "calendarYear"
#'     xLabel <- "Calender year"
#'     showX <- TRUE
#'     if (stratifyByGender) {
#'       aesthetics$group <- "gender"
#'       aesthetics$color <- "gender"
#'     }
#'     plotType <- "line"
#'   } else {
#'     xLabel <- ""
#'     if (stratifyByGender) {
#'       aesthetics$x <- "gender"
#'       aesthetics$color <- "gender"
#'       aesthetics$fill <- "gender"
#'       showX <- TRUE
#'     } else if (stratifyByAgeGroup) {
#'       aesthetics$x <- "ageGroup"
#'       showX <- TRUE
#'     } else {
#'       aesthetics$x <- 1
#'       showX <- FALSE
#'     }
#'     plotType <- "bar"
#'   }
#'   
#'   sortShortName <- plotData |>
#'     dplyr::select(cohortId) |>
#'     dplyr::distinct() |>
#'     dplyr::arrange(as.integer(sub(
#'       pattern = "^C", "", x = cohortId
#'     )))
#'   
#'   plotData <- plotData |>
#'     dplyr::arrange(cohortId = factor(cohortId, levels = sortShortName$cohortId),
#'                    cohortId)
#' 
#'   plotData$shortName <- factor(plotData$cohortId,
#'                                levels = sortShortName$cohortId)
#'   
#'   if (stratifyByAgeGroup) {
#'     sortAgeGroup <- plotData |>
#'       dplyr::select(ageGroup) |>
#'       dplyr::distinct() |>
#'       dplyr::arrange(as.integer(sub(
#'         pattern = "-.+$", "", x = ageGroup
#'       )))
#'     
#'     plotData <- plotData |>
#'       dplyr::arrange(ageGroup = factor(ageGroup, levels = sortAgeGroup$ageGroup),
#'                      ageGroup)
#'     
#'     plotData$ageGroup <- factor(plotData$ageGroup,
#'                                 levels = sortAgeGroup$ageGroup)
#'   }
#' 
#'   if (stratifyByGender) {
#'     # Make sure colors are consistent, no matter which genders are included:
#'     genders <- c("Female", "Male", "No matching concept")
#'     # Code used to generate palette:
#'     # writeLines(paste(RColorBrewer::brewer.pal(n = 2, name = "Dark2"), collapse = "\", \""))
#'     colors <- c("#D95F02", "#1B9E77", "#444444")
#'     colors <- colors[genders %in% unique(plotData$gender)]
#'     plotData$gender <- factor(plotData$gender, levels = genders)
#'   }
#'   
#'   plot <-
#'     ggplot2::ggplot(data = plotData, mapping = do.call(what = ggplot2::aes_string, args = aesthetics)) +
#'     ggplot2::xlab(xLabel) +
#'     ggplot2::ylab("Incidence Rate (/1,000 person years)") +
#'     ggplot2::scale_y_continuous(expand = c(0, 0))
#'   
#'   if (stratifyByCalendarYear) {
#'     distinctCalenderYear <- plotData$calendarYear |>
#'       unique() |>
#'       sort()
#'     if (all(!is.na(distinctCalenderYear))) {
#'       if (length(distinctCalenderYear) >= 8) {
#'         plot <-
#'           plot + ggplot2::scale_x_continuous(n.breaks = 8, labels = round)
#'       } else {
#'         plot <-
#'           plot + ggplot2::scale_x_continuous(breaks = distinctCalenderYear)
#'       }
#'     }
#'   }
#'   
#'   
#'   plot <- plot + ggplot2::theme(
#'     legend.position = "top",
#'     legend.title = ggplot2::element_blank(),
#'     axis.text.x = if (showX) {
#'       ggplot2::element_text(angle = 90, vjust = 0.5)
#'     } else {
#'       ggplot2::element_blank()
#'     }
#'   )
#'   
#'   if (plotType == "line") {
#'     plot <- plot +
#'       ggplot2::geom_line(mapping = ggplot2::aes(), size = 1, alpha = 0.6)
#'   } else {
#'     plot <-
#'       plot + ggplot2::geom_col(alpha = 0.6)
#'   }
#'   if (stratifyByGender) {
#'     plot <- plot + ggplot2::scale_color_manual(values = colors)
#'     plot <- plot + ggplot2::scale_fill_manual(values = colors)
#'   }
#'   # databaseId field only present when called in Shiny app:
#'   if (!is.null(data$databaseId) && length(data$databaseId) > 1) {
#'     if (yscaleFixed) {
#'       scales <- "fixed"
#'     } else {
#'       scales <- "free_y"
#'     }
#'     if (stratifyByGender | stratifyByCalendarYear) {
#'       if (stratifyByAgeGroup) {
#'         plot <-
#'           plot + facet_nested(databaseName + shortName ~ plotData$ageGroup, scales = scales)
#'       } else {
#'         plot <-
#'           plot + facet_nested(databaseName + shortName ~ ., scales = scales)
#'       }
#'     } else {
#'       plot <-
#'         plot + facet_nested(databaseName + shortName ~ ., scales = scales)
#'     }
#'     # spacing <- rep(c(1, rep(0.5, length(unique(plotData$shortName)) - 1)), length(unique(plotData$databaseId)))[-1]
#'     spacing <- plotData |>
#'       dplyr::distinct(.data$databaseId, .data$shortName) |>
#'       dplyr::arrange(.data$databaseId) |>
#'       dplyr::group_by(.data$databaseId) |>
#'       dplyr::summarise(count = dplyr::n(), .groups = "keep") |>
#'       dplyr::ungroup()
#'     spacing <-
#'       unlist(sapply(spacing$count, function(x) {
#'         c(1, rep(0.5, x - 1))
#'       }))[-1]
#'     
#'     if (length(spacing) > 0) {
#'       plot <-
#'         plot + ggplot2::theme(
#'           panel.spacing.y = ggplot2::unit(spacing, "lines"),
#'           strip.background = ggplot2::element_blank()
#'         )
#'     } else {
#'       plot <-
#'         plot + ggplot2::theme(strip.background = ggplot2::element_blank())
#'     }
#'   } else {
#'     if (stratifyByAgeGroup) {
#'       plot <- plot + ggplot2::facet_grid( ~ ageGroup)
#'     }
#'   }
#'   height <-
#'     1.5 + 1 * nrow(dplyr::distinct(plotData, .data$databaseId, .data$shortName))
#'   plot <- ggiraph::girafe(
#'     ggobj = plot,
#'     options = list(ggiraph::opts_sizing(width = .7),
#'                    ggiraph::opts_zoom(max = 5)),
#'     width_svg = 15,
#'     height_svg = height
#'   )
#'   return(plot)
#' }
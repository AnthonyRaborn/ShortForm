#' ACO Plot Function
#'
#' @param results The output from `antcolony.lavaan`.
#' @param verbose Logical. If true, prints the warnings and errors from `ggplot2`
#' regarding the plots being created. Defaults to `FALSE`.
#' @param ... Other arguments to be passed to the `ggplot2` functions. Currently
#' unimplemented.
#'
#' @return A list of three plots: 
#' \describe{
#' \item{1}{A pheromone plot, showing how the pheromone levels for each item change 
#' for each run of the algorithm;}
#' \item{2}{A mean gamma plot, showing a loess smoothing plot of changes in the mean
#' gamma (regression coefficients) of the best-fit model for each run of the 
#' algorithm;}
#' \item{3}{A mean variance explained plot, showing a loess smoothing plot of 
#' changes in the mean variance explained of the best-fit model for each run of the 
#' algorithm}
#' }
#' 
#' @export
#' 
#' @examples \dontrun{
#' # Below is a modified version of the example in the `antcolony.lavaan` function.
#' # The latent variables are allowed to be correlated for identification
#' results = antcolony.lavaan(data = lavaan::HolzingerSwineford1939,
#' ants = 5, evaporation = 0.7, 
#' antModel = ' visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 
#'              visual ~ textual + speed
#'              textual ~ speed', 
#' list.items = list(c('x1', 'x2', 'x3'), 
#'                   c('x4', 'x5', 'x6'), 
#'                   c('x7', 'x8', 'x9')), 
#'                   full = 9, i.per.f = c(2,2,2), 
#'                   factors = c('visual','textual','speed'), 
#'                   steps = 10, fit.indices = c('cfi'), 
#'                   fit.statistics.test = "(cfi > 0.6)", 
#'                   summaryfile = NULL, feedbackfile = NULL, max.run = 100)
#' 
#' # once converged, put the entire results object into the antcolony_plot function:
#' plots <- antcolony_plot(results)
#' 
#' # print the plot for changes in mean gamma
#' plots[[2]]
#'
#'}
antcolony_plot <- function(results, verbose = FALSE, ...) {
  summary_results <- results[[2]]
  item_pheromone_names <-
    grep("Pheromone", names(summary_results), value = TRUE)
  
  pheromone_long <-
    tidyr::gather(
      data = summary_results,
      key = "Item",
      value = "Pheromone",
      item_pheromone_names
    )
  
  pheromone_plot <- ggplot2::ggplot(
    pheromone_long,
    ggplot2::aes_string(
      x = "run",
      y = "Pheromone",
      group = "Item",
      fill = "Item",
      colour = "Item"
    )
  ) +
    ggplot2::geom_area(linetype = 1,
                       size = .1,
                       color = "black", na.rm = T) +
    ggplot2::ylab("Total Pheromone") +
    ggplot2::ggtitle("Changes in Pheromone") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        size = 30,
        face = "bold",
        hjust = .5
      )
    )
  
  gamma_plot <-
    ggplot2::ggplot(summary_results,
                    ggplot2::aes_string(x = "run", y = "mean.gamma")) +
    ggplot2::geom_smooth(fullrange = TRUE, se = FALSE, na.rm = T) +
    ggplot2::ylab(expression("Mean " * gamma)) +
    ggplot2::ggtitle(expression("Smoothed Changes in Mean " * gamma)) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(
      summary_results$run %in% c(1, max(summary_results$run)), 
      round(summary_results$mean.gamma, 3), ""
    )), vjust = 0, na.rm = T) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        size = 23,
        face = "bold",
        hjust = .5
      )
    )
  
  variance_plot <-
    ggplot2::ggplot(summary_results,
                    ggplot2::aes_string(x = "run", y = "mean.var.exp")) +
    ggplot2::geom_smooth(fullrange = TRUE, se = FALSE, na.rm = T) +
    ggplot2::ylab(expression("Mean Variance Explained")) +
    ggplot2::ggtitle(expression("Smoothed Changes in Mean Variance Explained")) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(
      summary_results$run %in% c(1, max(summary_results$run)), 
      round(summary_results$mean.var.exp, 3), ""
    )), vjust = 0, na.rm = T) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        hjust = .5
      )
    )
  
  plots <- list(pheromone_plot, gamma_plot, variance_plot)
  if (verbose) {
    return(plots)
  } else {
  return(suppressMessages(suppressWarnings(print(plots))))
  }
}
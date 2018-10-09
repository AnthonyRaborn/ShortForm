#' @export
#' 

plot.antcolony <- function(results, verbose = FALSE, type = "all") {
  summary_results <- results[[2]]
  pheromone_plot = gamma_plot = variance_plot = NULL
  item_pheromone_names <-
    grep("Pheromone", names(summary_results), value = TRUE)
  
  pheromone_long <-
    tidyr::gather(
      data = summary_results,
      key = "Item",
      value = "Pheromone",
      item_pheromone_names
    )
  if (type %in% c("all", "pheromone")) {
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
  }
  
  if (type %in% c("all", "gamma")) {
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
  }
  
  if (type %in% c("all", "variance")) {
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
  }
  
  plots <- list("Pheromone" = pheromone_plot, "Gamma" = gamma_plot, "Variance" = variance_plot)
  if (verbose) {
    return(plots)
  } else {
  return(suppressMessages(suppressWarnings(print(plots))))
  }
}

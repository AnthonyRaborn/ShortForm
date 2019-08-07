#' Plot Functions for ShortForm Objects
#' 
#' These are the plot functions for the results objects created by the 
#' \pkg{ShortForm} package (objects of class \code{antcolony}, \code{tabu},
#' and \code{simulatedAnnealing}).
#' 
#' Objects of classes \code{tabu} and \code{simulatedAnnealing} produce a 
#' single plot which show the changes in the objective function across each 
#' iteration of the algorithm. Objects of class \code{antcolony} can produce
#' up to four plots which show the changes in the pheromone levels for each
#' item, changes in the average standardized regression coefficients of the
#' model (gammas and betas), and changes in the amount of variance explained 
#' in the model across each iteration of the algorithm.
#' 
#' These functions do not currently allow users to modify the resulting
#' plots directly, but the objects produces are \pkg{ggplot2} objects which
#' should allow for additional user customization.
#' 
#' @param x An object with one of the following classes: \code{antcolony},
#' \code{tabu}, or \code{simulatedAnnealing}.
#' @param type A character string. One of "all", "pheromone", "gamma", "beta", or
#' "variance". Matched literally. Only used with objects of class \code{antcolony}.
#' @param ... Not used with the current S3 method implementation.
#' @name plot
#' @export
#' @method plot antcolony

plot.antcolony <- function(x, type = "all", ...) {
  summary_results <- x[[2]]
  pheromone_plot = gamma_plot = beta_plot = variance_plot = NULL
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
  
  if (type %in% c("all", "beta")) {
    beta_plot <-
      ggplot2::ggplot(summary_results,
                      ggplot2::aes_string(x = "run", y = "mean.beta")) +
      ggplot2::geom_smooth(fullrange = TRUE, se = FALSE, na.rm = T) +
      ggplot2::ylab(expression("Mean " * beta)) +
      ggplot2::ggtitle(expression("Smoothed Changes in Mean " * beta)) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(
        summary_results$run %in% c(1, max(summary_results$run)), 
        round(summary_results$mean.beta, 3), ""
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
  
  plots <- list("Pheromone" = pheromone_plot, "Gamma" = gamma_plot, 
                "Beta" = beta_plot, "Variance" = variance_plot)
  return(plots)
  
}

#' @rdname plot
#' @export
#' @method plot tabu
plot.tabu <- function(x, ...) {
  val = x$all.obj
  
  val = as.data.frame.matrix(
    cbind(paste0("Run ", 0:(length(val)-1)), 
          as.numeric(val)),
    stringsAsFactors = F)
  val$V2 <- as.numeric(val$V2)
  val$V1 <- factor(val$V1, levels = val$V1)
  colnames(val) = c("Run", "Fit")
  if (nrow(val) < 30) {
    xlabels <- c(T,F)
  } else {
    xlabels <- c(T, F, F, F)
  }
  
  plot <-  
    ggplot2::ggplot(val, ggplot2::aes_string(x = "Run", y = "Fit")) + 
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_point() + 
    ggplot2::ylab(expression("Criterion Function Value")) +
    ggplot2::ggtitle(expression("Changes in Criterion Function Value per Iteration")) +
    ggplot2::scale_x_discrete(breaks=levels(val$Run)[xlabels]) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        hjust = .5
      )
    )
  
  return(plot)
}

#' @rdname plot
#' @export
#' @method plot simulatedAnnealing
plot.simulatedAnnealing <- function(x, ...) {
  val <- data.frame("Iteration" = 1:length(x$allFit), 
                    "Fit" = as.numeric(x$allFit))
  
  
  plot <-  
    ggplot2::ggplot(val, ggplot2::aes_string(x = "Iteration", y = "Fit")) + 
    ggplot2::geom_smooth(method = 'loess', se = F) +
    ggplot2::ylab(paste0("Model Fit Value: ", names(x$bestFit))) +
    ggplot2::ggtitle(expression("Changes in Model Fit Value per Iteration")) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        hjust = .5
      )
    )
  
  return(plot)
}

#' @export
#' 

plot.simulatedAnnealing <- function(results) {
  val <- data.frame("Iteration" = 1:length(results$allFit), 
                    "Fit" = as.numeric(results$allFit))
  

  plot <-  
    ggplot2::ggplot(val, ggplot2::aes_string(x = "Iteration", y = "Fit")) + 
    ggplot2::geom_smooth(method = 'loess', se = F) +
    ggplot2::ylab(paste0("Model Fit Value: ", names(results$bestFit))) +
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

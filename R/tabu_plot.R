#' @export
#' 

plot.tabu <- function(results) {
  val = results$all.obj
  
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
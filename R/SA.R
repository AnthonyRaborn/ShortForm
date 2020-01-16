setClassUnion("matrixORlist", c("matrix", "list"))


#' An S4 class for the Simulated Annealing Algorithm
#'
#' @slot function_call The original function call.
#' @slot chains The number of chains used.
#' @slot chain_results A `matrix` (for multiple chains) or a `list` (for a single chain) of the chain results.
#' @slot all_fit A summary `vector` indicating the model fit results for
#' each iteration.
#' @slot best_fit The best model fit result using the selected `fitStatistic`. 
#' @slot best_model A `lavaan` object of the final solution.
#' @slot best_syntax A `character` vector of the final solution model syntax.
#' @slot runtime A `difftime` object of the total run time of the function.
#'
#' @importFrom methods new show
#'
#' @return An S4 object of class `SA`.
#' @export
#'
setClass('SA',
         slots =
           list(
             function_call = 'call',
             chains = 'numeric',
             chain_results = 'matrixORlist',
             all_fit = 'vector',
             best_fit = 'numeric',
             best_model = 'lavaan',
             best_syntax = 'character',
             runtime = 'ANY'
           )
)

#' Print method for class `SA`
#' 
#' @param object An S4 object of class `SA`.
#' 
#' @export
setMethod('show',
          signature = 'SA',
          definition = function(object) {
            line0 = c("Algorithm: Simulated Annealing")
            line1 = paste0(
              "Total Run Time: ",
              round(object@runtime[[1]], 3), 
              " ", 
              attr(object@runtime, "units"),
              " using ",
              object@chains,
              " chains. \n"
            )
            line2 = suppressWarnings(
              stringr::str_wrap(
                as.vector(c("Function call:\n", object@function_call, "\n")), 
                exdent = 2
              )
            )
            line3 = paste0(
              stringr::str_wrap(
                c("Final Model Syntax:", 
                  unlist(strsplit(object@best_syntax, '\n'))), 
                exdent = 2), 
              collapse = "\n"
            )
            to_console = paste0(c(line0, line1, line2, line3), collapse = "\n")
            cat(to_console)
          }
)

#' Plot method for class `SA`
#' 
#' @param x,y An S4 object of class `SA`.
#' @param ... Not used.
#' 
#' @export
setMethod('plot',
          signature = 'SA',
          definition = function(x, y, ...) {
            val <- data.frame(
              "Iteration" = 1:length(x@all_fit),
              "Fit" = as.numeric(x@all_fit)
            )
            
            
            plot <-
              ggplot2::ggplot(val, ggplot2::aes_string(x = "Iteration", y = "Fit")) +
              ggplot2::geom_line() +
              ggplot2::ylab(paste0("Model Fit Value: ", names(x@best_fit))) +
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
            
            plot
          }
          )

#' Summary method for class `SA`
#' 
#' @param object An S4 object of class `SA`.
#' 
#' @export
setMethod('summary',
          signature = 'SA',
          definition = function(object) {
            line0 = c("Algorithm: Simulated Annealing")
            line1 = paste0(
              "Total Run Time: ",
              round(object@runtime[[1]], 3), 
              " ", 
              attr(object@runtime, "units"),
              "\n"
            )
            line2 = c(capture.output(print(object@best_model)), "\n")
            line3 = paste0(
              stringr::str_wrap(
                c("\nFinal Model Syntax:", 
                  unlist(strsplit(object@best_syntax, "\n"))), 
                exdent = 2), 
              collapse = "\n"
            )
            to_console = paste0(c(line0, line1, line2, line3), collapse = "\n")
            cat(to_console)
          }
)

setClassUnion("charactorORdata.frame", c("character","data.frame"))
#' An S4 class for the Tabu Search Algorithm
#'
#' @slot function_call The original function call.
#' @slot all_fit A summary `vector` indicating the model fit results for
#' each iteration.
#' @slot best_fit The best model fit result using the selected `fitStatistic`. 
#' @slot best_model A `lavaan` object of the final solution.
#' @slot best_syntax A `character` vector of the final solution model syntax.
#' @slot runtime A `difftime` object of the total run time of the function.
#'
#' @importFrom methods new show
#'
#' @return An S4 object of class `TS`.
#' @export
#'
setClass('TS',
         slots =
           list(
             function_call = 'call',
             all_fit = 'vector',
             best_fit = 'numeric',
             best_model = 'lavaan',
             best_syntax = 'charactorORdata.frame',
             runtime = 'ANY'
           )
)

#' Print method for class `TS`
#' 
#' @param object An S4 object of class `TS`.
#' 
#' @export
setMethod('show',
          signature = 'TS',
          definition = function(object) {
            # check if the lavaan_partable was returned (a data.frame)
            if (is.data.frame(object@best_syntax)) {
              # convert to model.syntax for printing
              factors = unique(object@best_syntax$lhs)
              model.syntax = c()
              for (i in factors) {
                factorRows = which(object@best_syntax$lhs==i&
                                     object@best_syntax$op=="=~"&
                                     object@best_syntax$free==1)
                model.syntax = c(model.syntax,
                                 paste0(i, " =~ ", 
                                        paste0(object@best_syntax$rhs[factorRows],
                                               collapse = " + "
                                               )
                                        )
                                 )
              }
              for (i in factors) {
                factorRows = which(object@best_syntax$lhs==i&
                                     object@best_syntax$op=="~~"&
                                     object@best_syntax$free==1)
                if (length(factorRows) > 0) {
                  model.syntax = c(model.syntax,
                                   paste0(i, " ~~ ", 
                                          paste0(object@best_syntax$rhs[factorRows],
                                                 collapse = " + "
                                          )
                                   )
                  )
                }
              }
              model.syntax = paste0(model.syntax, collapse = "\n")
              } else (model.syntax = object@best_syntax)
            line0 = c("Algorithm: Tabu Search")
            line1 = paste0(
              "Total Run Time: ",
              round(object@runtime[[1]], 3), 
              " ", 
              attr(object@runtime, "units"),
              "\n"
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
                  unlist(strsplit(model.syntax, '\n'))), 
                exdent = 2), 
              collapse = "\n"
            )
            to_console = paste0(c(line0, line1, line2, line3), collapse = "\n")
            cat(to_console)
          }
)

#' Plot method for class `TS`
#' 
#' @param x,y An S4 object of class `TS`.
#' @param ... Not used.
#' 
#' @export
setMethod('plot',
          signature = 'TS',
          definition = function(x, y, ...) {
            val <- x@all_fit
            
            val <- as.data.frame.matrix(
              cbind(
                c(0:(length(val) - 1)),
                as.numeric(val)
              ),
              stringsAsFactors = F
            )
            colnames(val) <- c("Iteration", "Fit")

            plot <-
              ggplot2::ggplot(val, ggplot2::aes_string(x = "Iteration", y = "Fit")) +
              ggplot2::geom_line() +
              ggplot2::ylab(paste0("Model Fit Value", names(x@best_fit))) +
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

#' Summary method for class `TS`
#' 
#' @param object An S4 object of class `TS`.
#' 
#' @export
setMethod('summary',
          signature = 'TS',
          definition = function(object) {
            # check if the lavaan_partable was returned (a data.frame)
            if (is.data.frame(object@best_syntax)) {
              # convert to model.syntax for printing
              factors = unique(object@best_syntax$lhs)
              model.syntax = c()
              for (i in factors) {
                factorRows = which(object@best_syntax$lhs==i&
                                     object@best_syntax$op=="=~"&
                                     object@best_syntax$free==1)
                model.syntax = c(model.syntax,
                                 paste0(i, " =~ ", 
                                        paste0(object@best_syntax$rhs[factorRows],
                                               collapse = " + "
                                        )
                                 )
                )
              }
              for (i in factors) {
                factorRows = which(object@best_syntax$lhs==i&
                                     object@best_syntax$op=="~~"&
                                     object@best_syntax$free==1)
                if (length(factorRows) > 0) {
                  model.syntax = c(model.syntax,
                                   paste0(i, " ~~ ", 
                                          paste0(object@best_syntax$rhs[factorRows],
                                                 collapse = " + "
                                          )
                                   )
                  )
                }
              }
              model.syntax = paste0(model.syntax, collapse = "\n")
            } else (model.syntax = object@best_syntax)
            
            line0 = c("Algorithm: Tabu Search")
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
                  unlist(strsplit(model.syntax, "\n"))), 
                exdent = 2), 
              collapse = "\n"
            )
            to_console = paste0(c(line0, line1, line2, line3), collapse = "\n")
            cat(to_console)
          }
)

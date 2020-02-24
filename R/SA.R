setClassUnion("matrixORlist", c("matrix", "list"))
setClassUnion("lavaanORNULL", c("lavaan", "NULL"))

#' An S4 class for the modelCheck object
#'
#' @slot model.output A `lavaan` object.
#' @slot warnings A `character` vector of any warnings.
#' @slot errors A `character` vector of any errors.
#' @slot model.syntax A `character` vector of the modelCheck model syntax.
#'
#' @importFrom methods new show
#'
#' @return An S4 object of class `ACO`.
#' @export
#'
setClass('modelCheck',
         slots =
           list(
             model.output = 'lavaanORNULL',
             warnings = 'character',
             errors = 'character',
             model.syntax = 'character'
           )
)

#' An S4 class for the Simulated Annealing Algorithm
#'
#' @slot function_call The original function call.
#' @slot chains The number of chains used.
#' @slot chain_results A `matrix` (for multiple chains) or a `list` (for a single chain) of the chain results.
#' @slot all_fit A summary `vector` indicating the model fit results for
#' each iteration.
#' @slot best_fit The best model fit result using the selected `fitStatistic`. 
#' @slot best_model A `modelCheck` object of the final solution.
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
             best_model = 'modelCheck',
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
#' @importFrom graphics legend lines par
setMethod('plot',
          signature = 'SA',
          definition = function(x, y, ...) {

            temp <- as.data.frame(x@all_fit)
            
            availableColors <-
              c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#D03AF5", "#EEC21F", "gray62")
            
            par(oma = c(0,0,0,5))
            plot(
              temp[,1], 
              col = availableColors[1],
              type = 'l', 
              ylim = c(min(temp, na.rm = T), max(temp, na.rm = T)),
              bty = "L",
              main = "Model Fit Results per Chain",
              ylab = "Fit Statistic",
              xlab = "Chain Step"
            )
            
            if (2 <= ncol(temp) & ncol(temp) <=8 ) {
              
              for (i in 2:ncol(temp)) {
                lines(temp[,i], col = availableColors[i])
              }
              legend(
                par()$usr[2], par()$usr[4],
                legend = paste0("Chain ", 1:ncol(temp)), 
                col = availableColors[1:ncol(temp)],
                lty = 1,
                bty = 'n',
                xpd = NA)
              }
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
            line2 = c(capture.output(print(object@best_model@model.output)), "\n")
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

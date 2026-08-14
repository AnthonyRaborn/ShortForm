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
            lineCriterion = saCriterionLine(object)
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
            to_console = paste0(c(line0, line1, line2, line3, lineCriterion), collapse = "\n")
            cat(to_console)
          }
)

# builds the "Criterion: ... \nFinal Model Value: ..." line shared by SA's
# show() and summary(), reading fitStatistic/maximize from the captured
# function call and the resulting value from best_fit
saCriterionLine <- function(object) {
  fitStatistic <- extractCallArg(object@function_call, "fitStatistic")
  maximize <- extractCallArg(object@function_call, "maximize")
  paste0(
    "\nCriterion: ", fitStatistic,
    " (", if (isTRUE(maximize)) "maximized" else "minimized", ")",
    "\nFinal Model Value: ", paste(round(object@best_fit, 3), collapse = ", "),
    "\n"
  )
}

#' Plot method for class `SA`
#'
#' @description Plots the model fit results from the simulated annealing
#' algorithm. Up to 8 chains are plotted. Any infinite value in the fit
#' history (e.g. from a chain's initial model failing to fit) is coerced to
#' `NA`, excluding it from the plot rather than letting it collapse the
#' fit-value axis range.
#'
#' @param x,y An S4 object of class `SA`. `y` is included for method
#'  compatibility and is not used.
#' @param burn_in The number of fit results, starting with the first, to
#'  discard before plotting -- e.g. to exclude an unstable early period of
#'  the chain(s), as is common practice for Monte Carlo-style methods. Must
#'  be a non-negative integer less than the number of recorded steps.
#'  Default is `0` (no burn-in).
#' @param ... Not used.
#'
#' @export
#' @importFrom graphics axis legend lines par
setMethod('plot',
          signature = 'SA',
          definition = function(x, y, burn_in = 0, ...) {

            temp <- as.data.frame(x@all_fit)

            if (!is.numeric(burn_in) || length(burn_in) != 1 ||
                burn_in < 0 || burn_in >= nrow(temp)) {
              warning(
                "The burn_in parameter was set incorrectly. ",
                "It must be a single non-negative integer less than the ",
                "number of recorded steps. Defaulting to burn_in = 0."
              )
              burn_in <- 0
            }

            chainStep <- seq_len(nrow(temp))
            if (burn_in > 0) {
              temp <- temp[-seq_len(burn_in), , drop = FALSE]
              chainStep <- chainStep[-seq_len(burn_in)]
            }

            temp[temp == Inf | temp == -Inf] <- NA

            availableColors <-
              c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#D03AF5", "#EEC21F", "gray62")

            par(oma = c(0,0,0,5))
            plot(
              chainStep,
              temp[,1],
              col = availableColors[1],
              type = 'l',
              ylim = c(min(temp, na.rm = T), max(temp, na.rm = T)),
              bty = "L",
              main = "Model Fit Results per Chain",
              ylab = "Fit Statistic",
              xlab = "Chain Step",
              xaxt = 'n'
            )
            # force whole-number Chain Step labels -- with few plotted steps
            # (e.g. after a large burn_in) the default axis can otherwise
            # propose fractional tick marks
            xTicks <- pretty(chainStep)
            xTicks <- xTicks[xTicks == round(xTicks)]
            axis(1, at = xTicks)

            if (2 <= ncol(temp) & ncol(temp) <=8 ) {

              for (i in 2:ncol(temp)) {
                lines(chainStep, temp[,i], col = availableColors[i])
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
            lineCriterion = saCriterionLine(object)
            line2 = c(capture.output(print(object@best_model@model.output)), "\n")
            line3 = paste0(
              stringr::str_wrap(
                c("\nFinal Model Syntax:",
                  unlist(strsplit(object@best_syntax, "\n"))),
                exdent = 2),
              collapse = "\n"
            )
            to_console = paste0(c(line0, line1, line2, line3, lineCriterion), collapse = "\n")
            cat(to_console)
          }
)

setClassUnion("charactorORdata.frame", c("character","data.frame"))

#' An S4 class for the Tabu Search Algorithm
#'
#' @slot function_call The original function call, with every argument
#' resolved (whether the caller supplied it explicitly or it fell back to
#' its default) -- see `resolvedCall()`. In particular, `negateCriterion`
#' (which controls whether the search looks for the largest or smallest
#' value of the criterion/objective function, and is used by the `plot`
#' method to label which direction is better) is read from here rather than
#' from a dedicated slot.
#' @slot all_fit A summary `vector` indicating the model fit results for
#' each iteration.
#' @slot best_fit The best model fit result using the selected `fitStatistic`. A numeric value or vector, possibly named.
#' @slot best_model A `lavaan` object of the final solution.
#' @slot best_syntax A `character` vector of the final solution model syntax.
#' @slot runtime A `difftime` object of the total run time of the function.
#' @slot final_tabu_list The final list of Tabu models. Each element of the list is a `lavaan` object.
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
             runtime = 'ANY',
             final_tabu_list = 'list'
           )
)

# converts a Tabu search parameter table (best_syntax, when returned as a
# data.frame by tabu.sem()) into lavaan model syntax text, keeping only the
# currently-active (free) loadings and covariances. Shared by show() and
# summary() so the two never drift.
binvecToSyntax <- function(best_syntax) {
  if (!is.data.frame(best_syntax)) {
    return(best_syntax)
  }

  factors <- unique(best_syntax$lhs)
  model.syntax <- c()
  for (i in factors) {
    factorRows <- which(best_syntax$lhs == i &
                          best_syntax$op == "=~" &
                          best_syntax$free == 1)
    model.syntax <- c(model.syntax,
                      paste0(i, " =~ ",
                             paste0(best_syntax$rhs[factorRows],
                                    collapse = " + "
                                    )
                             )
                      )
  }
  for (i in factors) {
    factorRows <- which(best_syntax$lhs == i &
                          best_syntax$op == "~~" &
                          best_syntax$free == 1)
    if (length(factorRows) > 0) {
      model.syntax <- c(model.syntax,
                        paste0(i, " ~~ ",
                               paste0(best_syntax$rhs[factorRows],
                                      collapse = " + "
                               )
                        )
      )
    }
  }
  paste0(model.syntax, collapse = "\n")
}

#' Print method for class `TS`
#'
#' @param object An S4 object of class `TS`.
#'
#' @export
setMethod('show',
          signature = 'TS',
          definition = function(object) {
            model.syntax <- binvecToSyntax(object@best_syntax)
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
#' @description Plots the objective function's value across iterations.
#'
#' @param x,y An S4 object of class `TS`. `y` is included for method
#'  compatibility and is not used.
#' @param ... Not used.
#'
#' @export
setMethod("plot",
          signature = "TS",
          definition = function(x, y, ...) {

            fit_values <- as.numeric(x@all_fit)
            iterations <- seq_along(fit_values) - 1

            if (length(fit_values) == 0 || all(is.na(fit_values))) {
              plot.new()
              title(main = "Changes in Model Fit Value per Iteration")
              text(
                x = 0.5,
                y = 0.5,
                labels = "No fit values available",
                cex = 1
              )
              box()

              return(invisible(x))
            }

            negateCriterion <- extractCallArg(x@function_call, "negateCriterion")
            axisNote <- if (isTRUE(negateCriterion)) {
              " (higher = better fit)"
            } else {
              " (lower = better fit)"
            }

            plot(
              x = iterations,
              y = fit_values,
              type = "l",
              xlab = "Iteration",
              ylab = paste0("Model Fit Value(s): ", names(x@best_fit), axisNote),
              main = "Changes in Model Fit Value per Iteration",
              ...
            )

            box()

            invisible(x)
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
            model.syntax <- binvecToSyntax(object@best_syntax)

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

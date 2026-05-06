#' An S4 class for the Ant Colony Optimization Algorithm
#'
#' @slot function_call The original function call.
#' @slot summary A summary `data.frame` indicating the algorithm results for
#' each iteration.
#' @slot final_solution A `matrix` with the final solution information, including
#' fit indices, selected items, and pheromone level.
#' @slot best_model A `lavaan` object of the final solution.
#' @slot best_syntax A `character` vector of the final solution model syntax.
#' @slot runtime A `difftime` object of the total run time of the function.
#'
#' @importFrom methods new show
#'
#' @return An S4 object of class `ACO`.
#' @export
#'
setClass('ACO',
         slots =
           list(
             function_call = 'call',
             summary = 'data.frame',
             final_solution = 'matrix',
             best_model = 'lavaan',
             best_syntax = 'character',
             runtime = 'ANY'
           )
)

#' Print method for class `ACO`
#'
#' @param object An S4 object of class `ACO`
#'
#' @export
setMethod('show',
          signature = 'ACO',
          definition = function(object) {
            line0 = c("Algorithm: Ant Colony Optimization")
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
                  unlist(strsplit(object@best_syntax, '\n'))),
                exdent = 2),
              collapse = "\n"
              )
            to_console = paste0(c(line0, line1, line2, line3), collapse = "\n")
            cat(to_console)
          }
          )

#' Plot method for class `ACO`
#'
#' @param x,y An S4 object of class `ACO`
#' @param type A `character` value specifying the plot type. One of `'all'`
#' (for all plots), `'pheromone'`, `'gamma'`, `'beta'`, or `'variance'`.
#' @param ... Not used.
#' 
#' @importFrom grDevices hcl.colors
#' @importFrom graphics box polygon text plot.new title
#' @importFrom rlang .data
#' 
#' @export
setMethod("plot",
          signature = "ACO",
          definition = function(x, y, type = c("all", "pheromone", "gamma", "beta", "variance"), ...) {

            plot_empty_panel <- function(main_title, message) {
              plot.new()
              title(main = main_title)
              text(
                x = 0.5,
                y = 0.5,
                labels = message,
                cex = 1
              )
              box()
            }

            type <- match.arg(type)

            summary_results <- x@summary

            if (!"run" %in% names(summary_results)) {
              stop("x@summary must contain a 'run' column.")
            }

            run <- summary_results$run

            item_pheromone_names <- grep("Pheromone", names(summary_results), value = TRUE)

            old_par <- par(no.readonly = TRUE)
            on.exit(par(old_par), add = TRUE)

            plot_pheromone <- function() {

              if (length(item_pheromone_names) == 0) {
                stop("No columns containing 'Pheromone' were found in x@summary.")
              }

              pheromone_mat <- as.matrix(summary_results[, item_pheromone_names, drop = FALSE])
              pheromone_mat[is.na(pheromone_mat)] <- 0

              cumulative_pheromone <- t(apply(pheromone_mat, 1, cumsum))

              y_max <- max(cumulative_pheromone, na.rm = TRUE)

              plot(
                run,
                cumulative_pheromone[, ncol(cumulative_pheromone)],
                type = "n",
                ylim = c(0, y_max),
                xlab = "Run",
                ylab = "Total Pheromone",
                main = "Changes in Pheromone",
                ...
              )

              cols <- hcl.colors(ncol(pheromone_mat), palette = "Set 3")

              lower <- rep(0, length(run))

              for (i in seq_len(ncol(pheromone_mat))) {
                upper <- cumulative_pheromone[, i]

                polygon(
                  x = c(run, rev(run)),
                  y = c(upper, rev(lower)),
                  col = cols[i],
                  border = "black",
                  lwd = 0.1
                )

                lower <- upper
              }

              box()
            }

            plot_metric <- function(column, ylab, main_title) {
                if (!column %in% names(summary_results)) {
                  plot_empty_panel(
                    main_title = main_title,
                    message = paste0("Column '", column, "' not found")
                  )
                  return(invisible(FALSE))
                }

                y_values <- summary_results[[column]]

                if (length(y_values) == 0 || all(is.na(y_values))) {
                  plot_empty_panel(
                    main_title = main_title,
                    message = paste0("Column '", column, "' is empty")
                  )
                  return(invisible(FALSE))
                }

                plot(
                  run,
                  y_values,
                  type = "l",
                  xlab = "Run",
                  ylab = ylab,
                  main = main_title,
                  ...
                )

                endpoint_index <- which(
                  run %in% c(min(run, na.rm = TRUE), max(run, na.rm = TRUE)) &
                    !is.na(y_values)
                )

                if (length(endpoint_index) > 0) {
                  text(
                    x = run[endpoint_index],
                    y = y_values[endpoint_index],
                    labels = round(y_values[endpoint_index], 3),
                    pos = c(4, 2)[seq_along(endpoint_index)],
                    cex = 0.8
                  )
                }

                box()

                invisible(TRUE)
              }

            if (type == "all") {
              par(mfrow = c(2, 2))

              plot_pheromone()

              plot_metric(
                column = "mean.gamma",
                ylab = expression("Mean " * gamma),
                main_title = expression("Changes in Mean " * gamma)
              )

              plot_metric(
                column = "mean.beta",
                ylab = expression("Mean " * beta),
                main_title = expression("Changes in Mean " * beta)
              )

              plot_metric(
                column = "mean.var.exp",
                ylab = "Mean Variance Explained",
                main_title = "Changes in Mean Variance Explained"
              )

            } else if (type == "pheromone") {

              plot_pheromone()

            } else if (type == "gamma") {

              plot_metric(
                column = "mean.gamma",
                ylab = expression("Mean " * gamma),
                main_title = expression("Changes in Mean " * gamma)
              )

            } else if (type == "beta") {

              plot_metric(
                column = "mean.beta",
                ylab = expression("Mean " * beta),
                main_title = expression("Changes in Mean " * beta)
              )

            } else if (type == "variance") {

              plot_metric(
                column = "mean.var.exp",
                ylab = "Mean Variance Explained",
                main_title = "Changes in Mean Variance Explained"
              )
            }

            invisible(x)
          }
        )
#' Summary method for class `ACO`
#'
#' @param object An S4 object of class `ACO`
#'
#' @export
setMethod('summary',
          signature = 'ACO',
          definition = function(object) {
            line0 = c("Algorithm: Ant Colony Optimization")
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

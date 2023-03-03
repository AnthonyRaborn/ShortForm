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
#' @importFrom ggrepel geom_text_repel
#' @importFrom tidyr gather
#' 
#' @export
setMethod('plot',
          signature = 'ACO',
          definition = function(x, y, type = 'all', ...) {
            summary_results = x@summary
            pheromone_plot <- gamma_plot <- beta_plot <- variance_plot <- NULL
            mean.beta <- mean.gamma <- mean.var.exp <- run <- NULL
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
              pheromone_plot <-
                ggplot2::ggplot(
                pheromone_long,
                ggplot2::aes_string(
                  x = "run",
                  y = "Pheromone",
                  group = "Item",
                  fill = "Item",
                  colour = "Item"
                )
              ) +
                ggplot2::geom_area(
                  linetype = 1,
                  size = .1,
                  color = "black", na.rm = T
                ) +
                ggplot2::ylab("Total Pheromone") +
                ggplot2::xlab("Run") +
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
                ggplot2::ggplot(
                  summary_results,
                  ggplot2::aes_string(x = "run", y = "mean.gamma")
                ) +
                ggplot2::geom_line() +
                ggplot2::ylab(expression("Mean " * gamma)) +
                ggplot2::xlab("Run") +
                ggplot2::ggtitle(expression("Changes in Mean " * gamma)) +
                ggrepel::geom_text_repel(ggplot2::aes(label = ifelse(
                  run %in% c(1, max(run)),
                  round(mean.gamma, 3), ""
                )), na.rm = T) +
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
                ggplot2::ggplot(
                  summary_results,
                  ggplot2::aes_string(x = "run", y = "mean.beta")
                ) +
                ggplot2::geom_line() +
                ggplot2::ylab(expression("Mean " * beta)) +
                ggplot2::xlab("Run") +
                ggplot2::ggtitle(expression("Changes in Mean " * beta)) +
                ggrepel::geom_text_repel(ggplot2::aes(label = ifelse(
                  run %in% c(1, max(run)),
                  round(mean.beta, 3), ""
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
                ggplot2::ggplot(
                  summary_results,
                  ggplot2::aes_string(x = "run", y = "mean.var.exp")
                ) +
                ggplot2::geom_line() +
                ggplot2::ylab(expression("Mean Variance Explained")) +
                ggplot2::xlab("Run") +
                ggplot2::ggtitle(expression("Changes in Mean Variance Explained")) +
                ggrepel::geom_text_repel(ggplot2::aes(label = ifelse(
                  run %in% c(1, max(run)),
                  round(mean.var.exp, 3), ""
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

            if (type == "all") {
              plots <-
                list(
                  "Pheromone" = pheromone_plot, "Gamma" = gamma_plot,
                  "Beta" = beta_plot, "Variance" = variance_plot
                  )
            } else if (type == "pheromone") {
              plots <-
                pheromone_plot
            } else if (type == "gamma") {
              plots <-
                gamma_plot
            } else if (type == "beta") {
              plots <-
                beta_plot
            } else {
              plots <-
                variance_plot
            }

            plots
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

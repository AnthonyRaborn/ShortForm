#' \code{ShortForm} package
#'
#' Automated Item Selection Algorithms for Short Forms
#'
#' See the README on
#' \href{https://cran.r-project.org/package=ShortForm/README.html}{CRAN}
#' or \href{https://github.com/AnthonyRaborn/ShortForm#readme}{GitHub}
#'
#' @docType package
#' @name ShortForm
#' @importFrom stats runif
NULL

## quiets concerns of R CMD check about internal variables
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("allItems", "auto.cov.lv.x", "auto.cov.y", "auto.delta", "auto.fix.first",
                           "auto.fix.single", "auto.th", "auto.var", "bestModel", "currentModel", "estimator",
                           "factors", "int.lv.free", "int.ov.free", "itemsPerFactor", "model.type", "numItems",
                           "std.lv"
                           ))
}

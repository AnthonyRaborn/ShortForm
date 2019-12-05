#' \code{ShortForm} package
#'
#' Automated Item Selection Algorithms for Short Forms
#'
#' See the README on \href{https://github.com/AnthonyRaborn/ShortForm#readme}{GitHub} for more information.
#'
#' @docType package
#' @name ShortForm
#' @importFrom stats runif
NULL

## quiets concerns of R CMD check about internal variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "allItems", "auto.cov.lv.x", "auto.cov.y", "auto.delta", "auto.fix.first",
    "auto.fix.single", "auto.th", "auto.var", "bestModel", "currentModel", "estimator",
    "factors", "int.lv.free", "int.ov.free", "itemsPerFactor", "model.type", "numItems",
    "std.lv"
  ))
}

#' Create Package Startup Message
#'
#' Makes package startup message.
#'
#' Idea taken from https://github.com/ntguardian/MCHT/blob/master/R/StartupMessage.R
#'
#' @import utils
#' @examples
#' ShortForm:::ShortFormStartup()
ShortFormStartup <- function() {
  ShortForm <- c("  #####                             #######                      \n #     # #    #  ####  #####  ##### #        ####  #####  #    # \n #       #    # #    # #    #   #   #       #    # #    # ##  ## \n  #####  ###### #    # #    #   #   #####   #    # #    # # ## # \n       # #    # #    # #####    #   #       #    # #####  #    # \n #     # #    # #    # #   #    #   #       #    # #   #  #    # \n  #####  #    #  ####  #    #   #   #        ####  #    # #    # \n ")
  version <- paste("\t\t Version", as.character(utils::packageVersion("ShortForm")))
  penguin <- c("\t\t\t (o<", "\t\t\t //\\", "\t\t\t V_/_ ")

  message <- c(ShortForm, version, penguin)

  cat(message, sep = "\n")
}

#' Package Attach Hook Function
#'
#' Hook triggered when package attached.
#'
#' Idea taken from https://github.com/ntguardian/MCHT/blob/master/R/StartupMessage.R
#'
#' @param lib a character string giving the library directory where the package
#'            defining the namespace was found
#' @param pkg a character string giving the name of the package
#' @examples
#' ShortForm:::.onAttach(.libPaths()[1], "ShortForm")
.onAttach <- function(lib, pkg) {
  msg <- ShortFormStartup()
  if (!interactive()) {
    msg[1] <- paste("Package 'ShortForm' version", packageVersion("ShortForm"))
  }
  packageStartupMessage(msg)
  invisible()
}

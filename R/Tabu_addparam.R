#' Adds a parameter to the given search table. Checks whether parameter
#' is involved in any (in)equality constraints in a fitted lavaan model
#'
#' @param fitted.model fitted lavaan model
#' @param ptab search table
#' @param syntax model.syntax specifying the parameter to add to the current table
#' @param nullval optional numeric value specifying what the parameter should be fixed to (when fixed)
#' @param free optional logical value specifying whether the parameter should initially be set free (or not)
#' @param block optional numeric value specifying the group number to which the parameter corresponds
#'
#' @return A \code{data.frame} with lavaan-formatted parameter values.
#' @family Tabu Search
#' @import lavaan
#' @export
#'
#' @examples
#' \dontrun{
#' # load simulation data and select columns used in this example
#' data(simulated_test_data)
#' tabuData <- simulated_test_data[, c(1:10)]
#'
#' # specify an improper model (improper because data is unidimensional)
#' tabuModel <- "
#' Ability =~ Item1 + Item2 + Item3 + Item4
#' FakeAbility =~ Item5 + Item6 + Item7 + Item8
#' Ability ~ Outcome
#' FakeAbility ~ 0*Outcome"
#'
#' # run the initial misspecified model for Tabu
#'
#' init.model <- lavaan::lavaan(
#'   model = tabuModel, data = tabuData,
#'   auto.var = TRUE, auto.fix.first = FALSE, std.lv = TRUE, auto.cov.lv.x = TRUE
#' )
#'
#' # Use search.prep to prepare for the Tabu search
#' ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)
#'
#' # add an additional (mispecified) parameter
#' additional.param <- "Item1 ~~ 0.5*Item3"
#' ptab <- add.param(fitted.model = init.model, ptab = ptab, syntax = additional.param)
#'
#' # Perform Tabu Search
#' trial <- tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 2, tabu.size = 5)
#' }
#'
#' @author Carl F. Falk
#' @references \url{https://doi.org/10.1080/10705511.2017.1409074}

add.param <- function(fitted.model, ptab, syntax, nullval = NULL, free = NULL, block = NULL) {
  newpar <- lavaan::lavaanify(syntax)

  if (!is.null(block)) {
    newpar$block <- block
  }
  if (!is.null(free)) {
    newpar$free <- ifelse(free, 1, 0)
  }
  if (!is.null(nullval)) {
    newpar$nullval <- nullval
  } else {
    newpar$nullval <- newpar$ustart
  }

  newpar <- newpar[, c("lhs", "op", "rhs", "block", "free", "label", "nullval")]

  # Check whether newpar includes more than one row
  if (nrow(newpar) > 1) {
    newpar <- newpar[1, ]
    warning("lavaanify() results in more than one row of parameters, adding only the first row")
  }

  # Check whether parameter already exists in table
  matches <- par.matches(ptab, paste(newpar$lhs, newpar$op, newpar$rhs, sep = ""), block = block)
  if (any(matches)) {
    stop("Parameter already found in table")
  }

  # Check to see if parameter is part of constraint in existing model
  flag <- check.const(fitted.model, newpar)
  if (flag) {
    warning("Some parameters added to this table may be involved in (in)equality constraints")
  }

  # Add newpar to existing table
  ptab <- rbind(ptab, newpar)

  return(ptab)
}

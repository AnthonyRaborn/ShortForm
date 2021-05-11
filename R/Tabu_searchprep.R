#' Given a fitted lavaan model (e.g., CFA), prepares a table that contains
#' parameters that can be fixed/freed as part of a model specification search.
#'
#' @param fitted.model - an object of class "lavaan" that contains the initially fitted model for the search
#' @param loadings - a logical value that indicates whether cross-loadings will be part of the search
#' @param fcov - a logical value indicating whether factor covariances will be part of the search
#' @param errors - a logical value indicating whether error covariances will be part of the search
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
#' @author Carl F. Falk
#' @references \doi{10.1080/10705511.2017.1409074}

search.prep <- function(fitted.model, loadings = TRUE, fcov = TRUE, errors = FALSE) {

  # Parameter table from fitted model
  ptab <- lavaan::parTable(fitted.model)

  # Full parameter table
  fulltab <- lav_partable_full(ptab)

  # Merge together so we have what's free
  mergetab <- lavaan::lav_partable_merge(ptab, fulltab, remove.duplicated = TRUE, warn = FALSE)

  idx <- vector("numeric")
  null.val <- vector("numeric")

  lv.names <- vnames(ptab, type = "lv") # extract names of latent vars
  ov.names <- vnames(ptab, type = "ov.nox") # indicators

  # Loadings
  if (loadings) {
    # Index for which rows of table are loadings
    indx <- mergetab$op == "=~"

    # Omit any loadings that are fixed to a non-zero value
    # These may be fixed for identification
    indx[mergetab$free[indx] == 0 & mergetab$ustart[indx] != 0] <- FALSE

    idx <- c(idx, which(indx))
    null.val <- c(null.val, rep(0, sum(indx)))
  }

  # Factor cov
  if (fcov) {
    # Index for which rows are cov among latent vars
    indx <- mergetab$op == "~~" & mergetab$lhs %in% lv.names & mergetab$lhs != mergetab$rhs
    idx <- c(idx, which(indx))
    null.val <- c(null.val, rep(0, sum(indx)))
  }

  # Error covariances among indicators
  if (errors) {
    # Index for which rows are cov among indicators
    indx <- mergetab$op == "~~" & mergetab$lhs %in% ov.names & mergetab$lhs != mergetab$rhs
    idx <- c(idx, which(indx))
    null.val <- c(null.val, rep(0, sum(indx)))
  }

  # Check whether any parameters in the table are involved in any constraints
  flag <- check.const(fitted.model, mergetab[idx, ])

  if (flag) {
    warning("Some parameters in this table may be involved in (in)equality constraints")
  }

  # Clean up
  mergetab <- mergetab[idx, c("lhs", "op", "rhs", "block", "free", "label")]
  mergetab$nullval <- null.val
  mergetab$free <- ifelse(mergetab$free == 0, 0, 1)

  # Sort table
  lvnames <- lavaan::lavNames(mergetab, "lv")
  ovnames <- lavaan::lavNames(mergetab, "ov")
  mergetab <- mergetab[order(
    mergetab$block, # by group (or block)
    is.na(match(mergetab$lhs, c(lvnames))), # put latent vars first
    mergetab$op, # by op w/in lv and ov sections
    match(mergetab$lhs, c(lvnames, ovnames)), # then by lv and ov names on lhs
    match(mergetab$rhs, c(lvnames, ovnames))
  ), ] # then by same on rhs
  rownames(mergetab) <- 1:nrow(mergetab)

  ret <- mergetab

  return(ret)
}

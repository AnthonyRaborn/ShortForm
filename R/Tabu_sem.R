#' Given a fitted lavaan model, a search table, and an objective criterion,
#' performs a Tabu model specification search. Currently only supports
#' neighbors that are 1 move away from the current model.
#'
#' @param init.model initial fitted model of class lavaan
#' @param ptab search table (e.g., created by search.prep) that lists candidate
#'  parameters that can be modified as part of the search and how the parameters
#'  can be modified (fixed to what values)
#' @param criterion The objective to be minimized (or maximized, if
#'  `negateCriterion = TRUE`). Either a `character` fit-measure name
#'  recognized by \link[lavaan]{fitmeasures} (e.g. `"cfi"`), or a function
#'  that takes a lavaan object as its sole argument and returns a numeric
#'  value.
#' @param niter number of Tabu iterations to perform
#' @param tabu.size size of Tabu list
#' @param negateCriterion Logical. Should the search look for the smallest
#'  value of `criterion` (`FALSE`, e.g. AIC, where smaller is better), or the
#'  largest (`TRUE`, e.g. cfi, where larger is better)? Default is `FALSE`.
#'
#' @return An S4 object of class `TS`, with (among other slots) `best_fit`
#'  holding the best (minimal, or maximal if `negateCriterion = TRUE`)
#'  objective function value achieved, `best_model` the corresponding final
#'  lavaan model, and `best_syntax` a data.frame of the lavaan-formatted
#'  parameter table for the final model.
#' @export
#'
#' @examples
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
#' # Perform Tabu Search
#' trial <- tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC, niter = 2, tabu.size = 5)
#' @author Carl F. Falk
#' @references \doi{10.1080/10705511.2017.1409074}

tabu.sem <- function(init.model,
                     ptab,
                     criterion,
                     niter = 30,
                     tabu.size = 5,
                     negateCriterion = FALSE) {
  start.time = Sys.time()
  # picks the better of two objective values, and the better of a set of
  # candidate values, according to the search direction negateCriterion asks
  # for -- the largest value (maximizing) if TRUE, the smallest (minimizing,
  # the default) if FALSE
  isBetter <- if (negateCriterion) `>` else `<`
  bestIndex <- if (negateCriterion) which.max else which.min
  criterionFn <- resolveCriterion(criterion, negateCriterion)

  # Initialize objective function and best model
  best.obj <- all.obj <- current.obj <- criterionFn(init.model)
  best.mod <- current.mod <- init.model
  best.binvec <- current.binvec <- ptab

  tabu.list <- vector("numeric")

  # Do iterations
  for (it in 1:niter) {
    cat(paste0("\rRunning iteration ", it, " of ", niter, ".   "))
    # Loop through all neighbors
    tmp.obj <- vector("numeric")
    tmp.mod <- list()
    tmp.vec <- list()
    for (j in 1:nrow(current.binvec)) {
      tmp.binvec <- current.binvec
      bin <- 1 - tmp.binvec$free[j]
      tmp.binvec$free[j] <- bin
      fitmodel <- refit.model(init.model, tmp.binvec)

      if (!inherits(fitmodel, "try-error") &&
          fitmodel@Fit@converged && !any(is.na(fitmodel@Fit@se))) {
        fit.val <- criterionFn(fitmodel)
      } else {
        fit.val <- NA
      }

      tmp.obj <- c(tmp.obj, fit.val)
      tmp.mod[[j]] <- fitmodel
      tmp.vec[[j]] <- tmp.binvec
    }

    # Check which indices result in a valid objective function
    valid <- which(!is.na(tmp.obj))

    # Get just models not on Tabu list
    valid <- valid[!(valid %in% tabu.list)]

    if (length(valid) == 0) {
      # no candidate neighbor is both valid (converged, non-error) and
      # outside the tabu list this iteration; keep the current state and
      # try again next iteration rather than crashing
      all.obj <- c(all.obj, current.obj)
      next
    }

    # Out of valid models, pick model with best objective function value
    indx <- bestIndex(tmp.obj[valid])

    # Move current state to next model
    current.obj <- (tmp.obj[valid])[indx]
    all.obj <- c(all.obj, current.obj)
    current.mod <- (tmp.mod[valid])[[indx]]
    current.binvec <- (tmp.vec[valid])[[indx]]

    # Update Tabu list
    tabu.list <- c(valid[indx], tabu.list)
    if (length(tabu.list) > tabu.size) {
      tabu.list <- tabu.list[1:tabu.size]
    }

    # Update if the current model is better than the best model
    if (isBetter(current.obj, best.obj)) {
      best.obj <- current.obj
      best.mod <- current.mod
      best.binvec <- current.binvec
      tabu.list <- vector("numeric") # Clear Tabu list
    }
  }

  capturedCall <- resolvedCall(match.call(), formals())

  ret <-
    new("TS",
      function_call = capturedCall,
      all_fit = all.obj,
      best_fit = best.obj,
      best_model = best.mod,
      best_syntax = best.binvec,
      runtime = Sys.time() - start.time,
      final_tabu_list = list(tabu.list)
  )

  ret
}

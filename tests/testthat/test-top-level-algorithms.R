# end-to-end smoke tests for the three exported, top-level algorithm functions.

# simulatedAnnealing() -- short form mode ####
test_that(
  "simulatedAnnealing runs end-to-end and returns an SA object when creating a short form", {
    set.seed(1)
    defaultModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    result <- suppressWarnings(
      simulatedAnnealing(
        initialModel = defaultModel,
        originalData = lavaan::HolzingerSwineford1939,
        maxIterations = 3,
        criterion = "cfi",
        negateCriterion = TRUE,
        itemsPerFactor = c(2, 2, 2),
        items = paste0("x", 1:9)
      )
    )

    expect_s4_class(result, "SA")
    expect_s4_class(result@best_model, "modelCheck")
    expect_true(is.numeric(result@best_fit))
    expect_type(result@best_syntax, "character")
  }
)

# simulatedAnnealing() -- mergeModelSpecs (partial override + typo detection) ####
test_that(
  "simulatedAnnealing accepts a partial lavaan.model.specs, filling the rest from its defaults", {
    set.seed(1)
    defaultModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    result <- suppressWarnings(
      simulatedAnnealing(
        initialModel = defaultModel,
        originalData = lavaan::HolzingerSwineford1939,
        maxIterations = 3,
        criterion = "cfi",
        negateCriterion = TRUE,
        itemsPerFactor = c(2, 2, 2),
        items = paste0("x", 1:9),
        maxChanges = 1,
        lavaan.model.specs = list(estimator = "ML")
      )
    )

    expect_s4_class(result, "SA")
  }
)

test_that(
  "simulatedAnnealing errors clearly when lavaan.model.specs has an unrecognized (likely misspelled) name", {
    set.seed(1)
    defaultModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    expect_error(
      simulatedAnnealing(
        initialModel = defaultModel,
        originalData = lavaan::HolzingerSwineford1939,
        maxIterations = 3,
        criterion = "cfi",
        negateCriterion = TRUE,
        itemsPerFactor = c(2, 2, 2),
        items = paste0("x", 1:9),
        maxChanges = 1,
        lavaan.model.specs = list(estmator = "ML")
      ),
      "not recognized"
    )
  }
)

# simulatedAnnealing() -- full model (non-short-form) mode ####
# FIXED (see code review): when itemsPerFactor was NULL, simulatedAnnealing()
# never initialized bestModel/currentModel before using them, so the "full
# model" usage documented in ?simulatedAnnealing's own first example crashed
# immediately. bestModel/currentModel are now initialized from initialModel
# via parTableToSyntax() (R/lavaan_syntax_helpers.R), and shortForm is now
# derived from itemsPerFactor instead of trusted as an independent argument,
# so the loop correctly dispatches to randomNeighborFull() instead of
# randomNeighborShort().
test_that(
  "simulatedAnnealing runs end-to-end and returns an SA object for the documented full-model (non-short-form) usage", {
    set.seed(1)
    fittedModel <- lavaan::cfa(
      model =
        ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9',
      data = lavaan::HolzingerSwineford1939
    )

    result <- suppressWarnings(
      simulatedAnnealing(
        initialModel = fittedModel,
        originalData = lavaan::HolzingerSwineford1939,
        maxIterations = 3,
        criterion = "cfi",
        negateCriterion = FALSE
      )
    )

    expect_s4_class(result, "SA")
    expect_s4_class(result@best_model, "modelCheck")
    expect_true(is.numeric(result@best_fit))
  }
)

# tabuSearch() ####
test_that(
  "tabuSearch runs end-to-end and returns a TS object", {
    set.seed(1)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "
    data(simulated_test_data)

    result <- tabuSearch(
      initialModel = shortAntModel,
      originalData = simulated_test_data,
      itemsPerFactor = 7,
      maxIterations = 1,
      tabu.size = 3,
      parallel = FALSE
    )

    expect_s4_class(result, "TS")
    expect_true(is.numeric(result@best_fit))
  }
)

# tabu.sem() -- successful search ####
test_that(
  "tabu.sem runs end-to-end and returns a TS object with an improved fit", {
    set.seed(1)
    holzingerModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    init.model <- lavaan::lavaan(
      model = holzingerModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC, niter = 2, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_true(is.numeric(result@best_fit))
    expect_lte(result@best_fit, AIC(init.model))
  }
)

# tabu.sem() -- no neighbor ever improves on the initial model ####
# FIXED (see code review): best.mod was previously only assigned inside the
# `if (current.obj < best.obj)` branch, but was returned unconditionally, so
# tabu.sem() crashed whenever no iteration ever improved on the starting
# model. best.mod/current.mod are now initialized from init.model up front
# (matching the fix already applied to best.obj/current.obj), so a search
# that never finds an improvement now correctly returns the initial model
# as the "best" one found instead of crashing.
test_that(
  "tabu.sem returns the initial model as best when no candidate ever improves on it", {
    set.seed(1)
    holzingerModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    init.model <- lavaan::lavaan(
      model = holzingerModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = function(x) Inf, niter = 1, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_identical(result@best_model, init.model)
    expect_equal(result@best_fit, Inf)
  }
)

# tabu.sem() -- a candidate neighbor fails to refit entirely ####
# FIXED (see code review): refit.model() wraps the refit in try(silent =
# TRUE) and can return a "try-error" object rather than a lavaan object when
# a candidate parameter change makes the model unfittable. tabu.sem()
# previously accessed fitmodel@Fit@converged unconditionally, crashing with
# "no applicable method for `@`" the moment this happened. It now checks
# inherits(fitmodel, "try-error") first and treats that candidate as
# non-viable (NA), same as a non-converged fit.
test_that(
  "tabu.sem does not crash when a candidate neighbor fails to refit", {
    set.seed(1)
    data(simulated_test_data)
    tabuData <- simulated_test_data[, c(1:10)]
    tabuModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4
    FakeAbility =~ Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    FakeAbility ~ 0*Outcome"

    init.model <- lavaan::lavaan(
      model = tabuModel, data = tabuData,
      auto.var = TRUE, auto.fix.first = FALSE, std.lv = TRUE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC, niter = 2, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_true(is.numeric(result@best_fit))
  }
)

# tabu.sem() -- every candidate is either invalid or tabu ####
# FIXED (see code review): if every candidate neighbor either fails to
# converge/refit or is already on the tabu list, `valid` became empty and
# which.min(tmp.obj[valid])/tmp.mod[valid][[indx]] threw "subscript out of
# bounds". tabu.sem() now detects an empty valid set and carries the current
# state forward to the next iteration instead of crashing.
test_that(
  "tabu.sem does not crash when every candidate is invalid or tabu in an iteration", {
    set.seed(1)
    # a single-factor, 3-item model: perturbing any one candidate parameter
    # (e.g. freeing the fixed marker loading) leaves the model unidentified,
    # so every candidate fails to converge on the very first iteration --
    # deterministically reproducing an empty valid set
    singleFactorModel <- "f =~ x1 + x2 + x3"

    init.model <- lavaan::lavaan(
      model = singleFactorModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = function(x) Inf, niter = 2, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_identical(result@best_model, init.model)
  }
)

# tabu.sem() -- criterion returns NA for the initial model ####
# An NA candidate is now always treated as worse than the current best, and 
# an NA best is always displaced by any valid candidate.
test_that(
  "tabu.sem does not crash when criterion returns NA for the initial model", {
    set.seed(1)
    holzingerModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    init.model <- lavaan::lavaan(
      model = holzingerModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    # returns NA on its very first call (evaluating init.model), then a
    # deterministic valid value for every subsequent call (evaluating
    # candidate neighbors), reproducing an NA best.obj at the start of search
    callCount <- 0
    naFirstCallCriterion <- function(m) {
      callCount <<- callCount + 1
      if (callCount == 1) {
        return(NA_real_)
      }
      AIC(m)
    }

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = naFirstCallCriterion, niter = 2, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_true(is.numeric(result@best_fit))
    expect_false(is.na(result@best_fit))
  }
)

# tabu.sem() -- criterion intermittently returns NA for candidate neighbors ####
# regression test for the same NA-safety fix: with an objective that
# deterministically returns NA for roughly half of all candidates (odd vs.
# even parameter count), the search must still complete without error and
# must never adopt an NA value as its best fit.
test_that(
  "tabu.sem does not crash when criterion returns NA for some candidate neighbors", {
    set.seed(1)
    holzingerModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9'

    init.model <- lavaan::lavaan(
      model = holzingerModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    # deterministic: NA whenever the refit model has an even number of free
    # parameters, valid (npar) otherwise -- since each candidate flips
    # exactly one parameter's free/fixed status, this alternates by candidate
    naParityCriterion <- function(m) {
      npar <- length(lavaan::coef(m))
      if (npar %% 2 == 0) {
        return(NA_real_)
      }
      npar
    }

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = naParityCriterion, niter = 3, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_true(is.numeric(result@best_fit))
    expect_false(is.na(result@best_fit))
    expect_false(any(is.na(result@all_fit)))
  }
)

# antColony() ####
test_that(
  "antColony runs end-to-end and returns an ACO object", {
    set.seed(1)
    result <- antColony(
      data = lavaan::HolzingerSwineford1939,
      ants = 2, evaporation = 0.7,
      initialModel = " visual  =~ x1 + x2 + x3
                   textual =~ x4 + x5 + x6
                   speed   =~ x7 + x8 + x9 ",
      itemsPerFactor = c(3, 3, 3),
      steps = 2, fit.indices = c("cfi"), fit.statistics.test = "(cfi > 0.6)",
      maxIterations = 2, parallel = FALSE, verbose = FALSE
    )

    expect_s4_class(result, "ACO")
  }
)

# antColony() -- partial lavaan.model.specs override ####
test_that(
  "antColony runs end-to-end with a partial lavaan.model.specs override", {
    set.seed(1)
    result <- antColony(
      data = lavaan::HolzingerSwineford1939,
      ants = 2, evaporation = 0.7,
      initialModel = " visual  =~ x1 + x2 + x3
                   textual =~ x4 + x5 + x6
                   speed   =~ x7 + x8 + x9 ",
      itemsPerFactor = c(3, 3, 3),
      steps = 2, fit.indices = c("cfi"), fit.statistics.test = "(cfi > 0.6)",
      lavaan.model.specs = list(estimator = "ML"),
      maxIterations = 2, parallel = FALSE, verbose = FALSE
    )

    expect_s4_class(result, "ACO")
  }
)

# antColony() -- items defaults to colnames(data) when using sample.cov requires items ####
test_that(
  "antColony errors clearly when items is NULL and data is also NULL (sample.cov path)", {
    set.seed(1)
    holzingerCov <- stats::cov(lavaan::HolzingerSwineford1939[, paste0("x", 1:9)])

    expect_error(
      antColony(
        sample.cov = holzingerCov, sample.nobs = nrow(lavaan::HolzingerSwineford1939),
        ants = 2, evaporation = 0.7,
        initialModel = " visual  =~ x1 + x2 + x3
                     textual =~ x4 + x5 + x6
                     speed   =~ x7 + x8 + x9 ",
        itemsPerFactor = c(3, 3, 3),
        steps = 2, fit.indices = c("cfi"), fit.statistics.test = "(cfi > 0.6)",
        maxIterations = 2, parallel = FALSE, verbose = FALSE
      ),
      "items"
    )
  }
)

# tabuSearch() -- mergeModelSpecs (partial override + typo detection) ####
test_that(
  "tabuSearch accepts a partial lavaan.model.specs, filling the rest from its defaults", {
    set.seed(1)
    data(simulated_test_data)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "

    result <- tabuSearch(
      initialModel = shortAntModel,
      originalData = simulated_test_data,
      itemsPerFactor = 7,
      maxIterations = 1,
      tabu.size = 3,
      parallel = FALSE,
      lavaan.model.specs = list(estimator = "ML")
    )

    expect_s4_class(result, "TS")
  }
)

test_that(
  "tabuSearch errors clearly when lavaan.model.specs has an unrecognized (likely misspelled) name", {
    set.seed(1)
    data(simulated_test_data)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "

    expect_error(
      tabuSearch(
        initialModel = shortAntModel,
        originalData = simulated_test_data,
        itemsPerFactor = 7,
        maxIterations = 1,
        tabu.size = 3,
        parallel = FALSE,
        lavaan.model.specs = list(estmator = "ML")
      ),
      "not recognized"
    )
  }
)

# tabuSearch() -- parallel cluster setup under CRAN-check core limits ####
# FIXED (see comparison against the abandoned refactorSA/refactorTS branches):
# when parallel = TRUE and _R_CHECK_LIMIT_CORES_ is set (as it is on CRAN's
# check machines), the cluster (`cl`) and `%dopar%` were only ever created in
# the *other* branch of the nested core-count check, so `%dopar%` was left
# unbound -- "could not find function '%dopar%'" -- the moment this ran under
# CRAN-like conditions, even though it worked fine locally. ACO and SA do not
# have this bug (their cluster setup is unconditional within the `parallel`
# branch); Tabu's has been aligned to match.
test_that(
  "tabuSearch runs under simulated CRAN core-limit conditions", {
    previousLimitCores <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
    Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
    on.exit(
      if (is.na(previousLimitCores)) {
        Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
      } else {
        Sys.setenv("_R_CHECK_LIMIT_CORES_" = previousLimitCores)
      }
    )
    set.seed(1)
    data(simulated_test_data)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "

    result <- tabuSearch(
      initialModel = shortAntModel,
      originalData = simulated_test_data,
      itemsPerFactor = 7,
      maxIterations = 1,
      tabu.size = 3
    )

    expect_s4_class(result, "TS")
  }
)

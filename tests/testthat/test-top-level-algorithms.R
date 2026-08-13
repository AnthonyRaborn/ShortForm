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
        maxSteps = 3,
        fitStatistic = "cfi",
        maximize = TRUE,
        maxItems = c(2, 2, 2),
        items = paste0("x", 1:9)
      )
    )

    expect_s4_class(result, "SA")
    expect_s4_class(result@best_model, "modelCheck")
    expect_true(is.numeric(result@best_fit))
    expect_type(result@best_syntax, "character")
  }
)

# simulatedAnnealing() -- full model (non-short-form) mode ####
# FIXED (see code review): when maxItems was NULL, simulatedAnnealing() never
# initialized bestModel/currentModel before using them, so the "full model"
# usage documented in ?simulatedAnnealing's own first example crashed
# immediately. bestModel/currentModel are now initialized from initialModel
# via parTableToSyntax() (R/lavaan_syntax_helpers.R), and shortForm is now
# derived from maxItems instead of trusted as an independent argument, so the
# loop correctly dispatches to randomNeighborFull() instead of
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
        maxSteps = 3,
        fitStatistic = "cfi",
        maximize = FALSE
      )
    )

    expect_s4_class(result, "SA")
    expect_s4_class(result@best_model, "modelCheck")
    expect_true(is.numeric(result@best_fit))
  }
)

# tabuShortForm() ####
test_that(
  "tabuShortForm runs end-to-end and returns a TS object", {
    set.seed(1)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "
    data(simulated_test_data)

    result <- tabuShortForm(
      initialModel = shortAntModel,
      originalData = simulated_test_data,
      numItems = 7,
      niter = 1,
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
      tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 2, tabu.size = 5)
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
      tabu.sem(init.model = init.model, ptab = ptab, obj = function(x) Inf, niter = 1, tabu.size = 5)
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
      tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 2, tabu.size = 5)
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
      tabu.sem(init.model = init.model, ptab = ptab, obj = function(x) Inf, niter = 2, tabu.size = 5)
    )

    expect_s4_class(result, "TS")
    expect_identical(result@best_model, init.model)
  }
)

# antcolony.lavaan() ####
test_that(
  "antcolony.lavaan runs end-to-end and returns an ACO object", {
    set.seed(1)
    result <- antcolony.lavaan(
      data = lavaan::HolzingerSwineford1939,
      ants = 2, evaporation = 0.7,
      antModel = " visual  =~ x1 + x2 + x3
                   textual =~ x4 + x5 + x6
                   speed   =~ x7 + x8 + x9 ",
      list.items = list(c("x1", "x2", "x3"), c("x4", "x5", "x6"), c("x7", "x8", "x9")),
      full = 9, i.per.f = c(3, 3, 3), factors = c("visual", "textual", "speed"),
      steps = 2, fit.indices = c("cfi"), fit.statistics.test = "(cfi > 0.6)",
      summaryfile = NULL, feedbackfile = NULL, max.run = 2, parallel = FALSE
    )

    expect_s4_class(result, "ACO")
  }
)

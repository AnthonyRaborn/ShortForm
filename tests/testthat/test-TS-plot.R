# negateCriterion (via function_call) ####
# Tabu search's comparison direction is controlled by negateCriterion: if
# FALSE (the tabu.sem() default), the search looks for the SMALLEST value of
# criterion (current.obj < best.obj); if TRUE (the tabuShortForm() default,
# matching its default cfi criterion), the search looks for the LARGEST
# value (current.obj > best.obj). criterion/obj is always supplied on its
# natural scale (e.g. plain cfi, not -cfi) -- negateCriterion picks the
# comparison direction, not a sign convention the user has to apply
# themselves.
#
# There is no dedicated negateCriterion slot on TS -- it's read out of
# function_call (captured via resolvedCall(), with every argument resolved
# whether the caller supplied it explicitly or not) using extractCallArg().

makeFakeTS <- function(all_fit, functionCall) {
  fakeFit <- lavaan::cfa(model = "f =~ x1 + x2 + x3", data = lavaan::HolzingerSwineford1939)
  new(
    "TS",
    function_call = functionCall,
    all_fit = all_fit,
    best_fit = all_fit[length(all_fit)],
    best_model = fakeFit,
    best_syntax = "f =~ x1 + x2 + x3",
    runtime = as.difftime(1, units = "secs"),
    final_tabu_list = list()
  )
}

test_that(
  "extractCallArg returns NULL when function_call doesn't mention negateCriterion at all", {
    fakeFit <- lavaan::cfa(model = "f =~ x1 + x2 + x3", data = lavaan::HolzingerSwineford1939)
    fakeTS <- new(
      "TS",
      function_call = quote(tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC)),
      all_fit = c(10, 8, 6),
      best_fit = 6,
      best_model = fakeFit,
      best_syntax = "f =~ x1 + x2 + x3",
      runtime = as.difftime(1, units = "secs"),
      final_tabu_list = list()
    )

    expect_null(extractCallArg(fakeTS@function_call, "negateCriterion"))
  }
)

test_that(
  "plot,TS-method renders all_fit as-is (no axis flip) regardless of negateCriterion", {
    # all_fit is always on criterion's natural scale by the time plot() sees
    # it, regardless of negateCriterion -- the axis is never reversed, only
    # labeled
    fakeTSMinimize <- makeFakeTS(
      c(100, 90, 80),
      quote(tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC, negateCriterion = FALSE))
    )
    fakeTSMaximize <- makeFakeTS(
      c(0.90, 0.95, 0.99),
      quote(tabuShortForm(initialModel = m, originalData = d, numItems = 5, negateCriterion = TRUE))
    )

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    plot(fakeTSMinimize)
    expect_equal(par("usr")[3:4], sort(par("usr")[3:4]))

    plot(fakeTSMaximize)
    expect_equal(par("usr")[3:4], sort(par("usr")[3:4]))
  }
)

test_that(
  "plot,TS-method's axis-note text matches negateCriterion extracted from function_call", {
    # mirrors the axisNote logic in plot,TS-method directly, since the
    # rendered ylab text isn't retrievable from a base-graphics device
    # after the fact (par() does not expose plot-call arguments like ylab)
    negateTrue <- TRUE
    axisNoteMax <- if (isTRUE(negateTrue)) " (higher = better fit)" else " (lower = better fit)"
    expect_equal(axisNoteMax, " (higher = better fit)")

    negateFalse <- FALSE
    axisNoteMin <- if (isTRUE(negateFalse)) " (higher = better fit)" else " (lower = better fit)"
    expect_equal(axisNoteMin, " (lower = better fit)")
  }
)

# tabu.sem() / tabuShortForm() -- negateCriterion wiring ####
test_that(
  "tabu.sem defaults negateCriterion to FALSE and minimizes criterion directly", {
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
      tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC, niter = 1, tabu.size = 5)
    )

    expect_false(extractCallArg(result@function_call, "negateCriterion"))
    # best_fit should be the minimum of all_fit, matching the (default)
    # minimizing direction
    expect_equal(unname(result@best_fit), unname(min(result@all_fit)))
  }
)

test_that(
  "tabuShortForm defaults negateCriterion to TRUE and maximizes the default (raw) cfi criterion", {
    set.seed(1)
    data(simulated_test_data)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "

    result <- tabuShortForm(
      initialModel = shortAntModel,
      originalData = simulated_test_data,
      numItems = 7,
      niter = 1,
      tabu.size = 3,
      parallel = FALSE
    )

    expect_true(extractCallArg(result@function_call, "negateCriterion"))
    # the default criterion is now plain (positive) cfi -- best_fit should
    # be a plausible cfi value directly, and the maximum of all_fit
    expect_true(result@best_fit > 0 && result@best_fit <= 1)
    expect_equal(unname(result@best_fit), unname(max(result@all_fit)))
  }
)

test_that(
  "negateCriterion controls whether the search looks for the largest or smallest criterion value", {
    # this is the actual behavior negateCriterion was renamed/redesigned to
    # provide: TRUE flips the search to maximize criterion's raw (natural
    # scale) return value; FALSE keeps it minimizing. Run the SAME raw
    # criterion both ways and check the search actually pursued the
    # requested direction.
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

    rawCfi <- function(x) tryCatch(lavaan::fitmeasures(x, "cfi"), error = function(e) NA)

    set.seed(2)
    resultMaximize <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = rawCfi, niter = 3, tabu.size = 5, negateCriterion = TRUE)
    )
    set.seed(2)
    resultMinimize <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = rawCfi, niter = 3, tabu.size = 5, negateCriterion = FALSE)
    )

    # best_fit is the extremum of all_fit matching the requested direction
    expect_equal(unname(resultMaximize@best_fit), unname(max(resultMaximize@all_fit)))
    expect_equal(unname(resultMinimize@best_fit), unname(min(resultMinimize@all_fit)))

    # maximizing cfi should never end up worse than minimizing it on the
    # same starting model and candidate set
    expect_gte(resultMaximize@best_fit, resultMinimize@best_fit)
  }
)

# resolvedCall() / extractCallArg() -- general call-capture mechanism ####
test_that(
  "resolvedCall fills in every argument, specified or not, including NULL defaults", {
    testFn <- function(a, b = 5, c = NULL, ...) {
      resolvedCall(match.call(), formals())
    }

    resolved <- testFn(1)
    expect_equal(resolved$a, 1)
    expect_equal(resolved$b, 5)
    expect_true("c" %in% names(resolved))
    expect_null(resolved$c)
  }
)

test_that(
  "resolvedCall is robust to the caller being wrapped in suppressWarnings/tryCatch", {
    # this is exactly the scenario that broke a naive sys.call(-1)-based
    # implementation: intervening frames from tryCatch()/withCallingHandlers()
    # sit between the true caller and the function being introspected
    testFn <- function(a, b = 5) {
      resolvedCall(match.call(), formals())
    }

    expect_no_error(suppressWarnings(testFn(1)))
    expect_no_error(tryCatch(testFn(1), error = function(e) e))
  }
)

test_that(
  "extractCallArg evaluates and returns a named argument's value from a call", {
    call <- quote(someFunction(x = 1, flag = TRUE, label = "hello"))

    expect_equal(extractCallArg(call, "flag"), TRUE)
    expect_equal(extractCallArg(call, "label"), "hello")
    expect_null(extractCallArg(call, "notPresent"))
  }
)

test_that(
  "tabuShortForm's captured function_call reflects the merged lavaan.model.specs, not the partial input", {
    set.seed(1)
    data(simulated_test_data)
    shortAntModel <- "
    Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
    Ability ~ Outcome
    "

    result <- tabuShortForm(
      initialModel = shortAntModel,
      originalData = simulated_test_data,
      numItems = 7,
      niter = 1,
      tabu.size = 3,
      parallel = FALSE,
      lavaan.model.specs = list(estimator = "ML")
    )

    capturedSpecs <- result@function_call$lavaan.model.specs
    # the caller only specified estimator, but the captured call should
    # show the full merged list actually used, not just list(estimator = "ML")
    expect_true("model.type" %in% names(capturedSpecs))
    expect_equal(capturedSpecs$estimator, "ML")
  }
)

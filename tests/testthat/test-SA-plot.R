# plot,SA-method -- burn_in ####
# burn_in lets users discard an early, unstable period of the chain(s) from
# the plot -- common practice for Monte Carlo-style methods, which SA
# resembles. Adapted from an unmerged draft (originally called throw_away)
# found while reviewing the abandoned refactorSA/refactorTS branches.

makeFakeSA <- function(all_fit) {
  fakeModelCheck <- new(
    "modelCheck",
    model.output = NULL,
    warnings = character(0),
    errors = character(0),
    model.syntax = "f =~ x1 + x2"
  )
  new(
    "SA",
    function_call = quote(simulatedAnnealing()),
    chains = length(all_fit),
    chain_results = list(),
    all_fit = all_fit,
    best_fit = 0.95,
    best_model = fakeModelCheck,
    best_syntax = "f =~ x1 + x2",
    runtime = as.difftime(1, units = "secs")
  )
}

test_that(
  "plot,SA-method accepts burn_in = 0 (default) without dropping any steps", {
    fakeSA <- makeFakeSA(list(c(0.5, 0.6, 0.7, 0.8, 0.9)))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    expect_no_warning(plot(fakeSA))
  }
)

test_that(
  "plot,SA-method drops the first burn_in steps", {
    fakeSA <- makeFakeSA(list(c(0.5, 0.6, 0.7, 0.8, 0.9)))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    expect_no_warning(plot(fakeSA, burn_in = 2))
  }
)

test_that(
  "plot,SA-method coerces Inf/-Inf fit values to NA instead of breaking the axis range", {
    fakeSA <- makeFakeSA(list(c(0.5, 0.6, Inf, 0.8, 0.9), c(0.6, 0.65, -Inf, 0.85, 0.95)))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    # would error/produce a degenerate ylim of c(-Inf, Inf) if Inf/-Inf were
    # not coerced to NA before computing the plot range
    expect_no_error(plot(fakeSA))
  }
)

test_that(
  "plot,SA-method's whole-number tick calculation never proposes fractional ticks", {
    # mirrors the tick-selection logic in plot,SA-method: with very few
    # plotted steps (e.g. after a large burn_in), pretty() on its own can
    # propose fractional tick marks (e.g. 4.0, 4.5, 5.0), which the
    # round-value filter should always remove
    chainStep <- 4:5

    xTicks <- pretty(chainStep)
    xTicks <- xTicks[xTicks == round(xTicks)]

    expect_true(length(xTicks) > 0)
    expect_true(all(xTicks == round(xTicks)))
  }
)

test_that(
  "plot,SA-method runs without error on a very small post-burn-in range", {
    # this is the scenario the whole-number tick fix targets: few enough
    # remaining steps that the default axis would otherwise show decimals
    fakeSA <- makeFakeSA(list(c(0.5, 0.6, 0.7, 0.8, 0.9)))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    expect_no_error(plot(fakeSA, burn_in = 3))
  }
)

test_that(
  "plot,SA-method warns and falls back to burn_in = 0 for invalid input", {
    fakeSA <- makeFakeSA(list(c(0.5, 0.6, 0.7, 0.8, 0.9)))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    expect_warning(
      plot(fakeSA, burn_in = -1),
      "burn_in parameter was set incorrectly"
    )

    expect_warning(
      plot(fakeSA, burn_in = 999),
      "burn_in parameter was set incorrectly"
    )

    expect_warning(
      plot(fakeSA, burn_in = c(1, 2)),
      "burn_in parameter was set incorrectly"
    )
  }
)

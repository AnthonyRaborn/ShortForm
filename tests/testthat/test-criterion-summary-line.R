# saCriterionLine() / tsCriterionLine() / acoCriterionLine() ####
# these build the "Criterion: ... \nFinal Model Value: ..." line added to
# show()/summary() for SA, TS, and ACO, reading the criterion/fit-statistic
# selection from the captured function_call and its resulting value(s) from
# the object's fit slot (best_fit for SA/TS, final_solution for ACO)

defaultModel <-
  ' visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9'

test_that(
  "saCriterionLine reports criterion, negateCriterion direction, and best_fit", {
    set.seed(1)
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

    line <- saCriterionLine(result)

    expect_match(line, "Criterion: \"cfi\" \\(maximized\\)")
    expect_match(
      line,
      paste0("Final Model Value: ", round(result@best_fit, 3)),
      fixed = TRUE
    )
  }
)

test_that(
  "saCriterionLine reports \"minimized\" when negateCriterion = FALSE", {
    set.seed(1)
    result <- suppressWarnings(
      simulatedAnnealing(
        initialModel = defaultModel,
        originalData = lavaan::HolzingerSwineford1939,
        maxIterations = 3,
        criterion = "rmsea",
        negateCriterion = FALSE,
        itemsPerFactor = c(2, 2, 2),
        items = paste0("x", 1:9)
      )
    )

    expect_match(saCriterionLine(result), "Criterion: \"rmsea\" \\(minimized\\)")
  }
)

test_that(
  "show/summary for SA include the criterion line", {
    set.seed(1)
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

    showText <- paste(capture.output(show(result)), collapse = "\n")
    summaryText <- paste(capture.output(summary(result)), collapse = "\n")

    expect_match(showText, "Criterion: \"cfi\" \\(maximized\\)")
    expect_match(summaryText, "Criterion: \"cfi\" \\(maximized\\)")
  }
)

test_that(
  "tsCriterionLine reports a named best_fit for tabuSearch's default (fitmeasures-based) criterion", {
    set.seed(1)
    result <- suppressWarnings(
      tabuSearch(
        initialModel = defaultModel,
        originalData = lavaan::HolzingerSwineford1939,
        itemsPerFactor = c(2, 2, 2),
        maxIterations = 2,
        tabu.size = 2,
        parallel = FALSE
      )
    )

    line <- tsCriterionLine(result)

    # tabuSearch's default criterion negates cfi internally (negateCriterion
    # defaults to TRUE); best_fit is named after the fit measure since the
    # underlying lavaan::fitmeasures() call preserves its name
    expectedValue <- if (!is.null(names(result@best_fit))) {
      paste0("cfi = ", round(result@best_fit, 3))
    } else {
      as.character(round(result@best_fit, 3))
    }
    expect_match(line, "Criterion: \"cfi\"")
    expect_match(line, "\\(maximized\\)")
    expect_match(
      line,
      paste0("Final Model Value: ", expectedValue),
      fixed = TRUE
    )
  }
)

test_that(
  "tsCriterionLine deparses a plain function symbol and reports an unnamed value for tabu.sem", {
    set.seed(1)
    singleFactorModel <- "f =~ x1 + x2 + x3"
    init.model <- lavaan::lavaan(
      model = singleFactorModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = AIC, niter = 2, tabu.size = 5)
    )

    line <- tsCriterionLine(result)

    # tabu.sem's default negateCriterion is FALSE (AIC is already minimized directly)
    expect_match(line, "Criterion: AIC \\(minimized\\)")
    expect_match(
      line,
      paste0("Final Model Value: ", round(result@best_fit, 3)),
      fixed = TRUE
    )
  }
)

test_that(
  "tsCriterionLine also works when tabu.sem's criterion is a character fit-measure name", {
    set.seed(1)
    singleFactorModel <- "f =~ x1 + x2 + x3"
    init.model <- lavaan::lavaan(
      model = singleFactorModel, data = lavaan::HolzingerSwineford1939,
      auto.var = TRUE, auto.fix.first = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE
    )
    ptab <- search.prep(fitted.model = init.model, loadings = TRUE, fcov = TRUE, errors = FALSE)

    result <- suppressWarnings(
      tabu.sem(init.model = init.model, ptab = ptab, criterion = "cfi", negateCriterion = TRUE, niter = 2, tabu.size = 5)
    )

    expect_match(tsCriterionLine(result), "Criterion: \"cfi\" \\(maximized\\)")
  }
)

test_that(
  "show/summary for TS include the criterion line", {
    set.seed(1)
    result <- suppressWarnings(
      tabuSearch(
        initialModel = defaultModel,
        originalData = lavaan::HolzingerSwineford1939,
        itemsPerFactor = c(2, 2, 2),
        maxIterations = 2,
        tabu.size = 2,
        parallel = FALSE
      )
    )

    showText <- paste(capture.output(show(result)), collapse = "\n")
    summaryText <- paste(capture.output(summary(result)), collapse = "\n")

    expect_match(showText, "Criterion: \"cfi\"")
    expect_match(summaryText, "Criterion: \"cfi\"")
  }
)

test_that(
  "acoCriterionLine reports fit.indices, fit.statistics.test, and final_solution values", {
    set.seed(1)
    result <- antColony(
      data = lavaan::HolzingerSwineford1939,
      ants = 2, evaporation = 0.7,
      initialModel = defaultModel,
      itemsPerFactor = c(3, 3, 3),
      steps = 2, fit.indices = c("cfi"), fit.statistics.test = "(cfi > 0.6)",
      maxIterations = 2, parallel = FALSE, verbose = FALSE
    )

    line <- acoCriterionLine(result)

    expect_match(line, "Fit Indices: cfi")
    expect_match(line, "Fit Test: \\(cfi > 0\\.6\\)", fixed = FALSE)
    expect_match(
      line,
      paste0("Final Model Values: cfi = ", round(result@final_solution[1, "cfi"], 3)),
      fixed = TRUE
    )
  }
)

test_that(
  "acoCriterionLine handles multiple fit.indices", {
    set.seed(1)
    result <- antColony(
      data = lavaan::HolzingerSwineford1939,
      ants = 2, evaporation = 0.7,
      initialModel = defaultModel,
      itemsPerFactor = c(3, 3, 3),
      steps = 2, fit.indices = c("cfi", "tli"), fit.statistics.test = "(cfi > 0.6)",
      maxIterations = 2, parallel = FALSE, verbose = FALSE
    )

    line <- acoCriterionLine(result)

    expect_match(line, "Fit Indices: cfi, tli")
    expect_match(line, "cfi = ")
    expect_match(line, "tli = ")
  }
)

test_that(
  "show/summary for ACO include the criterion line", {
    set.seed(1)
    result <- antColony(
      data = lavaan::HolzingerSwineford1939,
      ants = 2, evaporation = 0.7,
      initialModel = defaultModel,
      itemsPerFactor = c(3, 3, 3),
      steps = 2, fit.indices = c("cfi"), fit.statistics.test = "(cfi > 0.6)",
      maxIterations = 2, parallel = FALSE, verbose = FALSE
    )

    showText <- paste(capture.output(show(result)), collapse = "\n")
    summaryText <- paste(capture.output(summary(result)), collapse = "\n")

    expect_match(showText, "Fit Indices: cfi")
    expect_match(summaryText, "Fit Indices: cfi")
  }
)

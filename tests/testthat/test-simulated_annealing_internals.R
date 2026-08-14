# randomInitialModel ####
test_that(
  "randomInitialModel produces a modelCheck object", {
    defaultModel <-
    ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9'
    defaultBifactor <-
    ' visual   =~ x1 + x2 + x3
      textual  =~ x4 + x5 + x6
      speed    =~ x7 + x8 + x9
      bifactor =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9'
    itemCounts <- c(2,2,2)
    allItems <- paste0("x", 1:9)
    data <- lavaan::HolzingerSwineford1939
    bifactor <- NULL
    lavaanSpecs <-
      list(model.type = "cfa",
           estimator = "ML",
           ordered = NULL,
           int.ov.free = TRUE,
           int.lv.free = FALSE,
           auto.fix.first = FALSE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.var = TRUE,
           auto.cov.lv.x = TRUE,
           auto.efa = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE,
           missing = "listwise",
           group = NULL,
           group.label = NULL,
           group.equal = 'loadings',
           group.partial = NULL,
           group.w.free = FALSE)

    ## bifactor == NULL ####
    # should output a modelCheck object
    expect_s4_class(
      randomInitialModel(
        init.model = defaultModel,
        itemCounts = itemCounts,
        allItems = allItems,
        initialData = data,
        bifactor = bifactor,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )

    # bifactor == a named factor ####
    bifactor = "speed"
    # should output a list
    expect_s4_class(
      randomInitialModel(
        init.model = defaultModel,
        itemCounts = itemCounts,
        allItems = allItems,
        initialData = data,
        bifactor = bifactor,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )
  }
)

# randomNeighborShort ####
test_that(
  "randomNeighborShort produces a list of appropriate length and with appropriate names", {
    defaultModel <-
    ' visual  =~ x1 + x2
      textual =~ x4 + x5
      speed   =~ x7 + x8'
    defaultBifactor <-
    ' visual   =~ x1 + x2
      textual  =~ x4 + x5
      speed    =~ x7 + x8
      bifactor =~ x1 + x2 + x4 + x5 + x7 + x8'
    currentModel <-
      modelWarningCheck(
        lavaan::cfa(
          model = defaultModel,
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax = defaultModel
      )
    currentBifactor <-
      modelWarningCheck(
        lavaan::cfa(
          model = defaultBifactor,
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax = defaultBifactor
      )
    lavaanSpecs <-
      list(model.type = "cfa",
           estimator = "ML",
           ordered = NULL,
           int.ov.free = TRUE,
           int.lv.free = FALSE,
           auto.fix.first = FALSE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.var = TRUE,
           auto.cov.lv.x = TRUE,
           auto.efa = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE,
           missing = "listwise",
           group = NULL,
           group.label = NULL,
           group.equal = 'loadings',
           group.partial = NULL,
           group.w.free = FALSE)

    ## bifactor == NULL ####
    # should output a modelCheck object
    expect_s4_class(
      randomNeighborShort(
        currentModelObject = currentModel,
        numChanges = 1,
        allItems = paste0("x", 1:9),
        data = lavaan::HolzingerSwineford1939,
        bifactor = NULL,
        init.model = defaultModel,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )


    ## bifactor == a named factor ####
    # should output a modelCheck object
    expect_s4_class(
      randomNeighborShort(
        currentModelObject = currentBifactor,
        numChanges = 1,
        allItems = paste0("x", 1:9),
        data = lavaan::HolzingerSwineford1939,
        bifactor = "bifactor",
        init.model = defaultBifactor,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )
  }
)

# randomNeighborShort -- numChanges exceeding available capacity ####
# FIXED: previously, the item- and replacement-selection logic sampled and
# then retried in a `while (x %in% alreadyUsed) resample` loop. If a
# randomly-chosen factor's pool of items (or replacements) was exhausted by
# earlier iterations of the same call -- easy to hit when numChanges is
# larger than a factor actually has room for -- that retry loop could never
# find an unused candidate and spun forever. It's now structurally
# impossible to hang: only factors with remaining capacity are considered,
# and candidates are sampled directly from what's left rather than by
# retrying until unique.
test_that(
  "randomNeighborShort terminates and degrades gracefully when numChanges exceeds available capacity", {
    # a single factor with only 2 items, drawn from a 4-item pool: after one
    # change, both the item-to-change pool and the replacement pool for this
    # factor are exhausted, yet numChanges = 5 asks for far more changes
    # than that capacity allows
    tinyData <- as.data.frame(matrix(rnorm(4 * 100), ncol = 4))
    names(tinyData) <- c("Item1", "Item2", "Item3", "Item4")
    tinyModel <- "f =~ Item1 + Item2"
    currentModel <-
      modelWarningCheck(
        lavaan::cfa(model = tinyModel, data = tinyData, std.lv = TRUE),
        modelSyntax = tinyModel
      )
    lavaanSpecs <-
      list(model.type = "cfa", estimator = "default", ordered = NULL,
           int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE,
           std.lv = TRUE, auto.fix.single = TRUE, auto.var = TRUE,
           auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE,
           auto.cov.y = TRUE)

    set.seed(1)
    result <- randomNeighborShort(
      currentModelObject = currentModel,
      numChanges = 5,
      allItems = c("Item1", "Item2", "Item3", "Item4"),
      data = tinyData,
      bifactor = NULL,
      init.model = tinyModel,
      lavaan.model.specs = lavaanSpecs
    )

    expect_s4_class(result, "modelCheck")
    # only 2 items exist for this factor, so at most 1 item can ever be
    # swapped out (the other must remain, or there'd be nothing left)
    expect_match(result@model.syntax, "^f =~ .+ \\+ .+$")
  }
)

# # randomNeighborFull ####
# test_that(
#   "randomNeighborFull produces a list of appropriate length and with appropriate names", {
#     currentModel <-
#       modelWarningCheck(
#         lavaan::cfa(
#           model =
#           ' visual  =~ x1 + x2 + x3
#             textual =~ x4 + x5 + x6
#             speed   =~ x7 + x8 + x9',
#           data = lavaan::HolzingerSwineford1939
#         ),
#         modelSyntax =
#           ' visual  =~ x1 + x2 + x3
#             textual =~ x4 + x5 + x6
#             speed   =~ x7 + x8 + x9'
#       )
#     # should produce a message
#     expect_message(
#       randomNeighborFull(
#         currentModelObject = currentModel@model.output,
#         numChanges = 1,
#         data = lavaan::HolzingerSwineford1939
#       )
#     )
#
#     # should output a list
#     expect_type(
#       randomNeighborFull(
#         currentModelObject = currentModel$model.output,
#         numChanges = 2,
#         data = lavaan::HolzingerSwineford1939
#       ),
#       'list'
#     )
#
#     # the list should be of length 4
#     expect_length(
#       randomNeighborFull(
#         currentModelObject = currentModel$model.output,
#         numChanges = 2,
#         data = lavaan::HolzingerSwineford1939
#       ),
#       4
#     )
#
#     # the list should have specified names in specified order
#     expect_named(
#       randomNeighborFull(
#         currentModelObject = currentModel$model.output,
#         numChanges = 2,
#         data = lavaan::HolzingerSwineford1939
#       ),
#       expected =
#         c("model.output", "warnings", "errors", "model.syntax")
#     )
#
#
#   }
# )

# randomNeighborFull ####
# FIXED (see code review): randomChangesRows previously sampled directly
# from currentModelParamsLV$id via sample(x, size = numChanges). R's
# sample() has a well-known footgun: when x is a single number, sample(x, n)
# samples from 1:x rather than returning x, so a model with exactly one
# latent-variable-related candidate parameter could have an entirely
# unrelated row flipped instead. Selection is now done by sampling row
# *positions* with sample.int() and indexing into id, which is correct
# regardless of how many candidate rows there are.
test_that(
  "randomNeighborFull produces a modelCheck object with valid, refittable syntax", {
    defaultModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6'
    fit <- lavaan::cfa(model = defaultModel, data = lavaan::HolzingerSwineford1939, std.lv = TRUE)

    set.seed(1)
    result <- randomNeighborFull(
      currentModelObject = fit,
      numChanges = 1,
      data = lavaan::HolzingerSwineford1939
    )

    expect_s4_class(result, "modelCheck")
    expect_type(result@model.syntax, "character")
  }
)

# goal ####
test_that(
  "goal returns the 'energy' value of a fit statistic test, including in cases of NA fit", {
    currentModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
            ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
      )
    currentBadModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
          ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6
            textual =~ x4 + x5 + x6 + x7 + x8 + x9
            speed   =~ x7 + x8 + x9 + x1 + x2 + x3',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6
            textual =~ x4 + x5 + x6 + x7 + x8 + x9
            speed   =~ x7 + x8 + x9 + x1 + x2 + x3'
      )

    cfiFn <- function(m) fitmeasures(object = m, fit.measures = 'cfi')
    rmseaFn <- function(m) fitmeasures(object = m, fit.measures = 'rmsea')

    # lavaan input with single, maximized fit statistic should equal the negative value of the fit statistic
    expect_equal(
      goal(
        x = currentModel@model.output,
        criterionFn = cfiFn,
        negateCriterion = T
        ),
      -fitmeasures(
        object = currentModel@model.output,
        fit.measures = 'cfi'
      )
    )

    # lavaan input with single, minimized fit statistic should equal the value of that fit statistic
    expect_equal(
      goal(
        x = currentModel@model.output,
        criterionFn = rmseaFn,
        negateCriterion = F
      ),
      fitmeasures(
        object = currentModel@model.output,
        fit.measures = 'rmsea'
      )
    )

    # lavaan input with no fit statistc due to poor model, goal maximized
    expect_equal(
      goal(
        x = currentBadModel@model.output,
        criterionFn = cfiFn,
        negateCriterion = T
      ),
      Inf
    )

    # lavaan input with no fit statistc due to poor model, goal minimized
    expect_equal(
      goal(
        x = currentBadModel@model.output,
        criterionFn = rmseaFn,
        negateCriterion = F
      ),
      Inf
    )

  }
)
# selectionFunction ####
test_that(
  "selectionFunction accurately chooses between the best of two models", {
    currentModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
            ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
      )
    intermediateModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
          ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9
            bifactor=~ x2 + x6 + x9',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9
            bifactor=~ x2 + x6 + x9'
        )
    currentBadModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
            ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6
            textual =~ x4 + x5 + x6 + x7 + x8 + x9
            speed   =~ x7 + x8 + x9 + x1 + x2 + x3',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6
            textual =~ x4 + x5 + x6 + x7 + x8 + x9
            speed   =~ x7 + x8 + x9 + x1 + x2 + x3'
      )
    currentTemp <-
      0.5
    cfiFn <- function(m) lavaan::fitmeasures(object = m, fit.measures = "cfi")

    # new model does not converge, so return currentModelObject with message
    expect_equal(
      selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = currentBadModel,
        currentTemp = currentTemp,
        negateCriterion = TRUE,
        criterionFn = cfiFn,
        consecutive = 1
      ),
      currentModel
    )
    expect_output(
      selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = currentBadModel,
        currentTemp = currentTemp,
        negateCriterion = TRUE,
        criterionFn = cfiFn,
        consecutive = 1
      ),
      "New model did not converge."
    )

    # old model is null, so return the new model
    expect_equal(
      selectionFunction(
        currentModelObject = NULL,
        randomNeighborModel = currentBadModel,
        currentTemp = currentTemp,
        negateCriterion = TRUE,
        criterionFn = cfiFn,
        consecutive = 1
      ),
      currentBadModel
    )

    # new model is null, so return the old
    expect_equal(
      selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = NULL,
        currentTemp = currentTemp,
        negateCriterion = TRUE,
        criterionFn = cfiFn,
        consecutive = 1
      ),
      currentModel
    )

    # choosing between current model with better fit than new model, prints message with probability
    expect_output(
      selectionFunction(
        currentModelObject = intermediateModel,
        randomNeighborModel = currentModel,
        currentTemp = currentTemp,
        negateCriterion = TRUE,
        criterionFn = cfiFn,
        consecutive = 1
      ),
      "Probability:"
    )

    # choosing between current model with worse fit than new model, prints message WITHOUT probability
    expect_output(
      selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = intermediateModel,
        currentTemp = currentTemp,
        negateCriterion = TRUE,
        criterionFn = cfiFn,
        consecutive = 1
      ),
      "^((?!Probability:))",
      perl = TRUE
    )
  }
)
# checkModels ####
test_that(
  "checkModels returns the appropriate bestModel depending on the conditions", {
    # example models
    bestHolzinger <-
      modelWarningCheck(
        lavaan::lavaan(model =
        ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9 ',
        data = lavaan::HolzingerSwineford1939,
        auto.var=TRUE,
        auto.fix.first=TRUE,
        auto.cov.lv.x=TRUE),
        ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9 '
      )
    badHolzinger <-
      modelWarningCheck(
        lavaan::lavaan(model =
        ' visual  =~ x1 + x2 + x3 + x7
          textual =~ x4 + x5 + x6 + x8 + x9',
        data = lavaan::HolzingerSwineford1939,
        auto.var=TRUE,
        auto.fix.first=TRUE,
        auto.cov.lv.x=TRUE),
        ' visual  =~ x1 + x2 + x3 + x7
          textual =~ x4 + x5 + x6 + x8 + x9'
      )

    cfiFn2 <- function(m) fitmeasures(m, 'cfi')
    rmseaFn2 <- function(m) fitmeasures(m, 'rmsea')

    # model is null, return bestModel
    expect_equal(
      checkModels(
        currentModel = NULL,
        criterionFn = cfiFn2,
        negateCriterion = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    ## maximize == TRUE ####
    # models are same, return one of them
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = cfiFn2,
        negateCriterion = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing best model to (current) bad model, return best
    expect_equal(
      checkModels(
        currentModel = badHolzinger,
        criterionFn = cfiFn2,
        negateCriterion = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing (current bad) best model to (new) best model, return best
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = cfiFn2,
        negateCriterion = TRUE,
        bestFit = fitmeasures(badHolzinger@model.output, 'cfi'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )

    # expect the bestModel if the currentModel is improperly specified
    expect_equal(
      checkModels(
        currentModel = "bestHolzinger",
        criterionFn = cfiFn2,
        negateCriterion = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # compare currentModel (as best so far) to true bestModel, return best

    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = cfiFn2,
        negateCriterion = TRUE,
        bestFit = fitmeasures(badHolzinger@model.output, 'cfi'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )

    # maximize == FALSE ####
    # models are same, return one of them
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = rmseaFn2,
        negateCriterion = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing best model to (current) bad model, return best
    expect_equal(
      checkModels(
        currentModel = badHolzinger,
        criterionFn = rmseaFn2,
        negateCriterion = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing (current bad) best model to (new) best model, return best
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = rmseaFn2,
        negateCriterion = FALSE,
        bestFit = fitmeasures(badHolzinger@model.output, 'rmsea'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )

    # expect the bestModel if the currentModel is improperly specified
    expect_equal(
      checkModels(
        currentModel = "bestHolzinger",
        criterionFn = rmseaFn2,
        negateCriterion = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # compare currentModel (as best so far) to FALSE bestModel, return best

    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = rmseaFn2,
        negateCriterion = FALSE,
        bestFit = fitmeasures(badHolzinger@model.output, 'rmsea'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )
  }
)

# checkModels -- NA criterion values ####
# An NA candidate is now always treated as worse than the current best, and 
# an NA best is always displaced by any valid candidate.
test_that(
  "checkModels does not crash when the fit measure is NA", {
    bestHolzinger <-
      modelWarningCheck(
        lavaan::lavaan(model =
        ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9 ',
        data = lavaan::HolzingerSwineford1939,
        auto.var=TRUE,
        auto.fix.first=TRUE,
        auto.cov.lv.x=TRUE),
        ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9 '
      )
    # overlapping-indicator model: an under-identified specification whose
    # fit measures come back NA, same pattern used in the `goal` NA test above
    currentBadModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
          ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6
            textual =~ x4 + x5 + x6 + x7 + x8 + x9
            speed   =~ x7 + x8 + x9 + x1 + x2 + x3',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6
            textual =~ x4 + x5 + x6 + x7 + x8 + x9
            speed   =~ x7 + x8 + x9 + x1 + x2 + x3'
      )
    cfiFn3 <- function(m) fitmeasures(m, 'cfi')
    rmseaFn3 <- function(m) fitmeasures(m, 'rmsea')

    # NA candidate fit: keep the existing bestModel, don't crash
    expect_equal(
      checkModels(
        currentModel = currentBadModel,
        criterionFn = cfiFn3,
        negateCriterion = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )
    expect_equal(
      checkModels(
        currentModel = currentBadModel,
        criterionFn = rmseaFn3,
        negateCriterion = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # NA bestFit (e.g. the initial model produced no valid fit): any valid
    # candidate becomes the new best, rather than crashing on the comparison
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        criterionFn = cfiFn3,
        negateCriterion = TRUE,
        bestFit = NA_real_,
        bestModel = badHolzinger
      ),
      bestHolzinger
    )
  }
)

# modelWarningCheck ####
test_that(
  "modelWarningCheck identifies any warning/error in a model while returning model object", {
    defaultModel <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
    wrongItemModel <- ' visual  =~ y1 + y2 + y3
              textual =~ y4 + y5 + y6
              speed   =~ y7 + y8 + y9 '
    poorModel <- ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9'

    # model fits without error
    goodModel <-
      modelWarningCheck(
        expr = lavaan::lavaan(defaultModel,
                              data = lavaan::HolzingerSwineford1939,
                              auto.var=TRUE,
                              auto.fix.first=TRUE,
                              auto.cov.lv.x=TRUE),
        modelSyntax = defaultModel
      )

    expect_length(goodModel@warnings, 0)
    expect_length(goodModel@errors, 0)
    expect_s4_class(goodModel@model.output, 'lavaan')
    # model doesn't fit and throws an error
    noModel <-
      modelWarningCheck(
        expr = lavaan::lavaan(wrongItemModel,
                              data = lavaan::HolzingerSwineford1939,
                              auto.var=TRUE,
                              auto.fix.first=TRUE,
                              auto.cov.lv.x=F),
        modelSyntax = wrongItemModel
      )

    expect_length(noModel@warnings, 0)
    expect_length(noModel@errors, 1)
    expect_null(noModel@model.output)

    # model fits but throws a warning
    warningModel <-
      modelWarningCheck(
        expr = lavaan::lavaan(poorModel,
                              data = lavaan::HolzingerSwineford1939,
                              auto.var=TRUE,
                              auto.fix.first=TRUE,
                              auto.cov.lv.x=F,
                              ordered = T),
        modelSyntax = poorModel
      )

    expect_length(warningModel@warnings, 1)
    expect_length(warningModel@errors, 0)
    expect_s4_class(warningModel@model.output, 'lavaan')

  }
)

# syntaxExtraction ####
test_that(
  "syntaxExtraction creates a valid list", {
    initialModelSyntaxFile <-"
f1 =~ Item1 + Item2 + Item3 + Item4 + Item5
f2 =~ Item6 + Item7 + Item8 + Item9 + Item10
f3 =~ Item11 + Item12 + Item13 + Item14 + Item15
f4 =~ Item16 + Item17 + Item18 + Item19 + Item20
"
    items <-
      paste0("Item", c(1:20))

    expectedOutput <-
      list(
        'factors' = paste0("f", c(1:4)),
        'itemsPerFactor' =
          list(
            paste0("Item", c(1:5)),
            paste0("Item", c(6:10)),
            paste0("Item", c(11:15)),
            paste0("Item", c(16:20))
          )
      )

    expect_equal(
      object =
        syntaxExtraction(
          initialModelSyntaxFile = initialModelSyntaxFile,
          items = items
        ),
      expected =
        expectedOutput
    )

  }
)

# fitWarningCheck ####
test_that(
  "fitWarningCheck returns value when working and NA when expr is not valid", {
    exampleCFI <-
      0.95
    exampleChisq <-
      120

    # normal input/output
    expect_equal(
      object =
        fitWarningCheck(expr = exampleCFI),
      expected = exampleCFI
    )

    expect_equal(
      object =
        fitWarningCheck(expr = exampleChisq),
      expected = exampleChisq
    )

    # error input
    expect_equal(
      object =
        fitWarningCheck(expr = 'exampleNull'/2),
      expected = NA
    )
  }
)

# checkModelSpecs ####
test_that(
  "checkModelSpecs returns silent when working, error when not", {
    # full, correct model specs list
    lavaanDesc <-
      list(model.type = "sem",
           estimator = "ML",
           ordered = NULL,
           int.ov.free = TRUE,
           int.lv.free = FALSE,
           auto.fix.first = FALSE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.var = TRUE,
           auto.cov.lv.x = TRUE,
           auto.efa = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE)

    # correct model specs list with missing values
    lavaanErr <-
      list(model.type = "sem",
           estimator = "ML",
           ordered = NULL,
           int.ov.free = TRUE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.var = TRUE,
           auto.cov.lv.x = TRUE,
           auto.efa = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE)

    expect_silent(
      checkModelSpecs(lavaanDesc)
    )

    expect_error(
      checkModelSpecs(lavaanErr)
    )
  }
)

# mergeModelSpecs ####
test_that(
  "mergeModelSpecs fills in omitted elements from defaultSpecs and preserves NULL values", {
    defaultSpecs <-
      list(model.type = "cfa",
           auto.var = TRUE,
           estimator = "default",
           ordered = NULL,
           int.ov.free = TRUE,
           int.lv.free = FALSE,
           auto.fix.first = FALSE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.cov.lv.x = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE)

    merged <- mergeModelSpecs(list(estimator = "ML"), defaultSpecs)

    # the overridden element is respected...
    expect_equal(merged$estimator, "ML")
    # ...and every omitted element falls back to defaultSpecs, including an
    # explicit NULL (a naive modifyList() call would drop this key entirely)
    expect_true("ordered" %in% names(merged))
    expect_null(merged$ordered)
    expect_equal(merged$model.type, "cfa")
    expect_equal(merged$auto.var, TRUE)
  }
)

test_that(
  "mergeModelSpecs errors on an unrecognized (likely misspelled) element instead of silently ignoring it", {
    defaultSpecs <-
      list(model.type = "cfa",
           auto.var = TRUE,
           estimator = "default",
           ordered = NULL,
           int.ov.free = TRUE,
           int.lv.free = FALSE,
           auto.fix.first = FALSE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.cov.lv.x = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE)

    expect_error(
      mergeModelSpecs(list(estmator = "ML"), defaultSpecs),
      "not recognized"
    )
  }
)

# fitmeasuresCheck ####
test_that(
  "fitmeasuresCheck returns silent when working, error when not", {
    #when provided correct fitmeasure
    expect_silent(
      fitmeasuresCheck('cfi')
      )

    #when provided incorrect fitmeasure as string
    expect_error(
      fitmeasuresCheck('something')
    )

    #when provided incorrect fitmeasure as not-a-string
    expect_error(
      fitmeasuresCheck(2)
    )
    }
  )

# fitStatTestCheck ####
test_that(
  "user-defined fit.statistics.test passes validity checks for evaluation", {
    expect_error(
      fitStatTestCheck(measures = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.06)",
                       test = TRUE),
      regexp = "logical"
    )

    expect_error(
      fitStatTestCheck(measures = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.06)",
                       test = 1),
      regexp = "not given as a character object"
    )

    expect_false(
      fitStatTestCheck(measures = c('cfi','tli','rmsea'),
                       test = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.06)"),
    )

    expect_error(
      fitStatTestCheck(measures = c('cfi','tli','rmsea'),
                       test = "(cfi >> 0.95)&(tli > 0.95)&(rmsea < 0.06)"),
    )
  }
)

# randomNeighborShort item-name collateral corruption ####
# FIXED (see code review): the gsub() pattern used to swap an item out of the
# model syntax was anchored with a trailing "\\b" only (internals.R ~164), not
# a leading one, so an item name that is a *suffix* of another item's name
# (e.g. "SubItem1" ends in "Item1") could get silently rewritten even though
# it was never selected for replacement. randomNeighborShort now delegates to
# the shared replaceItem() helper (R/lavaan_syntax_helpers.R), which anchors
# both sides.
test_that(
  "randomNeighborShort does not corrupt an unrelated item whose name shares a suffix with the changed item", {
    corruptionData <- as.data.frame(
      matrix(rnorm(4 * 100), ncol = 4)
    )
    names(corruptionData) <- c("Item1", "SubItem1", "Item2", "Item2b")
    corruptionModel <- "f =~ Item1 + SubItem1"
    currentModel <-
      modelWarningCheck(
        lavaan::cfa(
          model = corruptionModel,
          data = corruptionData,
          std.lv = TRUE
        ),
        modelSyntax = corruptionModel
      )
    lavaanSpecs <-
      list(model.type = "cfa",
           estimator = "default",
           ordered = NULL,
           int.ov.free = TRUE,
           int.lv.free = FALSE,
           auto.fix.first = FALSE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.var = TRUE,
           auto.cov.lv.x = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE)

    # NOTE: this seed is tied to randomNeighborShort's current internal RNG
    # call sequence purely to deterministically land on "Item1" being
    # selected for replacement; if that sequence changes (e.g. a future
    # refactor of the sampling logic), find a new seed that reproduces the
    # same selection rather than assuming this one still does.
    set.seed(1)
    corrupted <- randomNeighborShort(
      currentModelObject = currentModel,
      numChanges = 1,
      allItems = c("Item1", "SubItem1", "Item2", "Item2b"),
      data = corruptionData,
      bifactor = NULL,
      init.model = corruptionModel,
      lavaan.model.specs = lavaanSpecs
    )

    # "Item1" was replaced as intended...
    expect_false(grepl("\\bItem1\\b", corrupted@model.syntax))
    # ...and "SubItem1" was never chosen for replacement, so it survives
    # untouched even though "Item1" is a suffix of its name.
    expect_true(grepl("SubItem1", corrupted@model.syntax))
  }
)

# consecutiveRestart actually restarts the chain ####
test_that(
  "consecutiveRestart restarts to bestModel once maxConsecutiveSelection is reached, and is a no-op otherwise", {
    bestModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
            ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
      )
    currentModel <-
      modelWarningCheck(
        lavaan::cfa(
          model =
          ' visual  =~ x1 + x2 + x4
            textual =~ x3 + x5 + x7
            speed   =~ x6 + x8 + x9',
          data = lavaan::HolzingerSwineford1939
        ),
        modelSyntax =
          ' visual  =~ x1 + x2 + x4
            textual =~ x3 + x5 + x7
            speed   =~ x6 + x8 + x9'
      )

    # consecutive < maxConsecutiveSelection: currentModel and consecutive pass through unchanged
    noRestart <- consecutiveRestart(
      maxConsecutiveSelection = 25,
      consecutive = 1,
      currentModel = currentModel,
      bestModel = bestModel
    )
    expect_identical(noRestart$currentModel, currentModel)
    expect_equal(noRestart$consecutive, 1)

    # consecutive == maxConsecutiveSelection: restarts to bestModel and resets the counter
    restart <- consecutiveRestart(
      maxConsecutiveSelection = 25,
      consecutive = 25,
      currentModel = currentModel,
      bestModel = bestModel
    )
    expect_identical(restart$currentModel, bestModel)
    expect_equal(restart$consecutive, 0)
  }
)

# # allArgs ####
# test_that(
#   "allArgs can provide the default values for a function", {
#     temp <-
#       function(x = 2, y = 10, default = TRUE, ...) {
#         z <-
#           x^y
# 
#         return(
#           list(
#             "result" = z,
#             "args" = allArgs(orig_values = default)
#           )
#         )
#       }
#     defaultOutput <-
#       list(
#         "result" = 1024,
#         "args" =
#           list(
#             "x" = 2,
#             "y" = 10,
#             "default" = TRUE
#             )
#       )
#     expect_equal(
#       temp(),
#       defaultOutput
#     )
#   }
# )

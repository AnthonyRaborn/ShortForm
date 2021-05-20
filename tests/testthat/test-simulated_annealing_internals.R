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
    maxItems <- c(2,2,2)
    allItems <- paste0("x", 1:9)
    data <- lavaan::HolzingerSwineford1939
    bifactor <- FALSE
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
           missing = "listwise")

    ## bifactorModel == FALSE ####
    # should output a modelCheck object
    expect_s4_class(
      randomInitialModel(
        init.model = defaultModel,
        maxItems = maxItems,
        allItems = allItems,
        initialData = data,
        bifactorModel = bifactor,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )

    # bifactor == TRUE ####
    bifactor = TRUE
    # should output a list
    expect_s4_class(
      randomInitialModel(
        init.model = defaultModel,
        maxItems = maxItems,
        allItems = allItems,
        initialData = data,
        bifactorModel = bifactor,
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
        modelSyntax = defaultModel
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
           missing = "listwise")

    ## bifactorModel == FALSE ####
    # should output a modelCheck object
    expect_s4_class(
      randomNeighborShort(
        currentModelObject = currentModel,
        numChanges = 1,
        allItems = paste0("x", 1:9),
        data = lavaan::HolzingerSwineford1939,
        bifactor = FALSE,
        init.model = defaultModel,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )


    # bifactorModel == FALSE ####
    # should output a modelCheck object
    expect_s4_class(
      randomNeighborShort(
        currentModelObject = currentBifactor,
        numChanges = 1,
        allItems = paste0("x", 1:9),
        data = lavaan::HolzingerSwineford1939,
        bifactor = TRUE,
        init.model = defaultBifactor,
        lavaan.model.specs = lavaanSpecs
      ),
      'modelCheck'
    )
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

    # lavaan input with single, maximized fit statistic should equal the negative value of the fit statistic
    expect_equal(
      goal(
        x = currentModel@model.output,
        fitStatistic = 'cfi',
        maximize = T
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
        fitStatistic = 'rmsea',
        maximize = F
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
        fitStatistic = 'cfi',
        maximize = T
      ),
      Inf
    )

    # lavaan input with no fit statistc due to poor model, goal minimized
    expect_equal(
      goal(
        x = currentBadModel@model.output,
        fitStatistic = 'rmsea',
        maximize = F
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

    # new model does not converge, so return currentModelObject with message
    expect_equal(
      selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = currentBadModel,
        currentTemp = currentTemp,
        maximize = TRUE,
        fitStatistic = "cfi",
        consecutive = 1
      ),
      currentModel
    )
    expect_output(
      selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = currentBadModel,
        currentTemp = currentTemp,
        maximize = TRUE,
        fitStatistic = "cfi",
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
        maximize = TRUE,
        fitStatistic = "cfi",
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
        maximize = TRUE,
        fitStatistic = "cfi",
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
        maximize = TRUE,
        fitStatistic = "cfi",
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
        maximize = TRUE,
        fitStatistic = "cfi",
        consecutive = 1
      ),
      "^((?!Probability:))",
      perl = TRUE
    )
  }
)
# consecutiveRestart ####
test_that(
  "consecutiveRestart produces the correct model depending on the value of consecutive", {
    # currently not really testing due to how consecutiveRestart is working
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
      testedModel <-
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
    consecutive = 1

    # consecutive < maxConsecutiveSelection, do not replace currentModel
    expect_false(
      c(
        consecutiveRestart(
          maxConsecutiveSelection = 25,
          consecutive = 1
          ), identical(currentModel, bestModel)
      )
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

    # model is null, return bestModel
    expect_equal(
      checkModels(
        currentModel = NULL,
        fitStatistic = 'cfi',
        maximize = TRUE,
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
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing best model to (current) bad model, return best
    expect_equal(
      checkModels(
        currentModel = badHolzinger,
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing (current bad) best model to (new) best model, return best
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(badHolzinger@model.output, 'cfi'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )

    # expect the bestModel if the currentModel is improperly specified
    expect_equal(
      checkModels(
        currentModel = "bestHolzinger",
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # compare currentModel (as best so far) to true bestModel, return best

    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        fitStatistic = 'cfi',
        maximize = TRUE,
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
        fitStatistic = 'rmsea',
        maximize = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing best model to (current) bad model, return best
    expect_equal(
      checkModels(
        currentModel = badHolzinger,
        fitStatistic = 'rmsea',
        maximize = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing (current bad) best model to (new) best model, return best
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        fitStatistic = 'rmsea',
        maximize = FALSE,
        bestFit = fitmeasures(badHolzinger@model.output, 'rmsea'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )

    # expect the bestModel if the currentModel is improperly specified
    expect_equal(
      checkModels(
        currentModel = "bestHolzinger",
        fitStatistic = 'rmsea',
        maximize = FALSE,
        bestFit = fitmeasures(bestHolzinger@model.output, 'rmsea'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # compare currentModel (as best so far) to FALSE bestModel, return best

    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        fitStatistic = 'rmsea',
        maximize = FALSE,
        bestFit = fitmeasures(badHolzinger@model.output, 'rmsea'),
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

    expect_length(goodModel@warnings, 1)
    expect_length(goodModel@errors, 1)
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

    expect_length(noModel@warnings, 1)
    expect_length(noModel@errors, 2)
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

    expect_length(warningModel@warnings, 2)
    expect_length(warningModel@errors, 1)
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
        fitWarningCheck(expr = exampleCFI,
                        maximize = TRUE),
      expected = exampleCFI
    )

    expect_equal(
      object =
        fitWarningCheck(expr = exampleChisq,
                        maximize = FALSE),
      expected = exampleChisq
    )

    # error input, maximize
    expect_equal(
      object =
        fitWarningCheck(expr = 'exampleNull'/2,
                        maximize = TRUE),
      expected = NA
    )

    # error input, minimize
    expect_equal(
      object =
        fitWarningCheck(expr = 'exampleNull'/2,
                        maximize = FALSE),
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

# allArgs ####
test_that(
  "allArgs can provide the default values for a function", {
    temp <-
      function(x = 2, y = 10, default = TRUE, ...) {
        z <-
          x^y

        return(
          list(
            "result" = z,
            "args" = allArgs(orig_values = default)
          )
        )
      }
    defaultOutput <-
      list(
        "result" = 1024,
        "args" =
          list(
            "x" = 2,
            "y" = 10,
            "default" = TRUE
            )
      )
    expect_equal(
      temp(),
      defaultOutput
    )
  }
)

randomNeighborShort <-
  function(currentModelObject,
           numChanges,
           allItems,
           data,
           bifactor = FALSE,
           init.model,
           lavaan.model.specs) {
    
    mapply(
      assign,
      c("factors", "itemsPerFactor"),
      syntaxExtraction(initialModelSyntaxFile = init.model, items = allItems),
      MoreArgs = list(envir = environment())
    )
    
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = environment())
    )
    # take the model syntax from the currentModelObject
    internalModelObject <- 
      stringr::str_split(currentModelObject$model.syntax, 
                         pattern = "\n",
                         simplify = T)
    
    # extract the latent factor syntax
    mapply(
      assign,
      c("factors", "currentItems"),
      syntaxExtraction(initialModelSyntaxFile = internalModelObject, items = allItems),
      MoreArgs = list(envir = environment())
    )
    
    # randomly select current items to replace
    
    replacePattern <- paste0("\\b",
                             paste0(allItems,
                                    collapse = "\\b|\\b"
                             ),
                             "\\b")
    
    replacementItemPool <- c()
    for (i in 1:length(factors)) {
      if (class(allItems) == "list") {
        replacementItemPool[[i]] <- 
          allItems[[i]][!(allItems[[i]] %in% currentItems[[i]])]
      } else {
        replacementItemPool[[i]] <-
          allItems[!(allItems %in% currentItems[[i]])]
      }
    }
    changingItems <- c()
    replacementItem <- c()
    for (i in 1:numChanges) {
      # randomly select factor to have an item changed
      if (bifactor) {
        currentFactor <- sample(1:(length(factors) - 1), 1)
      } else {
        currentFactor <- sample(1:length(factors), 1)
      }
      # randomly select the item to be changed
      changingItemTemp <- c()
      changingItemTemp <- sample(currentItems[[currentFactor]], 1)
      while (changingItemTemp %in% changingItems ||
             length(changingItemTemp %in% changingItems) == 0) {
        changingItemTemp <- sample(currentItems[[currentFactor]], 1)
      }
      changingItems <- c(changingItems, changingItemTemp)
      # Sample an item from the items in the item pool
      tempReplacementItems <- sample(replacementItemPool[[currentFactor]], 1)
      while (tempReplacementItems %in% replacementItem) {
        tempReplacementItems <- sample(replacementItemPool[[currentFactor]], 1)
      }
      replacementItem <- c(replacementItem, tempReplacementItems)
    }
    
    for (i in 1:length(factors)) {
      for (j in 1:numChanges) {
        currentItems[[i]] <-
          gsub(
            pattern = paste0(changingItems[j], "\\b"),
            replacement = replacementItem[j],
            x = currentItems[[i]]
          )
      }
    }
    
    if (bifactor == TRUE) {
      # if bifactor == TRUE, fix the items so the newItems all load on the bifactor
      # assumes that the bifactor latent variable is the last one
      currentItems[[length(itemsPerFactor)]] <- unlist(currentItems[1:(length(itemsPerFactor) - 1)])
    }
    
    # create the new model syntax
    newModelSyntax <- as.vector(
      stringr::str_split(currentModelObject$model.syntax, 
                         "\n", 
                         simplify = T)
    )
    for (i in 1:length(factors)) {
      newModelSyntax[i] <- paste(
        factors[i],
        "=~",
        paste(currentItems[[i]], collapse = " + ")
      )
    }
    
    newModelSyntax <- stringr::str_flatten(newModelSyntax,
                                           collapse = "\n")
    
    # refit the model with new items
    randomNeighborModel <- modelWarningCheck(
      lavaan::lavaan(
        model = newModelSyntax,
        data = data,
        model.type = model.type,
        auto.var = auto.var,
        ordered = ordered,
        estimator = estimator,
        int.ov.free = int.ov.free,
        int.lv.free = int.lv.free,
        auto.fix.first = auto.fix.first,
        std.lv = std.lv,
        auto.fix.single = auto.fix.single,
        auto.cov.lv.x = auto.cov.lv.x,
        auto.th = auto.th,
        auto.delta = auto.delta,
        auto.cov.y = auto.cov.y
      )
    )
    
    randomNeighborModel$model.syntax = newModelSyntax
    return(randomNeighborModel)
  }

selectionFunction <-
  function(currentModelObject = currentModel,
           randomNeighborModel,
           currentTemp,
           maximize,
           fitStatistic,
           consecutive) {
    # check if the randomNeighborModel is a valid model for use
    if (length(randomNeighborModel[[2]]) > 0 |
        length(randomNeighborModel[[2]]) > 0) {
      return(currentModelObject)
    }
    
    # check that the current model isn't null
    if (is.null(currentModelObject[[1]])) {
      return(randomNeighborModel)
    } else {
    
    probability = exp(-(goal(randomNeighborModel[[1]], fitStatistic, maximize) - goal(currentModelObject[[1]], fitStatistic, maximize)) / currentTemp)
    
    }
    
    if (probability > stats::runif(1)) {
      newModel = randomNeighborModel
    } else { 
      newModel = currentModelObject
    }
  }

goal <- function(x, fitStatistic = 'cfi', maximize) {
  # if using lavaan and a singular fit statistic,
  if (class(x) == "lavaan" & is.character(fitStatistic) & length(fitStatistic) == 1) {
    energy <- fitWarningCheck(lavaan::fitMeasures(x, fit.measures = fitStatistic),
                            maximize)
    
    # if trying to maximize a value, return its negative
    if (maximize == TRUE) {
      return(-energy)
    } else {
      # if minimizing a value, return the value
      return(energy)
    }
  } else {
    if (class(x) == "NULL") {
      if (maximize == TRUE) {
        return(-Inf)
      } else {
        return(Inf)
      }
    }
  }
}

consecutiveRestart <-
  function(maxConsecutiveSelection = 25, consecutive) {
    if (consecutive == maxConsecutiveSelection) {
      currentModel = bestModel
      consecutive = 0
    }
  }

linearTemperature <- function(currentStep, maxSteps) {
  currentTemp <- (maxSteps - (currentStep)) / maxSteps
}

quadraticTemperature <- function(currentStep, maxSteps) {
  currentTemp <- ((maxSteps - (currentStep)) / maxSteps) ^ 2
}

logisticTemperature <- function(currentStep, maxSteps) {
  x = 1:maxSteps
  x.new = scale(x, center = T, scale = maxSteps / 12)
  currentTemp <- 1 / (1 + exp((x.new[(currentStep + 1)])))
}

checkModels <- function(currentModel, fitStatistic, maximize = maximize, bestFit = bestFit, bestModel) {
  if (class(currentModel) == "list") {
    currentSyntax <- currentModel$model.syntax
    currentModel <- currentModel[[1]]
  }
  if (is.null(currentModel)) {
    return(bestModel)
  }
  currentFit = fitWarningCheck(
    lavaan::fitmeasures(object = currentModel, fit.measures = fitStatistic),
    maximize
    )
  if (maximize == TRUE) {
    if (currentFit > bestFit) {
      bestModel = list()
      bestModel$bestModel = currentModel
      bestModel$bestSyntax = currentSyntax
    } else {
      bestModel = bestModel
    }
  } else {
    if (currentFit < bestFit) {
      bestModel = list()
      bestModel$bestModel = currentModel
      bestModel$bestSyntax = currentSyntax
    } else {
      if (currentFit < bestFit) {
        bestModel = currentModel
      } else {
        bestModel = bestModel
      }
    }
  }
  
    return(bestModel)
  }

modelWarningCheck <- function(expr) {
  warn <- err <- c()
  value <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        err <<-
          append(err, regmatches(paste(e), gregexpr("ERROR: [A-z ]{1,}", paste(e))))
        NULL
      }
    ),
    warning = function(w) {
      warn <<-
        append(warn, regmatches(paste(w), gregexpr("WARNING: [A-z ]{1,}", paste(w))))
      invokeRestart("muffleWarning")
    }
  )
  list(
    'lavaan.output' = value,
    'warnings' <-
      as.character(unlist(warn)),
    'errors' <- as.character(unlist(err))
  )
}


syntaxExtraction = function(initialModelSyntaxFile, items) {
  
  # extract the latent factor syntax
  factors = unique(lavaan::lavaanify(initialModelSyntaxFile)[lavaan::lavaanify(initialModelSyntaxFile)$op ==
                                                               "=~", 'lhs'])
  vectorModelSyntax = stringr::str_split(string = initialModelSyntaxFile,
                                         pattern = '\\n',
                                         simplify = TRUE)
  factorSyntax = c()
  itemSyntax = c()
  for (i in 1:length(factors)) {
    chosenFactorLocation = c(1:length(vectorModelSyntax))[grepl(x = vectorModelSyntax, pattern = paste0(factors[i], "[ ]{0,}=~ "))]
    factorSyntax[i] = vectorModelSyntax[chosenFactorLocation]
    # remove the factors from the syntax
    itemSyntax[i] <-
      gsub(
        pattern = paste(factors[i], "=~ "),
        replacement = "",
        x = factorSyntax[i]
      )
  }
  
  # extract the items for each factor
  itemsPerFactor = stringr::str_extract_all(string = itemSyntax,
                                            pattern = paste0("(\\b", paste0(
                                              paste0(unlist(items), collapse = "\\b)|(\\b"), "\\b)"
                                            )))
  return(list('factors' = factors, 'itemsPerFactor' = itemsPerFactor))
}

modelWarningCheck <- function(expr) {
  warn <- err <- c()
  value <- withCallingHandlers(tryCatch(expr, error = function(e) {
    err <<- append(err, regmatches(paste(e), gregexpr("ERROR: [A-z ]{1,}", 
                                                      paste(e))))
    NULL
  }), warning = function(w) {
    warn <<- append(warn, regmatches(paste(w), gregexpr("WARNING: [A-z ]{1,}", 
                                                        paste(w))))
    invokeRestart("muffleWarning")
  })
  list(lavaan.output = value, warnings <- as.character(unlist(warn)), 
       errors <- as.character(unlist(err)))
}

fitWarningCheck <- function(expr, maximize) {
  value <- withCallingHandlers(tryCatch(expr, 
                                        error = function(e) {
    if (maximize == T) {
      return(0)
    } else {
      return(Inf)
    }
    invokeRestart("muffleWarning")
  }
  )
  )
  return(value)
}


checkModelSpecs <- 
  function(
    x
  ) {
    
    requiredElements <-
      c('model.type',
        'auto.var',
        'estimator',
        'ordered',
        'int.ov.free',
        'int.lv.free',
        'auto.fix.first',
        'auto.fix.single',
        'auto.cov.lv.x',
        'auto.th',
        'auto.delta',
        'auto.cov.y',
        'std.lv')
    
    missingSpecs <-
      requiredElements[
        which(
          !requiredElements %in% names(x)
        )
        ]
    
    if (length(missingSpecs) > 0) {
      errorMessage <-
        paste0("The following elements of lavaan.model.specs have not been specified:\n\n",
               paste(missingSpecs, collapse = "\n"),
               "\n\nPlease include the proper specifications for these elements, or use the default values provided.")
      stop(errorMessage)
    }
  }

fitmeasuresCheck <-
  function(
    x
  ) {
    validMeasures <-
      c(
        "npar", "fmin",
        "chisq", "df", "pvalue", 
        "baseline.chisq", "baseline.df", "baseline.pvalue",    
        "cfi", "tli", "nnfi",
        "rfi", "nfi", "pnfi",
        "ifi", "rni",                
        "logl", "unrestricted.logl",
        "aic", "bic", "ntotal", "bic2",
        "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
        "rmr", "rmr_nomean", 
        "srmr", "srmr_bentler", "srmr_bentler_nomean", 
        "crmr", "crmr_nomean", "srmr_mplus", "srmr_mplus_nomean",
        "cn_05", "cn_01", 
        "gfi", "agfi", "pgfi", "mfi", "ecvi",
        "chisq.scaled", "df.scaled", "pvalue.scaled", "chisq.scaling.factor",
        "baseline.chisq.scaled", "baseline.df.scaled", "baseline.pvalue.scaled",
        "baseline.chisq.scaling.factor",
        "cfi.scaled", "tli.scaled",                   
        "cfi.robust", "tli.robust",                   
        "nnfi.scaled", "nnfi.robust",                  
        "rfi.scaled",  "nfi.scaled", "ifi.scaled", "rni.scaled", "rni.robust",
        "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
        "rmsea.pvalue.scaled", "rmsea.robust", "rmsea.ci.lower.robust", 
        "rmsea.ci.upper.robust", "rmsea.pvalue.robust"
      )
    
    invalidMeasures <-
      x[
        which(
          !x %in% validMeasures
        )
        ]
    
    if (length(invalidMeasures) > 0) {
      errorMessage <-
        paste0("The following elements of fit.indices or fitStatistics are not valid fit measures provided by the lavaan::fitmeasures function:\n\n",
               paste(invalidMeasures, collapse = "\n"),
               "\n\nPlease check the output of this function for proper spelling and capitalization of the fit measure(s) you are interested in.")
      stop(errorMessage)
    }

  }

fitStatTestCheck <-
  function(measures, test) {
    tempEnv <-
      new.env()
    mapply(
      assign, 
      measures, 
      0, 
      MoreArgs=list(envir = tempEnv))
    
    checkIfEval <-
      tryCatch(
        expr = eval(parse(text=test), 
                    envir = tempEnv), 
        error = function(e) {
          stop("There was a problem with the fit.statistics.test provided. It cannot be evaluated properly. Please read the function documentation to see how to properly specify a test.")
        }
      )
    
    if (!is.character(test)) {
          stop("There is a problem with the fit.statistics.test provided. The fit.statistics.test was given as a logical, not a character. Please read the function documentation to see how to properly specify a test. ")
        }
      
  }

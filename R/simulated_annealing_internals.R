randomInitialModel <- 
  function(init.model,
           maxItems,
           allItems,
           initialData,
           bifactorModel,
           lavaan.model.specs) {
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = environment())
    )
  # extract the latent factor syntax
  mapply(
    assign,
    c("factors", "itemsPerFactor"),
    syntaxExtraction(initialModelSyntaxFile = init.model, items = allItems),
    MoreArgs = list(envir = environment())
  )
  
  # save the external relationships
  vectorModel <- unlist(strsplit(x = init.model, split = "\\n"))
  externalRelation <- vectorModel[grep(" ~ ", vectorModel)]
  factorRelation <- vectorModel[grep(" ~~ ", vectorModel)]
  
  # reduce the number of items for each factor according to maxItems
  newItemsPerFactor <- list()
  for (i in 1:length(itemsPerFactor)) {
    newItemsPerFactor[[i]] <- 
      sample(x = unique(unlist(itemsPerFactor[i])), 
             size = unlist(maxItems[i]))
  }
  
  if (bifactorModel == TRUE) {
    # if bifactorModel == TRUE, fix the items so the newItems all load on the bifactor
    # assumes that the bifactor latent variable is the last one
    newItemsPerFactor[[length(itemsPerFactor)]] <- unlist(newItemsPerFactor[1:(length(itemsPerFactor) - 1)])
  }
  
  # create the new model syntax
  
  newModelSyntax <- c()
  for (i in 1:length(factors)) {
    newModelSyntax[i] <- paste(
      factors[i],
      "=~",
      paste(newItemsPerFactor[[i]], collapse = " + ")
    )
  }
  newModelSyntax <- 
    paste0(newModelSyntax, externalRelation, factorRelation, collapse = "\n")
  newModelSyntax <-
    stringr::str_replace_all(newModelSyntax, "\n\n", "\n")
  
  # fit the new model
  newModel <- modelWarningCheck(
    lavaan::lavaan(
      model = newModelSyntax,
      data = initialData,
      model.type = model.type,
      int.ov.free = int.ov.free,
      int.lv.free = int.lv.free,
      auto.fix.first = auto.fix.first,
      std.lv = std.lv,
      auto.fix.single = auto.fix.single,
      auto.var = auto.var,
      auto.cov.lv.x = auto.cov.lv.x,
      auto.th = auto.th,
      auto.delta = auto.delta,
      auto.cov.y = auto.cov.y,
      ordered = ordered,
      estimator = estimator,
    ),
    modelSyntax = newModelSyntax
  )
  
  return(newModel)
}

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
      stringr::str_split(currentModelObject@model.syntax, 
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
      if (inherits(allItems, "list")) {
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
      stringr::str_split(currentModelObject@model.syntax, 
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
      ),
      modelSyntax = newModelSyntax
    )
    
    return(randomNeighborModel)
  }

randomNeighborFull <-
  function(currentModelObject = currentModel,
           numChanges = numChanges,
           data) {
    # using lavaan functions, construct a full parameter table
    paramTable <- lavaan::parTable(currentModelObject)
    
    # select the rows that correspond to parameters related to the latent variables
    latentVariables <-
      row.names(lavaan::inspect(currentModelObject, "cor.lv"))
    currentModelParamsLV <-
      paramTable[paramTable$lhs %in% latentVariables, ]
    
    # randomly select rows to make changes to
    randomChangesRows <-
      sample(currentModelParamsLV$id, size = numChanges)
    changeParamTable <- paramTable[randomChangesRows, ]
    
    # make the changes. If currently free, fix to 0; if fixed to 0, set to free
    paramTable$free[randomChangesRows] <-
      ifelse(paramTable$free[randomChangesRows] != 0, 0, 1)
    
    # remove the starting value, estimates, and standard errors of the currentModel
    paramTable$est <- NULL
    paramTable$se <- NULL
    paramTable$start <- NULL
    paramTable$labels <- NULL
    
    # refit the model
    prevModel <- as.list(currentModelObject@call)
    prevModel$model <- paramTable
    
    randomNeighborModel <-
      modelWarningCheck(
      lavaan::lavaan(
        model = prevModel$model,
        data = data
      ),
      modelSyntax = prevModel$model
    )
    
    return(randomNeighborModel)
  }

goal <-
  function(x, fitStatistic = "cfi", maximize) {
    # if using lavaan and a singular fit statistic,
    if (inherits(x, "lavaan") &
        is.character(fitStatistic) & length(fitStatistic) == 1) {
      energy <- fitWarningCheck(lavaan::fitMeasures(x, fit.measures = fitStatistic),
                                maximize)
      if (is.na(energy) & maximize == TRUE) {
        energy = -Inf
      } else if (is.na(energy)) {
        energy = Inf
      }
      
      if (maximize == TRUE) {
        return(-energy)
      } else {
        return(energy)
      }
    } else {
      if (inherits(x, "NULL")) {
        if (maximize == TRUE) {
          return(-Inf)
        } else {
          return(Inf)
        }
      }
    }
  }

selectionFunction <-
  function (currentModelObject,
            randomNeighborModel,
            currentTemp,
            maximize,
            fitStatistic,
            consecutive)
  {
    # check if the randomNeighborModel is a valid model for use
    if (!inherits(randomNeighborModel, "modelCheck")) {
      return(currentModelObject)
    }
    if ( 
      length(randomNeighborModel@warnings) > 1 |
      length(randomNeighborModel@errors) > 1 
    ) {
      return(currentModelObject)
    }
    
    # check that the current model isn't null
    if (!inherits(currentModelObject, "modelCheck")) {
      return(randomNeighborModel)
      }
    if (is.null(currentModelObject@model.output)) {
      return(randomNeighborModel)
    }
    
    randomNeighborConverge <-
      tryCatch(
        lavaan::fitmeasures(object = randomNeighborModel@model.output,
                            fit.measures = fitStatistic),
        error = function(e) {
          if (length(e) > 0) {
            NULL
          }
        }
      )
    if (is.null(randomNeighborConverge)) {
      cat("\rNew model did not converge.                                                                                          ")
      return(currentModelObject)
    }
    # this is the Kirkpatrick et al. method of selecting between currentModel and randomNeighborModel
    if (goal(randomNeighborModel@model.output, fitStatistic, maximize) < goal(currentModelObject@model.output, fitStatistic, maximize)) {
      cat(paste0(
        "\rOld Fit: ",
        round(as.numeric(
          fitWarningCheck(
            lavaan::fitmeasures(object = currentModelObject@model.output,
                                fit.measures = fitStatistic),
            maximize
          )
        ), 3),
        " New Fit: ",
        round(as.numeric(
          fitWarningCheck(
            lavaan::fitmeasures(object = randomNeighborModel@model.output,
                                fit.measures = fitStatistic),
            maximize
          )
        ), 3),
        "                                                                    "
      ))
      consecutive <- consecutive + 1
      return(randomNeighborModel)
    } else {
      probability <- exp(-(
        goal(randomNeighborModel@model.output, fitStatistic, maximize) - goal(currentModelObject@model.output, fitStatistic, maximize)
      ) / currentTemp)
      
      if (is.nan(probability)) probability = 0
      
      if (probability > stats::runif(1)) {
        newModel <- randomNeighborModel
      } else {
        newModel <- currentModelObject
      }
      
      consecutive <- ifelse(
        identical(
          lavaan::parameterTable(newModel@model.output),
          lavaan::parameterTable(currentModelObject@model.output)
        ),
        consecutive + 1,
        0
      )
      cat(paste0(
        "\rOld Fit: ",
        round(as.numeric(
          lavaan::fitmeasures(object = currentModelObject@model.output,
                              fit.measures = fitStatistic)
        ), 3),
        " New Fit: ",
        round(as.numeric(
          lavaan::fitmeasures(object = randomNeighborModel@model.output,
                              fit.measures = fitStatistic)
        ), 3),
        " Probability: ",
        round(probability, 3)
      ))
      return(newModel)
    }
  }

consecutiveRestart <-
  function(maxConsecutiveSelection = 25, consecutive) {
    if (consecutive == maxConsecutiveSelection) {
      currentModel <- bestModel
      consecutive <- 0
    }
  }

checkModels <- function(currentModel, fitStatistic, maximize = maximize, bestFit = bestFit, bestModel) {
  if (is.null(currentModel)|!inherits(currentModel, "modelCheck")) {
    return(bestModel)
  }
  currentFit <- fitWarningCheck(
    lavaan::fitmeasures(object = currentModel@model.output, fit.measures = fitStatistic),
    maximize
  )
  if (is.na(currentFit)) {
    return(bestModel)
  }
  if (maximize == TRUE) {
    if (currentFit > bestFit) {
      bestModel <- currentModel
    } else {
      bestModel <- bestModel
    }
  } else {
    if (currentFit < bestFit) {
      bestModel <- currentModel
    } else {
      if (currentFit < bestFit) {
        bestModel <- currentModel
      } else {
        bestModel <- bestModel
      }
    }
  }

  return(bestModel)
}

modelWarningCheck <- function(expr, modelSyntax) {
  warn <- err <- c('none')
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
  
  new(
    'modelCheck',
    "model.output" = value,
    "warnings" = as.character(unlist(warn)),
    "errors" = as.character(unlist(err)),
    'model.syntax' = modelSyntax
  )
}


syntaxExtraction <- function(initialModelSyntaxFile, items) {

  # extract the latent factor syntax
  factors <- unique(lavaan::lavaanify(initialModelSyntaxFile)[lavaan::lavaanify(initialModelSyntaxFile)$op ==
    "=~", "lhs"])
  vectorModelSyntax <- stringr::str_split(
    string = initialModelSyntaxFile,
    pattern = "\\n",
    simplify = TRUE
  )
  factorSyntax <- c()
  itemSyntax <- c()
  for (i in 1:length(factors)) {
    chosenFactorLocation <- c(1:length(vectorModelSyntax))[grepl(x = vectorModelSyntax, pattern = paste0(factors[i], "[ ]{0,}=~ "))]
    factorSyntax[i] <- vectorModelSyntax[chosenFactorLocation]
    # remove the factors from the syntax
    itemSyntax[i] <-
      gsub(
        pattern = paste(factors[i], "=~ "),
        replacement = "",
        x = factorSyntax[i]
      )
  }

  # extract the items for each factor
  itemsPerFactor <- stringr::str_extract_all(
    string = itemSyntax,
    pattern = paste0("(\\b", paste0(
      paste0(unlist(items), collapse = "\\b)|(\\b"), "\\b)"
    ))
  )
  return(list("factors" = factors, "itemsPerFactor" = itemsPerFactor))
}

fitWarningCheck <-
  function(expr, maximize) {
    value <- withCallingHandlers(tryCatch(
      expr,
      error = function(e) {
        return(NA)
        invokeRestart("muffleWarning")
      }
    ))
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
    mapply(assign,
           measures,
           0,
           MoreArgs = list(envir = tempEnv))
    
    checkIfEval <-
      tryCatch(
        expr = eval(parse(text = test),
                    envir = tempEnv),
        error = function(e) {
          stop(
            "There was a problem with the fit.statistics.test provided.
            It cannot be evaluated properly.
            Please read the function documentation to see how to properly specify a test."
          )
        }
      )
    
    if (!is.character(test)&is.logical(test)) {
      stop(
        "There is a problem with the fit.statistics.test provided.
        The fit.statistics.test was given as a logical, not a character.
        Please read the function documentation to see how to properly specify a test."
      )
    } else if (!is.character(test)) {
      stop(
        "There is a problem with the fit.statistics.test provided.
        The fit.statistics.test was not given as a character object.
        Please read the function documentation to see how to properly specify a test."
      )
    }
    checkIfEval
  }

# allArgs ####
# allArgs <- function(orig_values = FALSE) {
#   # source: https://stackoverflow.com/a/47955845
#   # get formals for parent function
#   parent_formals <- formals(sys.function(sys.parent(n = 1)))
#   
#   # Get names of implied arguments
#   fnames <- names(parent_formals)
#   
#   # Remove '...' from list of parameter names if it exists
#   fnames <- fnames[-which(fnames == '...')]
#   
#   # Get currently set values for named variables in the parent frame
#   args <- evalq(as.list(environment()), envir = parent.frame())
#   
#   # Get the list of variables defined in '...'
#   args <- c(args[fnames], evalq(list(...), envir = parent.frame()))
#   
#   
#   if(orig_values) {
#     # get default values
#     defargs <- as.list(parent_formals)
#     defargs <- defargs[unlist(lapply(defargs, FUN = function(x) inherits(x, "name")))]
#     args[names(defargs)] <- defargs
#     setargs <- evalq(as.list(match.call())[-1], envir = parent.frame())
#     args[names(setargs)] <- setargs
#   }
#   return(args)
# }

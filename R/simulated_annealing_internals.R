

randomNeighborSelection <- function(currentModelObject = currentModel, numChanges = numChanges, data) {
  
  if (class(currentModelObject) == "list") {
    currentModelObject = currentModelObject[[1]]
  }
  
  # using lavaan functions, construct a full parameter table
  paramTable <- lavaan::parTable(currentModelObject)
  # fullParamTable <- lavaan:::lav_partable_full(paramTable)
  # currentModelParams <- lavaan::lav_partable_merge(paramTable, fullParamTable, remove.duplicated = TRUE, warn = FALSE)
  
  # select the rows that correspond to parameters related to the latent variables
  latentVariables <- row.names(lavaan::inspect(currentModelObject, "cor.lv"))
  currentModelParamsLV <- paramTable[paramTable$lhs %in% latentVariables,]
  
  # randomly select rows to make changes to
  randomChangesRows <- sample(currentModelParamsLV$id, size = numChanges)
  changeParamTable <- paramTable[randomChangesRows,]
  
  # make the changes. If currently free, fix to 0; if fixed to 0, set to free
  paramTable$free[randomChangesRows] <- 1 - paramTable$free[randomChangesRows]
  
  # remove the starting value, estimates, and standard errors of the currentModel
  paramTable$est <- NULL
  paramTable$se <- NULL
  paramTable$start <- NULL
  paramTable$labels <- NULL
  
  # refit the model
  prevModel <- as.list(currentModelObject@call)
  prevModel$model <- paramTable
  # randomNeighborModel <- try(do.call(eval(parse(text = "lavaan::lavaan")), prevModel[-1]), silent = TRUE)
  
  randomNeighborModel <- modelWarningCheck(
    lavaan::lavaan(
      model = prevModel$model,
      data = data
    )
  )
  
  return(randomNeighborModel)
}


selectionFunction <- function(currentModelObject = currentModel, randomNeighborModel, currentTemp, maximize, fitStatistic, consecutive){
  
  # check if the randomNeighborModel is a valid model for use
  if (length(randomNeighborModel[[2]]) > 0 | length(randomNeighborModel[[2]]) > 0) {
    return(currentModelObject)
  } 
  
  # check that the current model isn't null
  if (is.null(currentModelObject[[1]])) {
    return(randomNeighborModel)
  }
  
  # this is the Kirkpatrick et al. method of selecting between currentModel and randomNeighborModel
  if (goal(randomNeighborModel[[1]], fitStatistic, maximize) < goal(currentModelObject[[1]], fitStatistic, maximize)) {
    
    consecutive = consecutive + 1
    return(randomNeighborModel)
    
  } else {
    
    probability = exp(-(goal(randomNeighborModel[[1]], fitStatistic, maximize) - goal(currentModelObject[[1]], fitStatistic, maximize)) / currentTemp)
    
    if (probability > runif(1)) {
      newModel = randomNeighborModel
    } else { 
      newModel = currentModelObject
    }
    
    consecutive = ifelse(identical(lavaan::parameterTable(newModel[[1]]), 
                                   lavaan::parameterTable(currentModelObject[[1]])), 
                         consecutive + 1, 0)
    return(newModel)
  }
}

goal <- function(x, fitStatistic = 'cfi', maximize) {
  
  # if using lavaan and a singular fit statistic,
  if (class(x) == "lavaan" & is.character(fitStatistic) & length(fitStatistic) == 1) {
    energy <- lavaan::fitMeasures(x, fit.measures = fitStatistic)
    
    # if trying to maximize a value, return its negative
    if (maximize == TRUE) {
      return(-energy)
    } else {
      # if minimizing a value, return the value
      return(energy)
    }
  }
}

consecutiveRestart <- function(maxConsecutiveSelection = 25, consecutive){
  if (consecutive == maxConsecutiveSelection) {
    currentModel = bestModel
    consecutive = 0
  }
}

linearTemperature <- function(currentStep, maxSteps) {
  currentTemp <- (maxSteps - (currentStep))/maxSteps
}

quadraticTemperature <- function(currentStep, maxSteps) {
  currentTemp <- ((maxSteps - (currentStep))/maxSteps)^2
}

logisticTemperature <- function(currentStep, maxSteps) {
  x = 1:maxSteps
  x.new = scale(x, center = T, scale = maxSteps/12)
  currentTemp <- 1/(1 + exp((x.new[(currentStep)])))
}

checkModels <- function(currentModel, fitStatistic, maximize = maximize, bestFit = bestFit, bestModel) {
  if (class(currentModel) == "list") {
    currentModel <- currentModel[[1]]
  }
  if (is.null(currentModel)) {
    return(bestModel)
  }
  currentFit = lavaan::fitmeasures(object = currentModel, fit.measures = fitStatistic)
  if (maximize == TRUE) {
    if (currentFit > bestFit) {
      bestModel = currentModel
    } else {
      bestModel = bestModel
    }
  } else {
    if (currentFit < bestFit) {
      bestModel = currentModel
    } else {
      bestModel = bestModel
    }
  }
  
  return(bestModel)
}

modelWarningCheck <- function(expr) {
  warn <- err <- c()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      err <<- append(err, regmatches(paste(e), gregexpr("ERROR: [A-z ]{1,}", paste(e))))
      NULL
    }), warning = function(w) {
      warn <<- append(warn, regmatches(paste(w), gregexpr("WARNING: [A-z ]{1,}", paste(w))))
      invokeRestart("muffleWarning")
    })
  list('lavaan.output' = value, 'warnings' <- as.character(unlist(warn)), 'errors' <- as.character(unlist(err)))
}


randomNeighborSelectionShortForm <- function(currentModelObject = currentModel, numChanges = numChanges, allItems, data, bifactor = FALSE) {
  
  # take the model syntax from the currentModelObject
  internalModelObject = currentModelObject$model.syntax
  
  # extract the latent factor syntax
  factors = unique(lavaan::lavaanify(internalModelObject)[lavaan::lavaanify(internalModelObject)$op=="=~", 'lhs'])
  vectorModelSyntax = stringr::str_split(string = internalModelObject, pattern = '\\n', simplify = TRUE)
  factorSyntax = c()
  itemSyntax = c()
  for (i in 1:length(factors)){
    factorSyntax[i] = vectorModelSyntax[grepl(x = vectorModelSyntax, pattern = paste0(factors[i], " =~ "))]
    # remove the factors from the syntax
    itemSyntax[i] <- gsub(pattern = paste(factors[i], "=~ "), replacement = "", x = factorSyntax[i])
  }
  
  # extract the items for each factor
  itemsPerFactor = stringr::str_match_all(string = itemSyntax,
                                          pattern = paste0("(\\b", paste0(allItems, collapse = "\\b)|(\\b"), "\\b)"))
  # randomly select current items to replace
  replacePattern = paste0("\\b",
                          allItems,
                          collapse = "\\b|\\b")
  currentItems = unique(unlist(
    stringr::str_match_all(string = internalModelObject,
                           pattern = replacePattern
    )))
  currentItems = currentItems[currentItems %in% allItems]
  changingItems = sample(currentItems, numChanges)
  newItems = sample(allItems[!(allItems %in% currentItems)], numChanges)
  changingItemsPattern = paste0("\\b", changingItems, "\\b" )
  
  # make the changes
  for (i in 1:length(newItems)){
    internalModelObject = 
      stringr::str_replace(pattern = changingItemsPattern[i],
                           replacement = newItems[i],
                           string = internalModelObject)
  }
  if (bifactor == TRUE){
    # if bifactor == TRUE, fix the items so the newItems all load on the bifactor
    # assumes that the bifactor latent variable is the last one
    newItemsPerFactor[[length(itemsPerFactor)]] = unlist(newItemsPerFactor[1:(length(itemsPerFactor) - 1)])
  }
  
  # create the new model syntax
  newModelSyntax = vectorModelSyntax
  for (i in 1:length(factors)) {
    newModelSyntax[grepl(pattern =
                           paste0(factors[i], " =~ "), x = newModelSyntax)] <- gsub(
                             pattern = "(?<=(=~ ))[A-z0-9 \\+]{1,}",
                             replacement = paste0(newItemsPerFactor[[i]], collapse = " + "),
                             x = vectorModelSyntax[grepl(pattern =
                                                           paste0(factors[i], " =~ "), x = vectorModelSyntax)],
                             perl = TRUE
                           )
  }
  newModelSyntax = stringr::str_flatten(newModelSyntax, collapse = "\n")
  
  # refit the model with new items
  randomNeighborModel <- modelWarningCheck(
    lavaan::lavaan(
      model = internalModelObject, data = originalData,
      model.type = simulatedAnnealing.env$model.type,
      auto.var = simulatedAnnealing.env$auto.var,
      ordered = simulatedAnnealing.env$ordered,
      estimator = simulatedAnnealing.env$estimator,
      int.ov.free = simulatedAnnealing.env$int.ov.free,
      int.lv.free = simulatedAnnealing.env$int.lv.free,
      auto.fix.first = simulatedAnnealing.env$auto.fix.first,
      std.lv = simulatedAnnealing.env$std.lv,
      auto.fix.single = simulatedAnnealing.env$auto.fix.single,
      auto.cov.lv.x = simulatedAnnealing.env$auto.cov.lv.x,
      auto.th = simulatedAnnealing.env$auto.th,
      auto.delta = simulatedAnnealing.env$auto.delta,
      auto.cov.y = simulatedAnnealing.env$auto.cov.y)
  )
  
  randomNeighborModel$model.syntax = internalModelObject
  
  return(randomNeighborModel)
  
}

randomInitialModel <- function(initialModelSyntax, maxItems, data, allItems = items, bifactor = FALSE) {
  
  if (!is.character(initialModelSyntax)) {
    stop("Please input the initial model as a character string in the lavaan model.syntax format.")
  }
  
  # extract the latent factor syntax
  factors = unique(lavaan::lavaanify(initialModelSyntax)[lavaan::lavaanify(initialModelSyntax)$op=="=~", 'lhs'])
  vectorModelSyntax = stringr::str_split(string = initialModelSyntax, pattern = '\\n', simplify = TRUE)
  factorSyntax = c()
  itemSyntax = c()
  for (i in 1:length(factors)){
  factorSyntax[i] = vectorModelSyntax[grepl(x = vectorModelSyntax, pattern = paste0(factors[i], " =~ "))]
  # remove the factors from the syntax
  itemSyntax[i] <- gsub(pattern = paste(factors[i], "=~ "), replacement = "", x = factorSyntax[i])
  }
  
  # extract the items for each factor
  itemsPerFactor = stringr::str_match_all(string = itemSyntax,
                                          pattern = paste0("(\\b", paste0(allItems, collapse = "\\b)|(\\b"), "\\b)"))
  # reduce the number of items for each factor according to maxItems
  newItemsPerFactor = list()
  for (i in 1:length(itemsPerFactor)) {
    newItemsPerFactor[[i]] = sample(x = unique(na.omit(unlist(itemsPerFactor[i]))), size = unlist(maxItems[i]))
  }
  
  if (bifactor == TRUE){
    # if bifactor == TRUE, fix the items so the newItems all load on the bifactor
    # assumes that the bifactor latent variable is the last one
    newItemsPerFactor[[length(itemsPerFactor)]] = unlist(newItemsPerFactor[1:(length(itemsPerFactor) - 1)])
  }
  
  # create the new model syntax
  newModelSyntax = vectorModelSyntax
  for (i in 1:length(factors)) {
    newModelSyntax[grepl(pattern =
                           paste0(factors[i], " =~ "), x = newModelSyntax)] <- gsub(
                             pattern = "(?<=(=~ ))[A-z0-9 \\+]{1,}",
                             replacement = paste0(newItemsPerFactor[[i]], collapse = " + "),
                             x = vectorModelSyntax[grepl(pattern =
                                                           paste0(factors[i], " =~ "), x = vectorModelSyntax)],
                             perl = TRUE
                           )
  }
  newModelSyntax = stringr::str_flatten(newModelSyntax, collapse = "\n")
  
  # fit the new model
  newModel = modelWarningCheck(lavaan::lavaan(
    model = newModelSyntax, data = originalData,
    model.type = simulatedAnnealing.env$model.type,
    auto.var = simulatedAnnealing.env$auto.var,
    ordered = simulatedAnnealing.env$ordered,
    estimator = simulatedAnnealing.env$estimator,
    int.ov.free = simulatedAnnealing.env$int.ov.free,
    int.lv.free = simulatedAnnealing.env$int.lv.free,
    auto.fix.first = simulatedAnnealing.env$auto.fix.first,
    auto.fix.single = simulatedAnnealing.env$auto.fix.single,
    auto.cov.lv.x = simulatedAnnealing.env$auto.cov.lv.x,
    auto.th = simulatedAnnealing.env$auto.th,
    auto.delta = simulatedAnnealing.env$auto.delta,
    auto.cov.y = simulatedAnnealing.env$auto.cov.y))
  newModel$model.syntax = newModelSyntax
  
  return(newModel)
}

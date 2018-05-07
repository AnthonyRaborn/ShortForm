

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
  if (length(randomNeighborModel[[2]]) > 0 | length(randomNeighborModel[[2]] > 0)) {
    return(currentModel)
  } else {
    randomNeighborModel = randomNeighborModel[[1]]
  }
  
  if (class(currentModelObject) == "list") {
    currentModelObject = currentModelObject[[1]]
  }
  
  
  # this is the Kirkpatrick et al. method of selecting between currentModel and randomNeighborModel
  if (goal(randomNeighborModel, fitStatistic, maximize) < goal(currentModelObject, fitStatistic, maximize)) {
    
    consecutive = consecutive + 1
    return(randomNeighborModel)
    
  } else {
    
    probability = exp(-(goal(randomNeighborModel, fitStatistic, maximize) - goal(currentModelObject, fitStatistic, maximize)) / currentTemp)
    
    if (probability > runif(1)) {
      newModel = randomNeighborModel
    } else { 
      newModel = currentModelObject
    }
    
    consecutive = ifelse(identical(lavaan::parameterTable(newModel), 
                                   lavaan::parameterTable(currentModelObject)), 
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
  currentTemp <- (maxSteps - currentStep)/maxSteps
}

quadraticTemperature <- function(currentStep, maxSteps) {
  currentTemp <- ((maxSteps - currentStep)/maxSteps)^2
}

logisticTemperature <- function(currentStep, maxSteps) {
  x = 1:maxSteps
  x.new = scale(x, center = T, scale = maxSteps/12)
  currentTemp <- 1/(1 + exp((x.new[currentStep])))
}

checkModels <- function(currentModel, fitStatistic, maximize = maximize, bestFit = bestFit, bestModel) {
  if (class(currentModel) == "list") {
    currentModel <- currentModel[[1]]
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
  list(lavaan.output = value, warnings <- as.character(unlist(warn)), errors <- as.character(unlist(err)))
}


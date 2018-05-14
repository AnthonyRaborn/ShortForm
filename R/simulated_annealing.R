#' An adaptation of the simulated annealing algorithm for psychometric models.
#'
#' @description Simulated annealing mimics the physical process of annealing metals together. [Kirkpatrick et al. (1983)](http://science.sciencemag.org/content/220/4598/671) introduces this analogy and demonstrates its use; the implementation here follows this demonstration closely, with some modifications to make it better suited for psychometric models.
#'
#' @details ##### Simulated Annealing Outline ####
#' initialModel - the initial, full form
#' currentModel - the model of the current step
#' maxSteps - the number of steps (iterations)
#' currentStep - the current step
#' currentTemp - the current temperature. A function of the number of steps (such that temp = 0 at maxSteps), and values that control the shape of the overall temperature. Should be modifiable. At the least, needs a maxTemp value and values that control the shape (up to a cubic value, maybe?)
#' randomNeighbor - a function that determines how the form is changed at each step. Probably similar to the Tabu model. Should be able to change one or more parameters
#' goal - a function that determines the "goodness" of the currentModel. Typically in SA goodness is defined as minimization! Sometimes called an energ function
#' selectionFunction - a function that determines if a randomNeighbor change is accepted. Uses the goal function that determines the "goodness" of the currentModel and the "goodness" of the randomNeighbor, and the currentTemp to generate a probability of acceptance, then compares this probability to a U[0,1] variable to determine if accepted or not. A standard version of this is P(goal, goal', currentTemp) = [(1 if goal' better than goal), (exp(-(goal' - goal)/currentTemp) if goal' worse than goal)] (citation: Kirkpatrick et al., 1983). Allowing other functions would be ideal.
#' bestModel - the model with the best value of the goal function achieved so far
#' bestGoal - the best value of the goal function achieved so far
#' restartCriteria - if allowed, this would "restart" the SA process by changing currentModel to bestModel and continuing the process. Could be based on (1) the currentStep value, (2) the difference between goal(currentModel) and goal(bestModel), (3) randomness (i.e., could randomly restart, could randomly restart based on some values, etc), (4) other critera.
#'
#' @param initialModel The initial model as a character vector with lavaan model.syntax.
#' @param originalData The original data frame with variable names.
#' @param maxSteps The number of iterations for which the algorithm will run.
#' @param fitStatistic Either a single model fit statistic produced by lavaan, or a user-defined fit statistic function.
#' @param temperature Either an acceptable character value, or a user-defined temperature function. The acceptable values are "linear", "quadratic", or "logistic".
#' @param maximize Logical indicating if the goal is to maximize (TRUE) the fitStatistic for model selection.
#' @param Kirkpatrick Either TRUE to use Kirkpatrick et al. (1983) acceptance probability, or a user-defined function for accepting proposed models.
#' @param randomNeighbor Either TRUE to use the included function for randomNeighbor selection, or a user-defined function for creating random models.
#' @param lavaan.model.specs A list which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#' @param maxChanges An integer value greater than 1 setting the maximum number of parameters to change within randomNeighbor.
#' @param restartCriteria Either "consecutive" to restart after maxConsecutiveSelection times with the same model chosen in a row, or a user-defined function.
#' @param maximumConsecutive A numeric value used with restartCriteria.
#' @param maxItems When creating a short form, a vector of the number of items per factor you want the short form to contain. Defaults to `NULL`.
#' @param items A character vector of item names. Defaults to `NULL`. Ignored if `maxItems==FALSE`.
#' @param bifactor Logical. Indicates if the latent model is a bifactor model. If `TRUE`, assumes that the last latent variable in the provided model syntax is the bifactor (i.e., all of the retained items will be set to load on the last latent variable). Ignored if `maxItems==FALSE`.
#' @param ... Further arguments to be passed to other functions. Not implemented for any of the included functions.
#'
#' @return A named list: the 'bestModel' found, the 'bestFit', and 'allFit' values found by the algorithm.
#'
#' @examples
#' data(exampleAntModel)
#' data(simulated_test_data)
#' trial1 <- simulatedAnnealing(initialModel = lavaan::cfa(model = exampleAntModel,
#'                                                         data = simulated_test_data),
#'                              originalData = simulated_test_data, maxSteps = 3,
#'                              fitStatistic = 'rmsea', maximize = FALSE)
#' # lavaan::summary(trial1[[1]]) # shows the resulting model
#'
#' trial2 <- simulatedAnnealing(initialModel = exampleAntModel,
#' originalData = simulated_test_data,
#' maxSteps = 5, maxItems = 30, items = paste0("Item", 1:56))
#' # lavaan::summary(trial2[[1]]) # shows the resulting model
#' @import lavaan utils
#' @export

simulatedAnnealing <-
  function(initialModel,
           originalData,
           maxSteps,
           fitStatistic = 'cfi',
           temperature = "linear",
           maximize = TRUE,
           Kirkpatrick = TRUE,
           randomNeighbor = TRUE,
           lavaan.model.specs = list(
             model.type = "cfa",
             auto.var = TRUE,
             estimator = "default",
             ordered = NULL,
             int.ov.free = TRUE,
             int.lv.free = FALSE,
             std.lv = TRUE,
             auto.fix.first = FALSE,
             auto.fix.single = TRUE,
             auto.cov.lv.x = TRUE,
             auto.th = TRUE,
             auto.delta = TRUE,
             auto.cov.y = TRUE
           ),
           maxChanges = 5,
           restartCriteria = "consecutive",
           maximumConsecutive = 25,
           maxItems = NULL,
           items = NULL,
           bifactor = FALSE,
           ...) {
    
    #### initial values ####
    currentModel = NULL
    bestModel = NULL
    currentModel = bestModel = list(initialModel)
    currentStep = 0
    consecutive = 0
    allFit = c()
    
    # creates objects in the function environment that are fed into the lavaan function in order to fine-tune the model to user specifications
    # solution from: https://stackoverflow.com/questions/6375790/r-creating-an-environment-in-the-globalenv-from-inside-a-function
    # makeCache <- function() {
    #   # list(get = function(key) cache[[key]],
    #   #      set = function(key, value) cache[[key]] <- value
    #   #      )
    # }
    
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = environment())
      )
    
    if (!is.null(maxItems)) {
      # if using the short form option
      print("Initializing short form creation.")
      mapply(assign, x = 'allItems', value = list(items), USE.NAMES = FALSE, SIMPLIFY = FALSE, MoreArgs = list(envir = parent.frame()))
      randomInitialModel = function(initialModelSyntax, maxItems, initialData, bifactorModel = bifactor) {
        
        # extract the latent factor syntax
        mapply(assign, c("factors", "itemsPerFactor"), syntaxExtraction(initialModelSyntaxFile = initialModelSyntax), MoreArgs = list(envir = parent.frame()))
        
        # reduce the number of items for each factor according to maxItems
        newItemsPerFactor = list()
        for (i in 1:length(itemsPerFactor)) {
          newItemsPerFactor[[i]] = sample(x = unique(unlist(itemsPerFactor[i])), size = unlist(maxItems[i]))
        }
        
        if (bifactorModel == TRUE){
          # if bifactorModel == TRUE, fix the items so the newItems all load on the bifactor
          # assumes that the bifactor latent variable is the last one
          newItemsPerFactor[[length(itemsPerFactor)]] = unlist(newItemsPerFactor[1:(length(itemsPerFactor) - 1)])
        }
        
        # create the new model syntax
        
        newModelSyntax = c()
        for (i in 1:length(factors)) {
          newModelSyntax[i] = paste(factors[i], "=~",
                                    paste(newItemsPerFactor[[i]], collapse = " + "))
        }
        
        
        # fit the new model
        newModel = modelWarningCheck(
          lavaan::lavaan(
            model = newModelSyntax, data = initialData,
            model.type = model.type,
            auto.var = auto.var,
            ordered = ordered,
            estimator = estimator,
            int.ov.free = int.ov.free,
            int.lv.free = int.lv.free,
            auto.fix.first = auto.fix.first,
            auto.fix.single = auto.fix.single,
            auto.cov.lv.x = auto.cov.lv.x,
            auto.th = auto.th,
            auto.delta = auto.delta,
            auto.cov.y = auto.cov.y)
        )
        newModel$model.syntax = newModelSyntax
        
        return(newModel)
      }
      currentModel = randomInitialModel(initialModel,
                                        maxItems,
                                        initialData = originalData,
                                        bifactorModel = bifactor)
      print(paste("The initial short form is this:", currentModel$model.syntax))
      bestFit = tryCatch(
        lavaan::fitmeasures(object = bestModel, fit.measures = fitStatistic),
        error = function(e, checkMaximize = maximize) {
          if (length(e) > 0) {
            if (checkMaximize == TRUE) {
              return(0)
            } else {
              return(Inf)
            }
          }
        }
      )
      if (is.null(items)) {
          stop(
            "To use this function for short forms, you need to set both the maxItems to consider as well as the names of the items."
          )
      }
      
      print("Using the short form randomNeighbor function.")
      randomNeighbor <- function(currentModelObject = currentModel, numChanges = numChanges, allItems, data, bifactor = FALSE, initialModelSyntax) {
        
        # take the model syntax from the currentModelObject
        internalModelObject = currentModelObject$model.syntax
        
        # extract the latent factor syntax
        randomNeighbor.env <- new.env()
        mapply(assign, c("factors", "currentItems"), syntaxExtraction(initialModelSyntaxFile = internalModelObject), MoreArgs = list(envir = randomNeighbor.env))
        
        # randomly select current items to replace
        
        replacePattern = paste0("\\b",
                                allItems,
                                collapse = "\\b|\\b")
        
        replacementItemPool = c()
        for (i in 1:length(randomNeighbor.env$factors)) {
          if (class(allItems)=="list"){
          replacementItemPool[[i]] = allItems[[i]][!(allItems[[i]] %in% randomNeighbor.env$currentItems[[i]])]
          } else {
            replacementItemPool[[i]] = allItems[!(allItems %in% randomNeighbor.env$currentItems[[i]])]
            
          }
        }
        print(replacementItemPool)
        changingItems = c(); replacementItem = c()
        for (i in 1:numChanges){
          # randomly select factor to have an item changed
          if (bifactor) {
            currentFactor = sample(1:(length(randomNeighbor.env$factors)-1), 1)
          } else {
            currentFactor = sample(1:length(randomNeighbor.env$factors), 1)
          }
          # randomly select the item to be changed
          changingItemTemp = c()
          changingItemTemp = sample(randomNeighbor.env$currentItems[[currentFactor]], 1)
          while (changingItemTemp %in% changingItems||length(changingItemTemp %in% changingItems)==0) {
            changingItemTemp = sample(randomNeighbor.env$currentItems[[currentFactor]], 1)
          }
          changingItems = c(changingItems, changingItemTemp)
          print(paste("The items being changed are:" , paste(changingItems, collapse = " ")))
          # Sample an item from the items in the item pool
          tempReplacementItems = sample(replacementItemPool[[currentFactor]], 1)
          while (tempReplacementItems %in% replacementItem) {
            tempReplacementItems = sample(replacementItemPool[[currentFactor]], 1)
          }
          replacementItem = c(replacementItem, tempReplacementItems)
          print(paste("The new items are:", replacementItem))
        }
        
        for (i in 1:length(randomNeighbor.env$factors)) {
          for (j in 1:numChanges){
        randomNeighbor.env$currentItems[[i]] = 
          gsub(pattern = paste0("\\b", changingItems[j], "\\b"), replacement = replacementItem[j], x = randomNeighbor.env$currentItems[[i]])
        }
        }
        
        if (bifactor == TRUE){
          print("Bifactor model detected.")
          # if bifactor == TRUE, fix the items so the newItems all load on the bifactor
          # assumes that the bifactor latent variable is the last one
          randomNeighbor.env$currentItems[[length(itemsPerFactor)]] = unlist(randomNeighbor.env$currentItems[1:(length(itemsPerFactor) - 1)])
        }
        
        # create the new model syntax
        newModelSyntax = c()
        for (i in 1:length(randomNeighbor.env$factors)) {
          newModelSyntax[i] = paste(randomNeighbor.env$factors[i], "=~", 
                                    paste(randomNeighbor.env$currentItems[[i]], collapse = " + "))
        }
        
        newModelSyntax = stringr::str_flatten(newModelSyntax, collapse = "\n")
        
        # refit the model with new items
        randomNeighborModel <- modelWarningCheck(
          lavaan::lavaan(
            model = newModelSyntax, data = originalData,
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
            auto.cov.y = auto.cov.y)
        )
        
        randomNeighborModel$model.syntax = newModelSyntax
        print(currentModel$model.syntax)
        print(newModelSyntax)
        return(randomNeighborModel)
        
      }
       print("Finished initializing short form options.")
    } else {
      # if not using the short form option
      bestFit = tryCatch(
        lavaan::fitmeasures(object = bestModel, fit.measures = fitStatistic),
        error = function(e, checkMaximize = maximize) {
          if (length(e) > 0) {
            if (checkMaximize == TRUE) {
              return(0)
            } else {
              return(Inf)
            }
          }
        }
      )
      randomNeighbor <- function(currentModelObject = currentModel, numChanges = numChanges, data) {
        
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
    }
    
    
    #### selecting functions for use in algorithm ####
    if (temperature == "linear") {
      temperatureFunction <- linearTemperature
    } else if (temperature == "quadratic") {
      temperatureFunction <- quadraticTemperature
    } else if (temperature == "logistic") {
      temperatureFunction <- logisticTemperature
    } else if (class(temperature == "function")) {
      temperatureFunction <- temperature
    } else {
      stop(
        "You need to specify an appropriate default temperature (one of \"linear\", \"quadratic\", or \"logistic\") or include a temperature in the form of a function to continue."
      )
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
      
    
    if (restartCriteria == "consecutive") {
      restartCriteria <- consecutiveRestart
    } else if (class(restartCriteria) == "function") {
      restartCriteria = restartCriteria
    } else {
      restartCriteria = function() {
      }
      warning(
        "The restart criteria should to be either \"consecutive\" (the default) or a custom function. It has been set to NULL so the algorithm will not restart at all."
      )
    }
    
    #### perform algorithm ####
    
    print("Current Progress:")
    trackStep = txtProgressBar(
      min = 0,
      max = maxSteps - 1,
      initial = 1,
      style = 3
    )
    
    
    while (currentStep < maxSteps) {
      setTxtProgressBar(trackStep, currentStep)
      
      # how many changes to make?
      numChanges = sample(1:maxChanges, size = 1)
      # generate random model
      if (is.null(maxItems)) {
        randomNeighborModel = randomNeighbor(
          currentModelObject = currentModel,
          numChanges = numChanges,
          data = originalData
        )
      } else {
        randomNeighborModel = randomNeighbor(
          currentModelObject = currentModel,
          numChanges = numChanges,
          data = originalData,
          allItems = allItems,
          bifactor
        )
        
      }
      # select between random model and current model
      currentModel = selectionFunction(
        currentModel = currentModel,
        randomNeighborModel = randomNeighborModel,
        currentTemp = temperatureFunction(currentStep, maxSteps),
        maximize = maximize,
        fitStatistic = fitStatistic,
        consecutive = consecutive
      )
      # recored fit
      allFit[currentStep + 1] =  tryCatch(
        lavaan::fitmeasures(object = currentModel[[1]], fit.measures = fitStatistic),
        error = function(e) {
          if (length(e) > 0) {
            bestFit
          }
        }
      )
      # check for current best model
      bestModel = checkModels(currentModel, fitStatistic, maximize, bestFit, bestModel)
      bestFit = tryCatch(
        lavaan::fitmeasures(object = bestModel, fit.measures = fitStatistic),
        error = function(e) {
          if (length(e) > 0) {
            bestFit
          }
        }
      )
      # restart if the same model was chosen too many times
      restartCriteria(maxConsecutiveSelection = maximumConsecutive,
                      consecutive = consecutive)
      currentStep = currentStep + 1
      print(lavaan::summary(currentModel[[1]]))
    }
    
    setTxtProgressBar(trackStep, maxSteps)
    return(list(
      bestModel = bestModel,
      bestFit = bestFit,
      allFit = allFit
    ))
  }

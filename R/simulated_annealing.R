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
#' @param initialModel The initial model. Can either be a lavaan model object or a character vector with lavaan model.syntax.
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
    
    # creates objects in the global environment that are fed into the lavaan function in order to fine-tune the model to user specifications
    simulatedAnnealing.env <- new.env(parent = baseenv())
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = simulatedAnnealing.env)
    )
    
    
    if (!is.null(maxItems)) {
      currentModel = randomInitialModel(initialModel,
                                        maxItems,
                                        data = originalData,
                                        allItems = items)
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
        if (length(colnames(data)) > 0) {
          items = colnames(data)
        } else {
          stop(
            "To use this function for short forms, you need to set both the maxItems to consider as well as the names of the items."
          )
        }
      }
      if (randomNeighbor == TRUE) {
        randomNeighbor <- randomNeighborSelectionShortForm
      } else if (class(randomNeighbor == "function")) {
        randomNeighbor <- randomNeighbor
      } else{
        stop(
          "You need to specify a random neighbor function, or set randomNeighbor=TRUE to use the standard random neighbor function."
        )
      }
    } else {
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
      
      if (randomNeighbor == TRUE) {
        randomNeighbor <- randomNeighborSelection
      } else if (class(randomNeighbor == "function")) {
        randomNeighbor <- randomNeighbor
      } else{
        stop(
          "You need to specify a random neighbor function, or set randomNeighbor=TRUE to use the standard random neighbor function."
        )
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
    
    if (Kirkpatrick == TRUE) {
      selectionFunction <- selectionFunction
    } else if (class(Kirkpatrick == "function")) {
      selectionFunction <- Kirkpatrick
    } else {
      stop(
        "You need to specify a selection function, or set Kirkpatrick = TRUE to use the standard selection function."
      )
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
      if (is.null(items)) {
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
          allItems = items
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
        lavaan::fitmeasures(object = currentModel, fit.measures = fitStatistic),
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
    }
    
    setTxtProgressBar(trackStep, maxSteps)
    return(list(
      bestModel = bestModel,
      bestFit = bestFit,
      allFit = allFit
    ))
  }

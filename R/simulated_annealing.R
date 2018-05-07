##### Simulated Annealing Outline ####

# initialModel - the initial, full form
# currentModel - the model of the current step
# maxSteps - the number of steps (iterations)
# currentStep - the current step
# currentTemp - the current temperature. A function of the number of steps (such that temp = 0 at maxSteps), and values that control the shape of the overall temperature. Should be modifiable. At the least, needs a maxTemp value and values that control the shape (up to a cubic value, maybe?)
# randomNeighbor - a function that determines how the form is changed at each step. Probably similar to the Tabu model. Should be able to change one or more parameters
# goal - a function that determines the "goodness" of the currentModel. Typically in SA goodness is defined as minimization! Sometimes called an energ function
# selectionFunction - a function that determines if a randomNeighbor change is accepted. Uses the goal function that determines the "goodness" of the currentModel and the "goodness" of the randomNeighbor, and the currentTemp to generate a probability of acceptance, then compares this probability to a U[0,1] variable to determine if accepted or not. A standard version of this is P(goal, goal', currentTemp) = [(1 if goal' better than goal), (exp(-(goal' - goal)/currentTemp) if goal' worse than goal)] (citation: Kirkpatrick et al., 1983). Allowing other functions would be ideal.
# bestModel - the model with the best value of the goal function achieved so far
# bestGoal - the best value of the goal function achieved so far
# restartCriteria - if allowed, this would "restart" the SA process by changing currentModel to bestModel and continuing the process. Could be based on (1) the currentStep value, (2) the difference between goal(currentModel) and goal(bestModel), (3) randomness (i.e., could randomly restart, could randomly restart based on some values, etc), (4) other critera.

simulatedAnnealing <- function(initialModel, originalData, maxSteps, fitStatistic = 'cfi', temperature = "linear", maximize = TRUE, Kirkpatrick = TRUE, randomNeighbor = TRUE, maxChanges = 5, restartCriteria = "consecutive", ...){
  
  #### initial values ####
  currentModel = bestModel = initialModel
  currentStep = 0
  consecutive = 0
  bestFit = 0


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
  
  if (randomNeighbor == TRUE) {
    randomNeighbor <- randomNeighborSelection
  } else if (class(randomNeighbor == "function")) {
    randomNeighbor <- randomNeighbor
  } else{
    stop("You need to specify a random neighbor function, or set randomNeighbor=TRUE to use the standard random neighbor function.")
  }
  
  if (restartCriteria == "consecutive") {
    restartCriteria <- consecutiveRestart
  } else if (class(restartCriteria) == "function") {
    restartCriteria = restartCriteria
  } else {
    restartCriteria = function() {}
    warning(
      "The restart criteria should to be either \"consecutive\" (the default) or a custom function. It has been set to NULL so the algorithm will not restart at all."
    )
  }
  
  #### perform algorithm ####
  
  print("Current Progress:")
  trackStep = txtProgressBar(min = 0, max = maxSteps - 1, initial = 1, style = 3)

  
  while (currentStep < maxSteps) {
    
    setTxtProgressBar(trackStep, currentStep)
    
    # how many changes to make?
    numChanges = sample(1:maxChanges, size = 1)
    # generate random model
    randomNeighborModel = randomNeighbor(currentModelObject = currentModel, numChanges = numChanges, data = originalData)
    # select between random model and current model
    currentModel = selectionFunction(currentModel = currentModel, randomNeighborModel = randomNeighborModel, currentTemp = temperatureFunction(currentStep, maxSteps), maximize = maximize, fitStatistic = fitStatistic, consecutive = consecutive)
    # check for current best model
    bestModel = checkModels(currentModel, fitStatistic, maximize, bestFit, bestModel)
    bestFit = lavaan::fitmeasures(object = bestModel, fit.measures = fitStatistic)
    # restart if the same model was chosen too many times
    restartCriteria(consecutive = consecutive)
    currentStep = currentStep + 1
  }
  
  setTxtProgressBar(trackStep, maxSteps)
  return(list(bestModel, bestFit))
  }

data(exampleAntModel)
data(simulated_test_data)
trial1 <- simulatedAnnealing(initialModel = lavaan::cfa(model = exampleAntModel, 
                                                       data = simulated_test_data),
                            originalData = simulated_test_data, maxSteps = 10,
                            fitStatistic = 'rmsea', maximize = FALSE)
lavaan::summary(trial1[[1]])

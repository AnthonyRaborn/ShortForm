#' An adaptation of the simulated annealing algorithm for psychometric models.
#'
#' @description Simulated annealing mimics the physical process of annealing metals together. [Kirkpatrick et al. (1983)](http://science.sciencemag.org/content/220/4598/671) introduces this analogy and demonstrates its use; the implementation here follows this demonstration closely, with some modifications to make it better suited for psychometric models.
#'
#' @details \strong{Outline of the Pieces of the Simulated Annealing Algorithm}
#' * initialModel -- the initial, full form
#' * currentModel -- the model of the current step
#' * maxSteps -- the maximum number of steps (iterations)
#' * currentStep -- the current step
#' * currentTemp -- the current temperature. A function of the number of steps (such that temp = 0 at maxSteps), and values that control the shape of the overall temperature. A part of the function that determines the acceptance probability of newly -- generated models
#' * randomNeighbor -- a function that determines how the form is changed at each step. Should be able to change one or more parameters, and should have a way to control how many are changed.
#' * goal -- a function that determines the "goodness" of the currentModel. Typically in SA goodness is defined as minimization! Sometimes called an energy function
#' * selectionFunction -- a function that determines if a randomNeighbor change is accepted. Uses the goal function that determines the "goodness" of the currentModel and the "goodness" of the randomNeighbor, and the currentTemp to generate a probability of acceptance, then compares this probability to a Uniform(0,1) variable to determine if accepted or not. A standard version of this is:
#' ![](SA-goal.jpg)
#' (Kirkpatrick et al., 1983)
#' * bestModel -- the model with the best value of the goal function achieved so far
#' * bestGoal -- the best value of the goal function achieved so far
#' * restartCriteria -- if utilized, this would "restart" the SA process by changing currentModel to bestModel and continuing the process. Could be based on (1) the currentStep value, (2) the difference between goal(currentModel) and goal(bestModel), (3) randomness (i.e., could randomly restart, could randomly restart based on some values, etc), (4) other critera.
#'
#' @param initialModel The initial model as a `character` vector with lavaan model.syntax.
#' @param originalData The original `data.frame` with variable names.
#' @param maxSteps The number of iterations for which the algorithm will run.
#' @param fitStatistic Either a single model fit statistic produced by lavaan, or a user-defined fit statistic function.
#' @param temperature Either an acceptable `character` value, or a user-defined temperature function. The acceptable values are "linear", "quadratic", or "logistic".
#' @param maximize Logical indicating if the goal is to maximize (`TRUE`) the fitStatistic for model selection.
#' @param Kirkpatrick Either `TRUE` to use Kirkpatrick et al. (1983) acceptance probability, or a user-defined function for accepting proposed models.
#' @param randomNeighbor Either `TRUE` to use the included function for randomNeighbor selection, or a user-defined function for creating random models.
#' @param lavaan.model.specs A `list` which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#' @param maxChanges An `integer` value greater than 1 setting the maximum number of parameters to change within randomNeighbor. When creating a short form, should be no greater than the smallest reduction in items loading on one factor; e.g., when reducing a 2-factor scale from 10 items on each factor to 8 items on the first and 6 items on the second, maxChanges should be no greater than 2.
#' @param restartCriteria Either "consecutive" to restart after maxConsecutiveSelection times with the same model chosen in a row, or a user-defined function.
#' @param maximumConsecutive A positive `integer` value used with restartCriteria.
#' @param maxItems When creating a short form, a `vector` of the number of items per factor you want the short form to contain. Defaults to `NULL`.
#' @param items A `character` vector of item names. Defaults to `NULL`. Ignored if `maxItems==FALSE`.
#' @param bifactor Logical. Indicates if the latent model is a bifactor model. If `TRUE`, assumes that the last latent variable in the provided model syntax is the bifactor (i.e., all of the retained items will be set to load on the last latent variable). Ignored if `maxItems==FALSE`.
#' @param setChains Numeric. Sets the number of parallel chains to run. Default to `1`, which also sets the algorithm to run serially (e.g., on a single processor). Values greater than `1` result in the chains running on parallel processes using the `doSNOW` and `foreach` packages.
#' @param ... Further arguments to be passed to other functions. Not implemented for any of the included functions.
#'
#' @return A named list: the 'bestModel' found, the 'bestFit', and 'allFit' values found by the algorithm.
#'
#' @examples
#' \dontrun{
#' data(exampleAntModel)
#' data(simulated_test_data)
#' trial1 <- simulatedAnnealing(
#'   initialModel = lavaan::cfa(
#'     model = exampleAntModel,
#'     data = simulated_test_data
#'   ),
#'   originalData = simulated_test_data, maxSteps = 3,
#'   fitStatistic = "rmsea", maximize = FALSE
#' )
#' summary(trial1) # shows the resulting model
#'
#' trial2 <- simulatedAnnealing(
#'   initialModel = exampleAntModel,
#'   originalData = simulated_test_data,
#'   maxSteps = 2, maxItems = 30, items = paste0("Item", 1:56)
#' )
#' summary(trial2) # shows the resulting model
#' }
#' @import lavaan utils
#' @export
#' @md

simulatedAnnealing <-
  function(initialModel,
           originalData,
           maxSteps,
           fitStatistic = "cfi",
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
           setChains = 1,
           ...) {
    #### initial values ####
    if(!exists('originalData')) {
      stop("Please check that you have included the original data frame!")
    }
    if(!is.numeric(setChains)|setChains<1) {
      warning("The value for setChains was not valid. Defaulting to setChains=1.")
      setChains = 1
    }
    currentStep <- 1
    consecutive <- 0
    allFit <- c()

    # creates objects in the function environment that are fed into the lavaan function in order to fine-tune the model to user specifications
    # solution from: https://stackoverflow.com/questions/6375790/r-creating-an-environment-in-the-globalenv-from-inside-a-function
    # makeCache <- function() {
    #   # list(get = function(key) cache[[key]],
    #   #      set = function(key, value) cache[[key]] <- value
    #   #      )
    # }
    checkModelSpecs(lavaan.model.specs)
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = environment())
    )

    if (!is.null(maxItems)) {
      # if using the short form option
      cat("Initializing short form creation.")
      mapply(
        assign,
        x = "allItems",
        value = list(items),
        USE.NAMES = FALSE,
        SIMPLIFY = FALSE,
        MoreArgs = list(envir = environment())
      )
      randomInitialModel <- function(init.model = initialModel,
                                     maxItems,
                                     initialData = originalData,
                                     bifactorModel = bifactor) {
        # extract the latent factor syntax
        mapply(
          assign,
          c("factors", "itemsPerFactor"),
          syntaxExtraction(initialModelSyntaxFile = initialModel, items = allItems),
          MoreArgs = list(envir = environment())
        )

        # save the external relationships
        vectorModel <- unlist(strsplit(x = initialModel, split = "\\n"))
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
          paste(newModelSyntax, externalRelation, factorRelation, sep = "\n")
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
        # newModel$model.syntax <- newModelSyntax

        return(newModel)
      }
      currentModel <- bestModel <- randomInitialModel(initialModel,
        maxItems,
        initialData = originalData,
        bifactorModel = bifactor
      )
      cat(paste(
        "\nThe initial short form is:\n",
        paste(currentModel@model.syntax, collapse = "\n")
      ))
      bestFit <- tryCatch(
        lavaan::fitmeasures(object = bestModel@model.output, fit.measures = fitStatistic),
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

      cat("\nUsing the short form randomNeighbor function.")
      cat("\nFinished initializing short form options.")
    } else {
      # if not using the short form option
      bestFit <- tryCatch(
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

    if (class(restartCriteria) == "function") {
      restartCriteria <- restartCriteria
    } else if (restartCriteria == "consecutive") {
      restartCriteria <- consecutiveRestart
    } else {
      restartCriteria <- function(maxConsecutiveSelection, consecutive) {
        
      }
      warning(
        "The restart criteria should to be either \"consecutive\" (the default) or a custom function. It has been set to NULL so the algorithm will not restart at all."
      )
    }
    #### prepare parallel processing ####
    if (setChains > 1) {
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      progressPar <- function(n) {
        cat(paste("Chain number ", n, " complete. \n", sep = ""))
      }
      
      if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        num_workers <- 2L
      } else {
        if (setChains <= parallel::detectCores()) {
          num_workers <- setChains
        } else { 
          # use all cores in devtools::test()
          num_workers <- parallel::detectCores()
          }
      }
      cl <- parallel::makeCluster(num_workers,type="PSOCK", outfile = "")
      doSNOW::registerDoSNOW(cl)
      
    } else {
      num_workers <- 1L
      # pb <- txtProgressBar(max = maxSteps, style = 3)
      # progress <- function(n) {
      #   setTxtProgressBar(pb, n)
      # }
      progressSeq <- function(currentStep, maxSteps) {
        cat(paste0(
          "\r Current Step = ",
          currentStep,
          " of a maximum ",
          maxSteps,
          ".  "
        ), file = stdout())
      }
      
      
    }
    
    `%dopar%` <- foreach::`%dopar%`
    if (setChains > 1) {
      opts <- list(progress = progressPar)
    } else {
      opts <- list(progress = progressSeq)
    }
    
    chains = setChains
    currentStep <- 1
    consecutive <- 0
    
    #### perform algorithm ####
    start.time <- Sys.time()

    cat("\n Current Progress: \n")

    allModel <- paste0(currentModel@model.syntax, collapse = "\n")
    allFit <- bestFit
    
    chainResults <-
      foreach::foreach(chain = 1:chains, .inorder = F, .combine = rbind, 
                       .options.snow = opts) %dopar% {

        while (currentStep < maxSteps) {
          
          # how many changes to make?
          numChanges <- sample(1:maxChanges, size = 1)
          # generate random model
          if (shortForm == FALSE) {
            randomNeighborModel <- randomNeighborFull(
              currentModelObject = currentModel,
              numChanges = numChanges,
              data = originalData
            )
          } else if (shortForm == TRUE) {
            randomNeighborModel <- randomNeighborShort(
              currentModelObject = currentModel,
              numChanges = numChanges,
              data = originalData,
              allItems = allItems,
              bifactor = bifactor,
              init.model = initialModel,
              lavaan.model.specs = lavaan.model.specs
            )
          }
          # select between random model and current model
          currentModel <- selectionFunction(
            currentModel = currentModel,
            randomNeighborModel = randomNeighborModel,
            currentTemp = temperatureFunction(currentStep, maxSteps),
            maximize = maximize,
            fitStatistic = fitStatistic,
            consecutive = consecutive
          )
          # recored fit
          allFit[currentStep + 1] <- tryCatch(
          # allFit <- tryCatch(
            lavaan::fitmeasures(object = currentModel@model.output, fit.measures = fitStatistic),
            error = function(e) {
              if (length(e) > 0) {
                bestFit
              }
            }
          )
          # check for current best model
          bestModel <- checkModels(currentModel, fitStatistic, maximize, bestFit, bestModel)
          bestFit <- tryCatch(
            lavaan::fitmeasures(object = bestModel@model.output, fit.measures = fitStatistic),
            error = function(e) {
              if (length(e) > 0) {
                bestFit
              }
            }
          )
          
          allModel <- c(allModel, currentModel@model.syntax)
    
          # restart if the same model was chosen too many times
          restartCriteria(
            maxConsecutiveSelection = maximumConsecutive,
            consecutive = consecutive
          )
          currentStep <- currentStep + 1
          
        }
        returnList <-
          list(
            'allModel' = allModel,
            'allFit' = allFit,
            'bestModel' = bestModel,
            'bestFit' = bestFit
          )
      }
    
    
    if (setChains > 1) {
      foreach::registerDoSEQ()
      parallel::stopCluster(cl)
      
      best_fit <-
        max(as.numeric(chainResults[,'bestFit']))
      
      names(best_fit) <-
        names(chainResults[1,4][[1]])
      
      all_fit <-
        chainResults[,'allFit']
    } else {
      best_fit <-
        as.numeric(chainResults$bestFit)
      
      names(best_fit) <-
        names(chainResults$bestFit)
      
      all_fit <-
        as.numeric(chainResults$allFit)
    }
    
    
    results <-
      new(
        'SA',
        function_call = match.call(),
        chains = setChains,
        chain_results = chainResults,
        all_fit = all_fit,
        best_fit = best_fit,
        best_model = bestModel,
        best_syntax = bestModel@model.syntax,
        runtime = Sys.time() - start.time
      )

    results
  }


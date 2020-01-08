#' A function to implement the ant colony optimization algorithm for short form
#' specification searches with the package \link[lavaan]{lavaan}.
#'
#' @description The Ant Colony Optimization (ACO) algorithm (Dorigo & Stutzle,
#'  2004) can produce short forms of scales that are optimized with respect to
#'  characteristics selected by the developer, such as model fit and predictive
#'  relationships with other variables. The algorithm is based on the foraging
#'  behavior of a group of ants, which start searching for food in a variety of
#'  directions and then eventually all ants converge to the shortest distance to
#'  the food source. This behavior occurs because ants leave a pheronome trail
#'  behind as they search for food and ants in shorter paths leave stronger
#'  pheronome trails, which are detected by other ants and that will lead them
#'  to follow the shortest trail.
#'
#' @details This function sends a specified number of ants per iteration, which
#'  randomly select items to build a model, then evaluates the model based on
#'  pheromone levels. The pheromone levels are updated after each iteration
#'  according to the best-fitting model of that iteration. The algorithm's
#'  stopping rule is to end the search when a certain solution is the same for a
#'  given number of ants in a row.
#'
#'  PREPARATORY STEPS: For the ACO algorithm implementation for short for
#'  selection, the following decisions are needed:
#'
#'  1. Determine the target size for the short form.
#'
#'  2. Determine which characteristics should be optimized.
#'
#'  3. Define how the pheronome level will be computed: This is a function of
#'  the characteristics of the short form that will be optimized. In Leite,
#'  Huang and Marcoulides (2008), the pheronomone level was zero if model fit
#'  indices did not meet Hu and Bentler's (1999) suggested thresholds, and equal
#'  to the sum of path coefficients of a predictor variable if model fit indices
#'  met thresholds. Currently, the package only implements pheromone calculation
#'  based on regression coefficients or variance explained, with user-selected
#'  model fit index thresholds.
#'
#'  4. Define how many short forms should be evaluated before the best-so-far
#'  pheronome level is examined. Leite, Huang and Marcoulides (2008) used 10
#'  short forms.
#'
#'  5. Define the percentage of pheronome evaporation, if any. Leite, Huang and
#'  Marcoulides (2008) used 5\%.
#'
#'  6. Define convergence criterion. Leite, Huang and Marcoulides (2008) set the
#'  algorithm to converge if the short form did not improve in 100 x number of
#'  short forms in step 4.
#'
#'  IMPLEMENTATION: Once these decisions are made, the ACO algorithm selects
#'  short forms with the following steps:
#'
#'  Step 1. All items are assigned an initial weight of 1.
#'
#'  Step 2. A set of n short forms is selected by sampling with probability
#'  proportional to the item weights.
#'
#'  Step 3. Fit the latent variable model to the n short forms.
#'
#'  Step 4. Calculate the pheromone levels for the n short forms. Define the
#'  best-so-far pheronome level (if iteration 1) or compare the current best
#'  pheronome from the set of n short forms to the best-so-far pheronome.
#'
#'  Step 5. If the pheromone level of the best short form from step 4 exceeds
#'  the best-so-far pheronome level, update the best-so-far pheromone level and
#'  add it to the current weight of the items of the best short form.
#'
#'  Step 6. Return to step 2 until convergence criterion is reached.
#'
#' @param data The data being used in data frame format. Default value is
#'  \code{null}. Only one of \code{data} or \code{sample.cov} should be used.
#' @param sample.cov The sample covariance matrix. See \link[lavaan]{lavaan} for
#'  the specific format needed. Default value is \code{null}. Only one of
#'  \code{data} or \code{sample.cov} should be used.
#' @param sample.nobs A numeric value indicating the number of observations in
#'  the sample covariance matrix. If \code{sample.cov} is used, this must be
#'  filled in. Default value is \code{null}.
#' @param ants A numeric value indicating the number of ants to send (e.g.,
#'  number of short forms to evaluate) per iteration. Default value is 20.
#' @param evaporation A numeric value which sets the percentage of the pheremone
#'  that is retained after evaporation between steps of the algorithm. Default
#'  value is 0.9, indicating 10% evaporation. Should be within the range of
#'  (0,1), exclusive.
#' @param antModel The lavaan formatted model. See \link[lavaan]{lavaan} for more
#'  details. Defaults to the default \link[lavaan]{lavaan} values.
#' @param list.items A list containing one or more character vectors of item
#'  names for each factor, where each factor is a separate element of the list.
#'  The items should be input in the order in which the factors are input in
#'  \code{i.per.f} and \code{factors}.
#' @param full A numeric value indicating the total number of unique items in the
#'  test or scale.
#' @param i.per.f Vector with number of items per factor (e.g. target number), in
#'  the same order of \code{list.items} and \code{factors}.
#' @param factors Character vector with names of factors in the same order of
#'  \code{list.items} and \code{i.per.f}.
#' @param bifactor Either the name of the factor that all of the chosen items
#' will load on (as character), or `NULL` if the model is not a bifactor model.
#' @param steps A numeric value that sets the stopping rule, which is the number
#'  of ants in a row for which the model does not change.
#' @param lavaan.model.specs A list which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#' @param pheromone.calculation A character string specifying the method for
#'  calculating the pheromone strength. Must be one of "\code{gamma}"
#'  (standardized latent regression coefficients), "\code{beta}"
#'  (standardized observed regression coefficients), "\code{regression}"
#'  (both latent and observed regression coefficients, if they exist)
#'   or "\code{variance}" (proportion of
#'  variance explained by model). You must specify the entire string. Default is
#'  \code{gamma}.
#' @param fit.indices The fit indices (in lavaan format) extracted for model
#'  optimization. See \link[lavaan]{lavaan} for more details.
#' @param fit.statistics.test A character vector of the logical test being used
#'  for model optimization. The default is \code{"(cfi > 0.95)&(tli >
#'  0.95)&(rmsea < 0.06)"}. The format for the logical test should match 1) the
#'  names of the indices being used in \link[lavaan]{lavaan} and 2) the default
#'  provided above. At least one fit index must be included.
#' @param summaryfile The name of the summary file generated. A .txt file is
#'  suggested. Default is "summary.txt" and writes into the current working
#'  directory. This file writes a line for each ant within each step and
#'  includes (a) a vector of a 0/1 value for each item indicating whether the
#'  item was selected by that ant, (b) the run number, (c) the count number, (d)
#'  the ant number, and (e) the current pheromone level.
#' @param feedbackfile The name of the feedback file generated. An .html file is
#'  suggested. Default is "iteration.html" and writes into the current working
#'  directory. This file saves the result of each run, which includes (a) the
#'  run number, (b) the count number, (c) the ant number, (d) the step number
#'  (if the current run is successful) or "Failure" (if the current run is
#'  unsuccessful), and for successful runs (f) the chosen fit statistics (from
#'  \code{fit.indices}), the average of the gammas and betas (standardized regression
#'  coefficients), and the overall variance explained of the current run.
#' @param max.run The maximum number of ants to run before the algorithm stops.
#'  This includes failed iterations as well. Default is 1000.
#' @param verbose An option for increasing the amount of information displayed
#'  while the function runs. If \code{TRUE}, the function will display steps,
#'  ants, counts, and current run for each attempt as well as printing
#'  \code{"Failed iteration!"} for runs that do not converge and the model fit
#'  information for runs that do converge successfully. Default is \code{FALSE}.
#' @return A list with four elements: the first containing a named matrix with
#'  final model's best fit indices, the final pheromone level (either the mean
#'  of the standardized regression coefficients (gammas, betas, or both), or the mean variance
#'  explained), and a series of 0/1 values indicating the items selected in the
#'  final solution,  the second element containing tbe summary matrix of the
#'  best fit statistic value(s) for each run, the items chosen for said best fit,
#'  the mean gamma, beta, and variance explained for the best fit, and the item pheromone
#'  levels after each run, the third containing the best-fitting lavaan model
#'  object, and the fourth containing the best-fitting model syntax.
#'
#' @family Ant Colony Algorithms
#' @seealso \code{\link{antcolony.mplus}}
#' @examples
#' # a 3-factor example using the HolzingerSwineford1939 data from `lavaan`
#'
#' # some changes to the default values
#' # notice that in this example we are recreating the original model
#' abilityShortForm <- antcolony.lavaan(
#'   data = lavaan::HolzingerSwineford1939,
#'   ants = 2, evaporation = 0.7,
#'   antModel = " visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 ",
#'   list.items = list(c(
#'     "x1",
#'     "x2", "x3"
#'   ), c("x4", "x5", "x6"), c("x7", "x8", "x9")), full = 9, i.per.f =
#'     c(3, 3, 3), factors = c("visual", "textual", "speed"), steps = 1, fit.indices =
#'     c("cfi"), fit.statistics.test = "(cfi > 0.6)", summaryfile =
#'     NULL, feedbackfile = NULL, max.run = 2
#' )
#' \dontrun{
#' # using simulated test data and the default values for lavaan.model.specs
#' # first, read in the original or "full" model
#' data(exampleAntModel) # a character vector for a lavaan model
#'
#' # then, create the list of the items by the factors
#' # in this case, all items load onto the general 'Ability' factor
#' list.items <- list(c(
#'   "Item1", "Item2", "Item3", "Item4", "Item5",
#'   "Item6", "Item7", "Item8", "Item9", "Item10",
#'   "Item11", "Item12", "Item13", "Item14", "Item15",
#'   "Item16", "Item17", "Item18", "Item19", "Item20",
#'   "Item21", "Item22", "Item23", "Item24", "Item25",
#'   "Item26", "Item27", "Item28", "Item29", "Item30",
#'   "Item31", "Item32", "Item33", "Item34", "Item35",
#'   "Item36", "Item37", "Item38", "Item39", "Item40",
#'   "Item41", "Item42", "Item43", "Item44", "Item45",
#'   "Item46", "Item47", "Item48", "Item49", "Item50",
#'   "Item51", "Item52", "Item53", "Item54", "Item55", "Item56"
#' ))
#'
#' # load the data
#' data(simulated_test_data)
#'
#' # finally, call the function with some minor changes to the default values.
#' abilityShortForm <- antcolony.lavaan(
#'   data = simulated_test_data,
#'   ants = 5, evaporation = 0.7, antModel = exampleAntModel,
#'   list.items = list.items, full = 56, i.per.f = 20,
#'   factors = "Ability", steps = 3, fit.indices = c("cfi", "rmsea"),
#'   fit.statistics.test = "(cfi > 0.95)&(rmsea < 0.05)",
#'   summaryfile = "summary.txt",
#'   feedbackfile = "iteration.html",
#'   max.run = 500
#' )
#'
#' abilityShortForm # print the results of the final short form
#' }
#' @import lavaan utils
#' @export
#' @author Anthony W Raborn, \email{anthony.w.raborn@@gmail.com}

antcolony.lavaan <- function(data = NULL, sample.cov = NULL, sample.nobs = NULL,
                             ants = 20, evaporation = 0.9, antModel, list.items = NULL,
                             full = NULL, i.per.f = NULL, factors = NULL, bifactor = NULL, steps = 50,
                             lavaan.model.specs = list(
                               model.type = "cfa", auto.var = T, estimator = "default",
                               ordered = NULL, int.ov.free = TRUE, int.lv.free = FALSE,
                               auto.fix.first = TRUE, auto.fix.single = TRUE,
                               auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE,
                               auto.cov.y = TRUE, std.lv = F
                             ),
                             pheromone.calculation = "gamma", fit.indices = c("cfi", "tli", "rmsea"),
                             fit.statistics.test = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.06)",
                             summaryfile = NULL,
                             feedbackfile = NULL, max.run = 1000, verbose = FALSE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("The `lavaan` package is required to use this function. Please install `lavaan`, then try to use this function again.")
  }

  antcolony.lavaan.env <- new.env(parent = baseenv())

  if (pheromone.calculation %in% c("gamma", "beta", "regression", "variance") == FALSE) {
    stop("Pheromone calculation not recognized! Enter one of \'gamma\', \'beta\', \'regression\' or \'variance\'.")
  }
  # create initial, empty files to be used
  if (length(summaryfile) > 0) {
    write(x = "", file = summaryfile)
  }

  summary <- matrix(
    nrow = 1,
    ncol = (full + 3 + 3 + length(fit.indices) + full)
  )
  # ncol = number of items + 3 (run, ant, count) +
  # 2 (mean.gamma, mean.var.exp) + number of fit indices + number of items

  if (length(feedbackfile) > 0) {
    write(x = "", file = feedbackfile)
  }
  # creates the table of initial pheromone levels.
  include <- rep(2, full)
  # puts initial best solution (all items selected).
  best.so.far.solution <- include

  # creates a vector with all items. UNIQUE USED FOR CASES WHEN ITEMS CROSS-LOAD
  item.vector <- unique(unlist(list.items, use.names = F))
  if (!is.null(bifactor)) {
    item.vector <- item.vector[which(item.vector != bifactor)]
  }

  # reads the Lavaan model syntax input into the function
  input <- unlist(strsplit(antModel, "\n"))

  # creates a list to store factors.
  selected.items <- list.items

  # starts counting the iterations
  count <- 1

  # starts counting continuous runs regardless of result.
  run <- 1

  # defines initial best so far (overall) pheromone
  best.so.far.pheromone <- 0
  # defines initial best pheromone for the current trial of n ants.
  best.pheromone <- 0
  # defines initial solutions.
  previous.solution <- include
  step <- 1

  # creates objects in the function environment that are fed into the lavaan function in order to fine-tune the model to user specifications
  mapply(assign, names(lavaan.model.specs), lavaan.model.specs, MoreArgs = list(envir = antcolony.lavaan.env))

  # create values of "bad warnings" and "bad errors" that result in uninterpretable models
  bad.warnings <- c(
    "WARNING: could not compute standard errors",
    "WARNING: could not compute scaled test statistic",
    "WARNING: covariance matrix of latent variables is not positive definite",
    "WARNING: model has NOT converged",
    "WARNING: could not invert information matrix",
    "WARNING: the optimizer warns that a solution has NOT been found",
    "WARNING: some estimated ov variances are negative"
  )
  bad.errors <- c(
    "ERROR: initial model-implied matrix (Sigma) is not positive definite",
    "ERROR: missing observed variables in dataset"
  )
  
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2L
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores()
  }
  
  cl <- parallel::makeCluster(num_workers,type="PSOCK", outfile = "")
  doParallel::registerDoParallel(cl)
  `%dopar%` <- foreach::`%dopar%`
  ant = 0L
  
  start.time <- Sys.time()


  # starts loop through iterations.
  while (step <= steps) {
    if (run <= max.run) {
      cat(paste("\r Run number ", run, ".           ", sep = ""))
      # sends a number of ants per time.
      
      antResults <-
        foreach::foreach(ant = 1:ants, .inorder = F, .combine = rbind) %dopar% {
        if (verbose == TRUE) {
          cat(paste("\r Step = ", step, " and Ant = ", ant, " and Count = ", count, " and Run = ", run, ".   ", sep = ""))
        }
        
        # selects items for all factors.
        newModelList <- antcolonyNewModel(
          itemList = list.items,
          itemVector = item.vector,
          includedItems = include,
          model = input,
          itemCount = i.per.f,
          factorNames = factors,
          bifactor
        )
        
        mapply(assign, names(newModelList), newModelList, MoreArgs = list(envir = antcolony.lavaan.env))
        
        
        selected.items <- lapply(antcolony.lavaan.env$selected.items, sort)
        selected.vector <- unlist(antcolony.lavaan.env$selected.items, use.names = F)
        select.indicator <- is.element(item.vector, selected.vector)
        
        # MODIFY LAVAAN SYNTAX
        new_ant_model <- paste(antcolony.lavaan.env$input, collapse = "\n")
        
        # Run the model check function
        # checks for and saves error/warning messages within the lavaan output,
        # as well as the fit indices
        modelCheck <- modelWarningCheck(
          lavaan::lavaan(
            model = new_ant_model, data = data, sample.cov = sample.cov,
            sample.nobs = sample.nobs,
            model.type = antcolony.lavaan.env$model.type,
            ordered = antcolony.lavaan.env$ordered,
            estimator = antcolony.lavaan.env$estimator,
            int.ov.free = antcolony.lavaan.env$int.ov.free,
            int.lv.free = antcolony.lavaan.env$int.lv.free,
            auto.fix.first = antcolony.lavaan.env$auto.fix.first,
            std.lv = antcolony.lavaan.env$std.lv,
            auto.fix.single = antcolony.lavaan.env$auto.fix.single,
            auto.var = antcolony.lavaan.env$auto.var,
            auto.cov.lv.x = antcolony.lavaan.env$auto.cov.lv.x,
            auto.th = antcolony.lavaan.env$auto.th,
            auto.delta = antcolony.lavaan.env$auto.delta,
            auto.cov.y = antcolony.lavaan.env$auto.cov.y
          )
        )
        
        # Save the error and warning messages
        warnings <- modelCheck[[2]]
        errors <- modelCheck[[3]]
        # Check the above messages and set pheromone to zero under 'bad' circumstances
        if (any(errors %in% bad.errors) || any(warnings %in% bad.warnings)) {
          pheromone <- 0
          if (verbose == TRUE) {
            cat("Failed iteration!")
          }
          
          # writes feedback about non-convergence and non-positive definite.
          if (length(summaryfile) > 0) {
            fit.info <- matrix(c(select.indicator, run, count, ant, 999, 999, round((include), 5)), 1, )
            write.table(fit.info,
                        file = summaryfile, append = T,
                        quote = F, sep = " ", row.names = F, col.names = F
            )
          }
          
          # provide feedback about search.
          if (length(feedbackfile) > 0) {
            feedback <- c(paste("<h1>", run, "-", count, "-", ant, "-", step, "- Failure", "</h1>"))
            write(feedback, file = feedbackfile, append = T)
          }
          # finishes if for non-convergent cases.
        } else {
          if (verbose == TRUE) {
            cat("                  ")
          }
          # compute fit indices, gammas, betas, and residual variances
          modelInfo <- modelInfoExtract(
            modelCheckObj = modelCheck,
            fitIndices = fit.indices
          )
          
          mapply(assign, names(modelInfo), modelInfo, MoreArgs = list(envir = antcolony.lavaan.env))
          mapply(assign, names(antcolony.lavaan.env$model.fit), antcolony.lavaan.env$model.fit, MoreArgs = list(envir = antcolony.lavaan.env))
          
          # provide feedback about search.
          if (length(feedbackfile) > 0) {
            feedback <- c(paste(
              "<h1>", "run:", run, "count:", count, "ant:", ant, "step:", step, "<br>",
              "Fit Statistics:", antcolony.lavaan.env$model.fit, "<br>",
              "GAMMA:", mean(antcolony.lavaan.env$std.gammas),
              "BETA:", mean(antcolony.lavaan.env$std.betas),
              "VAR.EXP:", mean(antcolony.lavaan.env$variance.explained), "</h1>"
            ))
            write(feedback, file = feedbackfile, append = T)
          }
          
          # implements fit requirement.
          if (eval(parse(text = fit.statistics.test),
                   envir = antcolony.lavaan.env
          ) == FALSE) {
            # Model didn't fit well enough, so set pheromone to 0.
            pheromone <- 0
          } else {
            # Model fit well enough, so calculate pheromone by either gamma or variance.
            if (pheromone.calculation == "gamma") { # mean of standardized gammas
              pheromone <- round(mean(antcolony.lavaan.env$std.gammas, na.rm = T), 3)
            } else {
              if (pheromone.calculation == "beta") { # mean of standardized betas
                pheromone <- round(mean(antcolony.lavaan.env$std.betas, na.rm = T), 3)
              } else {
                if (pheromone.calculation == "regression") { # mean of all regression coefs
                  pheromone <- round(mean(antcolony.lavaan.env$std.reg.coef, na.rm = T), 3)
                }
                if (pheromone.calculation == "variance") { # mean of r^2 values
                  pheromone <- round(mean(antcolony.lavaan.env$variance.explained, na.rm = T), 3)
                }
              }
            }
          }
          # move below to outside of %dopar%
          # adjusts count based on outcomes and selects best solution.
          # if (pheromone >= best.pheromone) {
          #   
          #   # updates solution.
          #   best.solution <- select.indicator
          #   # updates best RMSEA.
          #   best.fit.indices <- antcolony.lavaan.env$model.fit
          #   # updates best pheromone
          #   best.pheromone <- pheromone
          # }
          
          # Move to next ant.
          
          # end else clause for converged solutions
        }
        # move below to outside of %dopar%
        # evaluates the criterion to stop the interactions
        # the criterion is that the scale selected cannot change in the specified
        # number of steps.
        # if (sum(previous.solution != select.indicator) == 0) {
        #   step <- step + 1
        # } else {
        #   step <- 1
        # }
        
        # previous.solution <- select.indicator
        # returnMatrix = matrix(c(
        #   select.indicator,
        #   run, count, ant,
        #   antcolony.lavaan.env$model.fit,
        #   pheromone,
        #   mean(antcolony.lavaan.env$std.gammas),
        #   mean(antcolony.lavaan.env$std.betas),
        #   mean(antcolony.lavaan.env$variance.explained)
        # ), 1, )
        # colnames(returnMatrix) = c(item.vector, "run", "count", "ant",
        # names(antcolony.lavaan.env$model.fit), "pheromone", "mean.std.gammas",
        # "mean.std.betas", "mean.var.exp")
        returnMatrix = list(
          'solution' = select.indicator,
          'run' = run,
          'count' = count,
          'ant' = ant,
          'model.fit' = antcolony.lavaan.env$model.fit,
          'pheromone' = pheromone,
          'mean.std.gammas' = mean(antcolony.lavaan.env$std.gammas),
          'mean.std.betas' = mean(antcolony.lavaan.env$std.betas),
          'mean.var.exp' = mean(antcolony.lavaan.env$variance.explained),
          'model.output' = modelCheck$model.output,
          'model.syntax' = new_ant_model
        )
        
        returnMatrix
      }
      
      # antResults <- as.data.frame(antResults)
      # implements pheromone evaporation.
      include <- include * evaporation
      
      bestAnt <-
        which(unlist(antResults[,'pheromone'])==
                max(unlist(antResults[,'pheromone'])))[[1]]
      best.pheromone <-
        antResults[[bestAnt,'pheromone']]


      # adjusts pheromone and best.so.far values only if the current pheromone is best than the previous.
      if (best.pheromone > best.so.far.pheromone) {
        include.pheromone <- antResults[[bestAnt, 'solution']] * best.pheromone
        include <- include + include.pheromone

        best.so.far.solution <- as.numeric(antResults[[bestAnt, 'solution']])
        best.so.far.pheromone <- best.pheromone
        best.so.far.fit.indices <- antResults[[bestAnt, 'model.fit']]
        best.so.far.model <- antResults[[bestAnt, 'model.output']]
        best.so.far.syntax <- antResults[[bestAnt, 'model.syntax']]

        if (!all(sapply(antResults[-1,'solution'], FUN = identical, antResults[1, 'solution']))) {
          # re-starts count.
          count <- 1
          step <- step + ants
        }
      } else {

        # advances count.
        count <- count + 1

        # adds more pheromone to the best so far solution.
        include.pheromone <- best.so.far.solution * best.so.far.pheromone

        # updates pheromone.
        include <- include + include.pheromone
      }


      # ends loop.
      run <- run + 1
      summary <- 
        rbind(
          summary,
          matrix(
            c(
              as.numeric(antResults[[bestAnt, 'solution']]),
              run,
              bestAnt,
              count,
              antResults[[bestAnt, 'model.fit']],
              antResults[[bestAnt, 'mean.std.gammas']],
              antResults[[bestAnt, 'mean.std.betas']],
              antResults[[bestAnt, 'mean.var.exp']],
              include
            ),
           nrow = 1
           )
        )
        
    }
    if (run == max.run) {
      warning("Max runs reached! Problems converging onto a solution.")
      break
      }
  }
  
  foreach::registerDoSEQ()
  parallel::stopCluster(cl)

  print("Compiling results.")

  summary <- data.frame(summary[-1, ])
  colnames(summary) <- 
    c(item.vector,
      "run",
      "ant",
      "count",
      fit.indices, 
      "mean.gamma",
      "mean.beta",
      "mean.var.exp", 
      paste0(item.vector, ".Pheromone")
      )

  final.solution <- matrix(c(best.so.far.fit.indices, best.so.far.pheromone, best.so.far.solution), 1, ,
    dimnames = list(NULL, c(names(antcolony.lavaan.env$model.fit), paste0("mean_", pheromone.calculation), item.vector))
  )
  
  results <-
    new(
      'ACO',
      function_call = match.call(),
      summary = summary,
      final_solution = final.solution,
      best_model = best.so.far.model,
      best_syntax = best.so.far.syntax,
      runtime = Sys.time() - start.time
    )

  results
}

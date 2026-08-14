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
#'  Huang and Marcoulides (2008), the pheromone level was zero if model fit
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
#' @param evaporation A numeric value which sets the percentage of the pheromone
#'  that is retained after evaporation between steps of the algorithm. Default
#'  value is 0.9, indicating 10% evaporation. Should be within the range of
#'  (0,1), exclusive.
#' @param initialModel The lavaan formatted model. See \link[lavaan]{lavaan} for more
#'  details. Defaults to the default \link[lavaan]{lavaan} values. NOTE: Each factor
#'  and/or regression needs to be specified on a single line. Newline breaks and
#'  carriage returns WILL break the function.
#' @param items A `character` vector of candidate item names. Defaults to
#'  `NULL`, which uses all column names in `data` (required if `data` is
#'  `NULL`, i.e. when using `sample.cov`/`sample.nobs` instead). Every
#'  candidate item must appear on its factor's line in `initialModel`; an
#'  item cross-loading on multiple factors should appear on each of those
#'  factors' lines.
#' @param itemsPerFactor Numeric vector with the target number of items to
#'  retain per factor, in the same order the factors appear in
#'  `initialModel`.
#' @param bifactor Either the name of the factor that all of the chosen items
#' will load on (as character), or `NULL` if the model is not a bifactor model.
#' @param steps A numeric value that sets the stopping rule, which is the number
#'  of ants in a row for which the model does not change.
#' @param lavaan.model.specs A list which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. These are automatically set internally, then updated by the user-provided
#'  values -- a partial list is accepted, and any element you omit falls back
#'  to the default for that element. Every name you do supply must match one
#'  of the recognized element names, or the call errors.
#'  Note that this drastically affects the algorithm, and care must be
#'  taken to ensure that the algorithm can fit valid models as it searches for
#'  the best model. See the default arguments for examples of what you can change
#'  and \link[lavaan]{lavaan} for more details on what arguments are available
#'  to change.
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
#' @param maxIterations The maximum number of ants to run before the algorithm stops.
#'  This includes failed iterations as well. Default is 1000.
#' @param parallel An option for using parallel processing. If \code{TRUE}, the
#'  function will utilize all available cores (up to the number of ants). Default
#'  is \code{TRUE}.
#' @param verbose Logical. If `TRUE` (the default), prints per-ant progress to
#'  the console. The full per-run history is always available afterward via
#'  the returned object's `summary` and `final_solution` slots regardless of
#'  this setting.
#' @return An S4 object of class `ACO`, with (among other slots)
#'  `final_solution` holding a named matrix with the final model's best fit
#'  indices, the final pheromone level (either the mean of the standardized
#'  regression coefficients (gammas, betas, or both), or the mean variance
#'  explained), and a series of 0/1 values indicating the items selected in
#'  the final solution; `summary` holding the summary data.frame of the best
#'  fit statistic value(s) for each run, the items chosen for said best fit,
#'  the mean gamma, beta, and variance explained for the best fit, and the
#'  item pheromone levels after each run; `best_model` holding the
#'  best-fitting lavaan model object; and `best_syntax` holding the
#'  best-fitting model syntax.
#'
#' @family Ant Colony Algorithms
#' @seealso \code{\link{antcolony.mplus}}
#' @examples
#' # a 3-factor example using the HolzingerSwineford1939 data from `lavaan`
#'
#' # some changes to the default values
#' # notice that in this example we are recreating the original model
#' abilityShortForm <- antColony(
#'   data = lavaan::HolzingerSwineford1939,
#'   ants = 2, evaporation = 0.7,
#'   initialModel = " visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 ",
#'   itemsPerFactor = c(3, 3, 3), steps = 2, fit.indices =
#'     c("cfi"), fit.statistics.test = "(cfi > 0.6)",
#'     maxIterations = 2, parallel = FALSE
#' )
#' \dontrun{
#' # using simulated test data and the default values for lavaan.model.specs
#' # first, read in the original or "full" model
#' data(exampleAntModel) # a character vector for a lavaan model
#'
#' # load the data
#' data(simulated_test_data)
#'
#' # finally, call the function with some minor changes to the default values.
#' # every candidate item (all 56, in this case) must already appear on its
#' # factor's line in exampleAntModel -- see ?exampleAntModel
#' abilityShortForm <- antColony(
#'   data = simulated_test_data,
#'   ants = 5, evaporation = 0.7, initialModel = exampleAntModel,
#'   itemsPerFactor = 20,
#'   steps = 3, fit.indices = c("cfi", "rmsea"),
#'   fit.statistics.test = "(cfi > 0.95)&(rmsea < 0.05)",
#'   maxIterations = 500
#' )
#'
#' abilityShortForm # print the results of the final short form
#'
#' # an example using binary (ordered) data
#' # create the simulated full model and model data
#' sim_model <- "
#' f1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
#' f2 =~ x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20
#' f3 =~ x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30"
#'
#' sim_data <-
#'   cbind(
#'     psych::sim.rasch(nvar = 10)$items,
#'     psych::sim.rasch(nvar = 10)$items,
#'     psych::sim.rasch(nvar = 10)$items
#'   )
#'
#' colnames(sim_data) = paste0("x", 1:30)
#' # fit with antColony
#' # note that ONLY the estimator and ordered args
#' # of lavaan.model.specs are changed. This retains
#' # the default args, fitting a CFA but with ordered data.
#' example <-
#' antColony(
#'   data = sim_data,
#'   ants = 5, evaporation = 0.7,
#'   initialModel = sim_model,
#'   lavaan.model.specs =
#'     list(estimator = "wlsmv", ordered = T),
#'   itemsPerFactor = c(5, 5, 5),
#'   steps = 20,
#'   fit.indices = c("cfi.scaled"),
#'   fit.statistics.test = "(cfi.scaled > 0.90)",
#'   maxIterations = 500,
#'   parallel = T
#' )
#' # note that this example will take a bit of time to run
#' # as ordered data factor analysis is computationally expensive.
#' }
#' @import lavaan utils
#' @export
#' @author Anthony W Raborn, \email{anthony.w.raborn@@gmail.com}

antColony <- function(data = NULL, sample.cov = NULL, sample.nobs = NULL,
                             ants = 20, evaporation = 0.9, initialModel, items = NULL,
                             itemsPerFactor = NULL, bifactor = NULL, steps = 50,
                             lavaan.model.specs = list(
                               model.type = "cfa", auto.var = T, estimator = "default",
                               ordered = NULL, int.ov.free = TRUE, int.lv.free = FALSE,
                               auto.fix.first = TRUE, auto.fix.single = TRUE, auto.var = TRUE,
                               auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE,
                               auto.cov.y = TRUE, std.lv = F,
                               group = NULL, group.label = NULL, group.equal = 'loadings',
                               group.partial = NULL, group.w.free = FALSE
                             ),
                             pheromone.calculation = "gamma", fit.indices = c("cfi", "tli", "rmsea"),
                             fit.statistics.test = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.06)",
                             maxIterations = 1000, parallel = T, verbose = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("The `lavaan` package is required to use this function. Please install `lavaan`, then try to use this function again.")
  }
  fitmeasuresCheck(fit.indices)
  antColonyEnv <- new.env(parent = baseenv())

  if (pheromone.calculation %in% c("gamma", "beta", "regression", "variance") == FALSE) {
    stop("Pheromone calculation not recognized! Enter one of \'gamma\', \'beta\', \'regression\' or \'variance\'.")
  }

  if (is.null(items)) {
    if (is.null(data)) {
      stop("`items` must be supplied when using sample.cov/sample.nobs instead of data.")
    }
    items <- colnames(data)
  }
  # derives factors and each factor's candidate item pool from initialModel's
  # syntax (every candidate item must appear on its factor's line -- a
  # cross-loaded item should appear on each of those factors' lines)
  extracted <- syntaxExtraction(initialModelSyntaxFile = initialModel, items = items)
  factors <- extracted$factors
  list.items <- extracted$itemsPerFactor
  full <- length(unique(unlist(list.items, use.names = FALSE)))
  i.per.f <- itemsPerFactor

  summaryObject <- matrix(
    nrow = 1,
    ncol = (full + 3 + 3 + length(fit.indices) + full)
  )

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
  input <- unlist(strsplit(initialModel, "\n"))

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
  # use a default set of specifications that fits a CFA
  default.lavaan.model.specs = list(
    model.type = "cfa", estimator = "default",
    ordered = NULL, int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = TRUE,
    auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE, auto.th = TRUE,
    auto.delta = TRUE, auto.cov.y = TRUE, std.lv = F, group = NULL, group.label = NULL,
    group.equal = "loadings", group.partial = NULL, group.w.free = FALSE
  )
  # fill in any lavaan.model.specs the user omitted with the defaults above,
  # so a partial override (e.g. estimator = "wls") is respected without
  # requiring the full list; errors on any unrecognized (likely misspelled)
  # name instead of silently ignoring it
  lavaan.model.specs <- mergeModelSpecs(lavaan.model.specs, default.lavaan.model.specs)

  # create values of "bad warnings" and "bad errors" that result in uninterpretable models
  bad.warnings <- c(
    "could not compute standard errors",
    "could not compute scaled test statistic",
    "covariance matrix of latent variables is not positive definite",
    "model has NOT converged",
    "could not invert information matrix",
    "the optimizer warns that a solution has NOT been found",
    "some estimated ov variances are negative"
  )
  bad.errors <- c(
    "initial model-implied matrix (Sigma) is not positive definite",
    "missing observed variables in dataset"
  )

  parallelSetup <- setupParallelCluster(parallel, parallel::detectCores())
  cl <- parallelSetup$cluster
  num_workers <- parallelSetup$num_workers
  `%dopar%` <- parallelSetup$dopar

  ant = 0L
  progress <- function(n) {
    if (verbose) {
      cat(paste("\r Run number ", run, " and ant number ", n, ".           ", sep = ""))
    }
  }
  opts <- list(progress = progress)

  start.time <- Sys.time()


  # starts loop through iterations.
    while (run <= maxIterations) {
      antResults <-
        foreach::foreach(ant = 1:ants, .inorder = F, .combine = rbind, .options.snow = opts, .errorhandling = 'remove') %dopar% {

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

        # only selected.items (of newModelList's input/selected.items/all.items)
        # is used below
        selected.items <- lapply(newModelList$selected.items, sort)
        selected.vector <- unlist(newModelList$selected.items, use.names = F)
        select.indicator <- is.element(item.vector, selected.vector)

        # MODIFY LAVAAN SYNTAX
        new_ant_model <- 
          input
        for (factor in 1:length(factors)) {
          temp_factor_definition <-
            new_ant_model[grepl(factors[factor], new_ant_model)]
          temp_factor_definition <-
            sub(
              "=~[[:alnum:][:space:] +]{1,}", 
              paste0("=~ ", paste0(selected.items[[factor]], collapse = " + ")), 
              temp_factor_definition
              )
          new_ant_model[grepl(factors[factor], new_ant_model)] <-
            temp_factor_definition
        }

        # Run the model check function
        # checks for and saves error/warning messages within the lavaan output,
        # as well as the fit indices
        modelCheck <- modelWarningCheck(
          do.call(
            lavaan::lavaan,
            c(
              list(
                model = new_ant_model, data = data, sample.cov = sample.cov,
                sample.nobs = sample.nobs
              ),
              lavaan.model.specs
            )
          ),
          modelSyntax = new_ant_model
        )

        # Save the error and warning messages
        warnings <- modelCheck@warnings
        errors <- modelCheck@errors
        # Check the above messages and set pheromone to zero under 'bad' circumstances
        if (length(warnings) > 0 | length(errors) > 0) {
          if (any(grepl(paste0(bad.errors, collapse = "|"), errors, ignore.case = T)) ||
              any(grepl(paste0(bad.warnings, collapse = "|"), warnings, ignore.case = T))) {
            pheromone <- 0
            # finishes if for non-convergent cases.
          }
        }
         else {
          modelInfo <- modelInfoExtract(
            modelCheckObj = modelCheck,
            fitIndices = fit.indices
          )

          mapply(assign, names(modelInfo), modelInfo, MoreArgs = list(envir = antColonyEnv))
          mapply(assign, names(antColonyEnv$model.fit), antColonyEnv$model.fit, MoreArgs = list(envir = antColonyEnv))

          # implements fit requirement.
          if (eval(parse(text = fit.statistics.test),
                   envir = antColonyEnv
          ) == FALSE) {
            # Model didn't fit well enough, so set pheromone to 0.
            pheromone <- 0
          } else {
            # Model fit well enough, so calculate pheromone by either gamma or variance.
            if (pheromone.calculation == "gamma") { # mean of standardized gammas
              pheromone <- round(mean(antColonyEnv$std.gammas, na.rm = T), 3)
            } else {
              if (pheromone.calculation == "beta") { # mean of standardized betas
                pheromone <- round(mean(antColonyEnv$std.betas, na.rm = T), 3)
              } else {
                if (pheromone.calculation == "regression") { # mean of all regression coefs
                  pheromone <- round(mean(antColonyEnv$std.reg.coef, na.rm = T), 3)
                }
                if (pheromone.calculation == "variance") { # mean of r^2 values
                  pheromone <- round(mean(antColonyEnv$variance.explained, na.rm = T), 3)
                }
              }
            }
          }

          # end else clause for converged solutions
        }

        returnMatrix = list(
          'solution' = select.indicator,
          'run' = run,
          'count' = count,
          'ant' = ant,
          'model.fit' = antColonyEnv$model.fit,
          'pheromone' = pheromone,
          'mean.std.gammas' = mean(antColonyEnv$std.gammas),
          'mean.std.betas' = mean(antColonyEnv$std.betas),
          'mean.var.exp' = mean(antColonyEnv$variance.explained),
          'model.output' = modelCheck@model.output,
          'model.syntax' = new_ant_model
        )

        returnMatrix
      }

      # implements pheromone evaporation.
      include <- include * evaporation

      bestAnt <-
        which(unlist(antResults[,'pheromone'])==
                max(unlist(antResults[,'pheromone'])))[[1]]
      best.pheromone <-
        antResults[[bestAnt,'pheromone']]


      # adjusts pheromone and best.so.far values only if the current pheromone is as good or better than the previous.
      if (best.pheromone >= best.so.far.pheromone) {
        include.pheromone <- antResults[[bestAnt, 'solution']] * best.pheromone
        include <- include + include.pheromone

        best.so.far.solution <- as.numeric(antResults[[bestAnt, 'solution']])
        best.so.far.pheromone <- best.pheromone
        best.so.far.fit.indices <- antResults[[bestAnt, 'model.fit']]
        best.so.far.model <- antResults[[bestAnt, 'model.output']]
        best.so.far.syntax <- antResults[[bestAnt, 'model.syntax']]

        if (!identical(best.so.far.solution, previous.solution)) {
          # re-starts count, since the best-so-far solution changed from the previous run.
          count <- 1
        }
        previous.solution <- best.so.far.solution
      } else {

        # advances count.
        count <- count + ants

        # adds more pheromone to the best so far solution.
        include.pheromone <- best.so.far.solution * best.so.far.pheromone

        # updates pheromone.
        include <- include + include.pheromone
      }


      # ends loop.
      run <- run + 1
      summaryObject <-
        rbind(
          summaryObject,
          matrix(
            c(
              as.numeric(antResults[[bestAnt, 'solution']]),
              run,
              bestAnt,
              count,
              if (length(antResults[[bestAnt, 'model.fit']]) < length(fit.indices)) {
                rep(NA, times = length(fit.indices))
              } else {
                antResults[[bestAnt, 'model.fit']]
              },
              antResults[[bestAnt, 'mean.std.gammas']],
              antResults[[bestAnt, 'mean.std.betas']],
              antResults[[bestAnt, 'mean.var.exp']],
              include
            ),
           nrow = 1
           )
        )
        if (count >= steps) {
          break
        }

    }

  teardownParallelCluster(cl)

  if (verbose) {
    print("Compiling results.")
  }

  summaryObject <- data.frame(summaryObject)[-1, ]
  colnames(summaryObject) <-
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

  final.solution <-
    matrix(
      c(best.so.far.fit.indices, best.so.far.pheromone, best.so.far.solution),
      1,
      dimnames = list(
        NULL,
        c(names(best.so.far.fit.indices), paste0("mean_", pheromone.calculation), item.vector)
        )
  )

  # capture the full call with every argument resolved (specified or not),
  # then substitute the actual *merged* lavaan.model.specs (computed near
  # the top of this function) in place of whatever partial/omitted
  # expression the caller wrote, so the stored call reflects what was
  # really used, not just what was typed
  capturedCall <- resolvedCall(match.call(), formals())
  capturedCall$lavaan.model.specs <- lavaan.model.specs

  results <-
    new(
      'ACO',
      function_call = capturedCall,
      summary = summaryObject,
      final_solution = final.solution,
      best_model = best.so.far.model,
      best_syntax = best.so.far.syntax,
      runtime = Sys.time() - start.time
    )

  results
}

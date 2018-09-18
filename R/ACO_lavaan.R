#'A function to implement the ant colony optimization algorithm for short form
#'specification searches with the package \link[lavaan]{lavaan}.
#'
#'@description The Ant Colony Optimization (ACO) algorithm (Dorigo & Stutzle,
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
#'@details This function sends a specified number of ants per iteration, which
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
#'@param data The data being used in data frame format. Default value is
#'  \code{null}. Only one of \code{data} or \code{sample.cov} should be used.
#'@param sample.cov The sample covariance matrix. See \link[lavaan]{lavaan} for
#'  the specific format needed. Default value is \code{null}. Only one of
#'  \code{data} or \code{sample.cov} should be used.
#'@param sample.nobs A numeric value indicating the number of observations in
#'  the sample covariance matrix. If \code{sample.cov} is used, this must be
#'  filled in. Default value is \code{null}.
#'@param ants A numeric value indicating the number of ants to send (e.g.,
#'  number of short forms to evaluate) per iteration. Default value is 20.
#'@param evaporation A numeric value which sets the percentage of the pheremone
#'  that is retained after evaporation between steps of the algorithm. Default
#'  value is 0.9, indicating 10% evaporation. Should be within the range of
#'  (0,1), exclusive.
#'@param antModel The lavaan formatted model. See \link[lavaan]{lavaan} for more
#'  details. Defaults to the default \link[lavaan]{lavaan} values.
#'@param list.items A list containing one or more character vectors of item
#'  names for each factor, where each factor is a separate element of the list.
#'  The items should be input in the order in which the factors are input in
#'  \code{i.per.f} and \code{factors}.
#'@param full A numeric value indicating the total number of unique items in the
#'  test or scale.
#'@param i.per.f Vector with number of items per factor (e.g. target number), in
#'  the same order of \code{list.items} and \code{factors}.
#'@param factors Character vector with names of factors in the same order of
#'  \code{list.items} and \code{i.per.f}.
#'@param bifactor Either the name of the factor that all of the chosen items 
#'will load on (as character), or `NULL` if the model is not a bifactor model.
#'@param steps A numeric value that sets the stopping rule, which is the number
#'  of ants in a row for which the model does not change.
#'@param lavaan.model.specs A list which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#'@param pheromone.calculation A character string specifying the method for
#'  calculating the pheromone strength. Must be one of "\code{gamma}"
#'  (standardized regression coefficients) or "\code{variance}" (proportion of
#'  variance explained by model). You must specify the entire string.
#'@param fit.indices The fit indices (in lavaan format) extracted for model
#'  optimization. See \link[lavaan]{lavaan} for more details.
#'@param fit.statistics.test A character vector of the logical test being used
#'  for model optimization. The default is \code{"(cfi > 0.95)&(tli >
#'  0.95)&(rmsea < 0.06)"}. The format for the logical test should match 1) the
#'  names of the indices being used in \link[lavaan]{lavaan} and 2) the default
#'  provided above. At least one fit index must be included.
#'@param summaryfile The name of the summary file generated. A .txt file is
#'  suggested. Default is "summary.txt" and writes into the current working
#'  directory. This file writes a line for each ant within each step and
#'  includes (a) a vector of a 0/1 value for each item indicating whether the
#'  item was selected by that ant, (b) the run number, (c) the count number, (d)
#'  the ant number, and (e) the current pheromone level.
#'@param feedbackfile The name of the feedback file generated. An .html file is
#'  suggested. Default is "iteration.html" and writes into the current working
#'  directory. This file saves the result of each run, which includes (a) the
#'  run number, (b) the count number, (c) the ant number, (d) the step number
#'  (if the current run is successful) or "Failure" (if the current run is
#'  unsuccessful), and for successful runs (f) the chosen fit statistics (from
#'  \code{fit.indices}), the average of the gammas (standardized regression
#'  coefficients), and the overall variance explained of the current run.
#'@param max.run The maximum number of ants to run before the algorithm stops.
#'  This includes failed iterations as well. Default is 1000.
#'@param verbose An option for increasing the amount of information displayed
#'  while the function runs. If \code{TRUE}, the function will display steps,
#'  ants, counts, and current run for each attempt as well as printing
#'  \code{"Failed iteration!"} for runs that do not converge and the model fit
#'  information for runs that do converge successfully. Default is \code{FALSE}.
#'@return A list with four elements: the first containing a named matrix with
#'  final model's best fit indices, the final pheromone level (either the mean
#'  of the standardized regression coefficients (gammas), or the mean variance
#'  explained), and a series of 0/1 values indicating the items selected in the
#'  final solution,  the second element containing tbe summary matrix of the 
#'  best fit statistic value(s) for each run, the items chosen for said best fit, 
#'  the mean gamma and variance explained for the best fit, and the item pheromone 
#'  levels after each run, the third containing the best-fitting lavaan model
#'  object, and the fourth containing the best-fitting model syntax.
#'  
#'@family Ant Colony Algorithms
#'@seealso \code{\link{antcolony.mplus}}
#' @examples
#' # a 3-factor example using the HolzingerSwineford1939 data from `lavaan`
#'
#' # some changes to the default values
#' # notice that in this example we are recreating the original model
#' abilityShortForm = antcolony.lavaan(data = lavaan::HolzingerSwineford1939,
#' ants = 1, evaporation = 0.7, 
#' antModel = ' visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 ', 
#' list.items = list(c('x1',
#' 'x2', 'x3'), c('x4', 'x5', 'x6'), c('x7', 'x8', 'x9')), full = 9, i.per.f =
#' c(3,3,3), factors = c('visual','textual','speed'), steps = 1, fit.indices =
#' c('cfi'), fit.statistics.test = "(cfi > 0.6)", summaryfile =
#' NULL, feedbackfile = NULL, max.run = 2)
#'
#' \dontrun{
#' # using simulated test data and the default values for lavaan.model.specs
#' # first, read in the original or "full" model
#' data(exampleAntModel) # a character vector for a lavaan model
#'
#' # then, create the list of the items by the factors
#' # in this case, all items load onto the general 'Ability' factor
#' list.items <- list(c('Item1','Item2','Item3','Item4','Item5',
#' 'Item6','Item7','Item8','Item9','Item10',
#' 'Item11','Item12','Item13','Item14','Item15',
#' 'Item16','Item17','Item18','Item19','Item20',
#' 'Item21','Item22','Item23','Item24','Item25',
#' 'Item26','Item27','Item28','Item29','Item30',
#' 'Item31','Item32','Item33','Item34','Item35',
#' 'Item36','Item37','Item38','Item39','Item40',
#' 'Item41','Item42','Item43','Item44','Item45',
#' 'Item46','Item47','Item48','Item49','Item50',
#' 'Item51','Item52','Item53','Item54','Item55','Item56'))
#'
#' # load the data
#' data(simulated_test_data)
#'
#' # finally, call the function with some minor changes to the default values.
#' abilityShortForm = antcolony.lavaan(data = simulated_test_data,
#' ants = 5, evaporation = 0.7, antModel = exampleAntModel,
#' list.items = list.items, full = 56, i.per.f = 20,
#' factors = 'Ability', steps = 3, fit.indices = c('cfi', 'rmsea'),
#' fit.statistics.test = "(cfi > 0.95)&(rmsea < 0.05)",
#' summaryfile = 'summary.txt',
#' feedbackfile = 'iteration.html',
#' max.run = 500)
#'
#' abilityShortForm[[1]] # print the results of the final short form
#' }
#'@import lavaan utils
#'@export
#'@author Anthony W Raborn, \email{anthony.w.raborn@@gmail.com}

antcolony.lavaan = function(data = NULL, sample.cov = NULL, sample.nobs = NULL,
                     ants = 20, evaporation = 0.9, antModel, list.items = NULL,
                     full = NULL, i.per.f = NULL, factors = NULL, bifactor = NULL, steps = 50,
                     lavaan.model.specs = list(model.type = "cfa", auto.var = T, estimator = "default", ordered = NULL, int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = TRUE, auto.fix.single = TRUE, auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y = TRUE),
                     pheromone.calculation = "gamma", fit.indices = c("cfi", "tli", "rmsea"),
                     fit.statistics.test = "(cfi > 0.95)&(tli > 0.95)&(rmsea < 0.06)",
                     summaryfile = NULL,
                     feedbackfile = NULL, max.run = 1000, verbose = FALSE) {

  if(!requireNamespace("lavaan", quietly = TRUE)){
    stop("The `lavaan` package is required to use this function. Please install `lavaan`, then try to use this function again.")
  }

  antcolony.lavaan.env <- new.env(parent = baseenv())

  if(pheromone.calculation %in% c("gamma","variance") == FALSE) {
    stop("Pheromone calculation not recognized! Enter either \'gamma\' or \'variance\'." )
  }
  # create initial, empty files to be used
  if(length(summaryfile) > 0){
  write(x = "", file = summaryfile)
  }
  
  summary = matrix(nrow = 1, 
                   ncol = (full + 3 + 2 + length(fit.indices) + full))
  # ncol = number of items + 3 (run, ant, count) + 
  # 2 (mean.gamma, mean.var.exp) + number of fit indices + number of items
  
  if(length(feedbackfile) > 0) {
    write(x = "", file = feedbackfile)
    }
  #creates the table of initial pheromone levels.
  include = rep(2,full)
  #puts initial best solution (all items selected).
  best.so.far.solution = include

  #creates a vector with all items. UNIQUE USED FOR CASES WHEN ITEMS CROSS-LOAD
  item.vector = unique(unlist(list.items, use.names = F))
  if (!is.null(bifactor)) {
    item.vector = item.vector[which(item.vector!=bifactor)]
    }

  #reads the Lavaan model syntax input into the function
  input = unlist(strsplit(antModel, '\n'))

  #creates a list to store factors.
  selected.items = list.items

  #starts counting the iterations
  count = 1

  #starts counting continuous runs regardless of result.
  run = 1

  #defines initial best so far (overall) pheromone
  best.so.far.pheromone = 0
  #defines initial best pheromone for the current trial of n ants.
  best.pheromone = 0
  #defines initial solutions.
  previous.solution = include
  step = 1

  # creates objects in the global environment that are fed into the lavaan function in order to fine-tune the model to user specifications
  mapply(assign, names(lavaan.model.specs), lavaan.model.specs, MoreArgs=list(envir = antcolony.lavaan.env))

  # create the function to check for and save error/warning messages within the lavaan output, as well as saving the fit indices
  modelWarningCheck <- function(expr) {
    warn <- err <- c()
    value <- withCallingHandlers(
      tryCatch(expr, error=function(e) {
        err <<- append(err, regmatches(paste(e), gregexpr("ERROR: [A-z ]{1,}", paste(e))))
        NULL
      }), warning=function(w) {
        warn <<- append(warn, regmatches(paste(w), gregexpr("WARNING: [A-z ]{1,}", paste(w))))
        invokeRestart("muffleWarning")
      })
    list(lavaan.output = value, warnings <- as.character(unlist(warn)), errors <- as.character(unlist(err)))
  }

  #starts loop through iterations.
  while (step <= steps) {
    if (run <= max.run){
      cat(paste("\r Run number ", run, ".           ", sep = ""))
      #sends a number of ants per time.
      ant  = 1
      while (ant <= ants) {
        if(verbose == TRUE){
          cat(paste("\r Step = ", step, " and Ant = ", ant, " and Count = ", count, " and Run = ", run, ".   ", sep = ""))}
        #selects items for all factors.
        all.items <- c()
        if (!is.character(bifactor)) {
        for (factor in 1:length(list.items)) {

          #selects the items for a short form for the factor
          positions = is.element(item.vector,list.items[[factor]])
          prob = include[positions]/sum(include[positions])

          items = sample(list.items[[factor]], size = i.per.f[factor],replace = F,prob)

          #stores selected items.
          selected.items[[factor]] = items


          #replaces the lavaan syntax for factor specification.
          factor.position = grep(paste(factors[factor],"=~"),input,ignore.case=T)
          input[factor.position]  = paste(factors[factor],"=~", paste(items,collapse =" + "))
          all.items = c(all.items, items)
          #finishes loop
        }
        } else  {
          bifactor.items = c()
          for (factor in 1:(length(list.items)-1)) {
            
            #selects the items for a short form for the factor
            positions = is.element(item.vector,list.items[[factor]])
            prob = include[positions]/sum(include[positions])
            
            items = sample(list.items[[factor]], size = i.per.f[factor],replace = F,prob)
            
            #stores selected items.
            selected.items[[factor]] = items
            bifactor.items = c(bifactor.items, items)
            
            #replaces the lavaan syntax for factor specification.
            factor.position = grep(paste(factors[factor],"=~"),input,ignore.case=T)
            input[factor.position]  = paste(factors[factor],"=~", paste(items,collapse =" + "))
            all.items = c(all.items, items)
            #finishes loop
          }
          input[grep(paste(bifactor, "=~"), input)] = paste(bifactor, "=~", paste(all.items, collapse = " + "))
          selected.items[[length(list.items)]] = bifactor.items
        }

        selected.items = lapply(selected.items,sort)
        selected.vector = unlist(selected.items, use.names = F)
        select.indicator = is.element(item.vector,selected.vector)
        notselect.indicator = (select.indicator == FALSE)

                # MODIFY LAVAAN SYNTAX
        new_ant_model = paste(input, collapse = '\n')

        # Run the model check function
        modelCheck <- modelWarningCheck(
          lavaan::lavaan(
            model = new_ant_model, data = data, sample.cov = sample.cov,
            sample.nobs = sample.nobs,
            model.type = antcolony.lavaan.env$model.type,
            auto.var = antcolony.lavaan.env$auto.var,
            ordered = antcolony.lavaan.env$ordered,
            estimator = antcolony.lavaan.env$estimator,
            int.ov.free = antcolony.lavaan.env$int.ov.free,
            int.lv.free = antcolony.lavaan.env$int.lv.free,
            auto.fix.first = antcolony.lavaan.env$auto.fix.first,
            auto.fix.single = antcolony.lavaan.env$auto.fix.single,
            auto.cov.lv.x = antcolony.lavaan.env$auto.cov.lv.x,
            auto.th = antcolony.lavaan.env$auto.th,
            auto.delta = antcolony.lavaan.env$auto.delta,
            auto.cov.y = antcolony.lavaan.env$auto.cov.y))
        # Save the error and warning messages
        warnings <- modelCheck[[2]]
        errors <- modelCheck[[3]]
        # Check the above messages and set pheromone to zero under certain circumstances
        # the circumstances in question:
        bad.warnings <- c("WARNING: could not compute standard errors",
                          "WARNING: could not compute scaled test statistic", 
                          "WARNING: covariance matrix of latent variables is not positive definite", 
                          "WARNING: model has NOT converged", 
                          "WARNING: could not invert information matrix", 
                          "WARNING: the optimizer warns that a solution has NOT been found")
         bad.errors <- c("ERROR: initial model-implied matrix (Sigma) is not positive definite",
                         "ERROR: missing observed variables in dataset")
        if(any(errors %in% bad.errors) || any(warnings %in% bad.warnings)){
          pheromone = 0
          if(verbose == TRUE) {
            cat("Failed iteration!")
            }
          
          #writes feedback about non-convergence and non-positive definite.
          if(length(summaryfile) > 0){
          fit.info = matrix(c(select.indicator,run,count,ant,999,999,round((include),5)),1,)
          write.table(fit.info, file = summaryfile, append = T,
                      quote = F, sep = " ", row.names = F, col.names = F)
          }
          
          #provide feedback about search.
          if(length(feedbackfile) > 0){
          feedback = c(paste("<h1>",run,"-",count,"-",ant,"-",step,"- Failure", "</h1>" ) )
          write(feedback, file = feedbackfile, append = T)
          }
          #finishes if for non-convergent cases.
        } else {
          if (verbose == TRUE) {
            cat("                  ")
          }
          # compute fit indices, gammas, and residual variances
          # first, fit indices
          model.fit <- lavaan::fitMeasures(modelCheck$lavaan.output, fit.indices)

          # next, gamma/variances
          # estimate the standardized coefficients of the variables
          standard.coefs <- lavaan::standardizedSolution(modelCheck$lavaan.output, se = FALSE, zstat = FALSE, pvalue = FALSE, remove.def = TRUE)
          # extract the regression coefficients
          std.gammas <- standard.coefs[which(standard.coefs[,2]=="=~"),]$est.std

          # obtains the variance explained ("rsquare") from lavaan
          variance.explained <- lavaan::lavInspect(modelCheck$lavaan.output, "rsquare")
          mapply(assign, names(model.fit), model.fit, MoreArgs=list(envir = antcolony.lavaan.env))
          
          #saves information about the selected items and the RMSEA they generated for the final ant.
          if (ant == ants && length(summaryfile) > 0){
            fit.info = matrix(c(select.indicator,run,count,ant,model.fit,mean(std.gammas), mean(variance.explained),
                                round(include,2)),1,)

            write.table(fit.info, file = summaryfile, append = T,
                        quote = F, sep = " ", row.names = F, col.names = F)
          }
          
          if (ant == ants){
            summary <- rbind(summary,
                             matrix(c(select.indicator,run,count,ant,model.fit,
                                      mean(std.gammas), mean(variance.explained),
                                      round(include,2)),1,))
          }
          
          #provide feedback about search.
          if(length(feedbackfile) > 0){
          feedback = c(paste("<h1>","run:",run,"count:",count,"ant:",ant,"step:",step,"<br>",
                             "Fit Statistics:",model.fit,"<br>",
                             "GAMMA:",mean(std.gammas), "VAR.EXP:", mean(variance.explained),"</h1>" ) )
          write(feedback, file = feedbackfile, append = T)
          }
          
          #implements fit requirement.
          if (eval(parse(text=fit.statistics.test), 
                   envir = antcolony.lavaan.env) == FALSE) {
            # Model didn't fit well enough, so set pheromone to 0.
            pheromone = 0 } else {
              # Model fit well enough, so calculate pheromone by either gamma or variance.
              if (pheromone.calculation == "gamma") {#mean of standardized gammas
                pheromone = round(mean(std.gammas, na.rm = T),3)
              } else {
                if (pheromone.calculation == "variance") { #mean of r^2 values
                  pheromone = round(mean(variance.explained, na.rm = T),3)
                }
              }
            }

          #adjusts count based on outcomes and selects best solution.
          if (pheromone >= best.pheromone) {

            #updates solution.
            best.solution = select.indicator
            #updates best RMSEA.
            best.fit.indices = model.fit
            #updates best pheromone
            best.pheromone = pheromone
          }

          #Move to next ant.
          ant = ant + 1

          #end else clause for converged solutions
        }
        #evaluates the criterion to stop the interactions
        #the criterion is that the scale selected cannot change in the specified
        #number of steps.
        if (sum(previous.solution != select.indicator) == 0)
        {step = step + 1} else {step = 1}
        #assigns current solution to previous solution.
        previous.solution = select.indicator

        #ends loop through ants.
      }

      #implements pheromone evaporation.
      include = include*evaporation


      #adjusts pheromone only if the current pheromone is best than the previous.
      if (best.pheromone > best.so.far.pheromone) {
        #Adjusts the pheromone levels.
        include.pheromone = best.solution * best.pheromone

        #updates pheromone.
        include = include + include.pheromone

        #updates best so far solution and pheromone, with corresponding RMSEA.
        best.so.far.solution = best.solution
        best.so.far.pheromone = best.pheromone
        best.so.far.fit.indices = best.fit.indices
        best.so.far.model = modelCheck$lavaan.output
        best.so.far.syntax = new_ant_model
        #re-starts count.
        count = 1

        #end if clause for pheromone adjustment.
      } else {

        #advances count.
        count = count + 1

        #adds more pheromone to the best so far solution.
        include.pheromone = best.so.far.solution * best.so.far.pheromone

        #updates pheromone.
        include = include + include.pheromone

        #finish else clause.
      }


      #ends loop.
      run = run + 1
    }
    if(run == max.run) {
      warning("Max runs reached! Problems converging onto a solution.")
    }
  }

  print("Compiling results.")
  #ranks items within factor.
  # ranks = c()
  # total.i.per.f = sapply(list.items,length)
  # maximum = cumsum(total.i.per.f)
  # minimum = c(1, (cumsum(total.i.per.f)+1))
  # ratio = include
  # for (f in 1:length(total.i.per.f)) {
  #   ranked.items = rank(ratio[minimum[f]:maximum[f]],ties.method = c("max"));print(ranked.items)
  #   ranks = c(ranks,ranked.items)
  #   names(ranks) = c("Pheromone", "Ranks")
  # }
  # 
  # results = cbind(round(ratio,3),ranks)
  # dimnames(results) = list(c(item.vector),c("ratio","ranks"))
  # 
  summary <- data.frame(summary[-1,])
  colnames(summary) = c(item.vector, "run", "ant", "count", fit.indices, "mean.gamma", "mean.var.exp", paste0(item.vector, ".Pheromone"))
  
  final.solution = matrix(c(best.so.far.fit.indices,best.so.far.pheromone,best.so.far.solution),1,,
                          dimnames=list(NULL,c(names(model.fit),"mean_gamma",item.vector)))

  #FINISH FUNCTION.
  # return(list(final.solution, results, summary))
  return(list(final.solution, summary, 'best.model' = best.so.far.model, 'best.syntax' = best.so.far.syntax))
}

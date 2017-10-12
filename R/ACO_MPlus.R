#'A function to implement the ant colony optimization algorithm for short form
#'specification searches, either using MPlus directly via
#'\code{\link[base]{system}} calls or using Mplus indirectly with the package
#'\link[MplusAutomation]{MplusAutomation}.
#'
#'@description The Ant Colony Optimization (ACO) algorithm (Dorigo & Stutzle,
#'2004) can produce short forms of scales that are optimized with respect to
#'characteristics selected by the developer, such as model fit and predictive
#'relationships with other variables. The algorithm is based on the foraging
#'behavior of a group of ants, which start searching for food in a variety of
#'directions and then eventually all ants converge to the shortest distance to
#'the food source. This behavior occurs because ants leave a pheronome trail
#'behind as they search for food and ants in shorter paths leave stronger
#'pheronome trails, which are detected by other ants and that will lead them to
#'follow the shortest trail.
#'
#'@details This function sends a specified number of ants
#'per iteration, which randomly select items to build a model, then evaluates
#'the model based on pheromone levels. The pheromone levels are updated after
#'each iteration according to the best-fitting model of that iteration. The
#'algorithm's stopping rule is to end the search when a certain solution is the
#'same for a given number of ants in a row. When constructing the mplus dataset
#'and when \code{Mplus.Automation=FALSE}, make sure that items in 'categorical
#'are' and 'usevariables' are specifications that take the same number of lines
#'per short form.
#'
#'PREPARATORY STEPS: For the ACO algorithm implementation for short
#'for selection, the following decisions are needed:
#'
#'1. Determine the target size for the short form.
#'
#'2. Determine which characteristics should be optimized.
#'
#'3. Define how the pheronome level will be computed: This is a function of the
#'characteristics of the short form that will be optimized. In Leite, Huang and
#'Marcoulides (2008), the pheronomone level was zero if model fit indices did
#'not meet Hu and Bentler's (1999) suggested thresholds, and equal to the sum of
#'path coefficients of a predictor variable if model fit indices met thresholds.
#'Currently, the package only implements pheromone calculation based on
#'regression coefficients or variance explained, with user-selected model fit
#'index thresholds.
#'
#'4. Define how many short forms should be evaluated before the best-so-far
#'pheronome level is examined. Leite, Huang and Marcoulides (2008) used 10 short
#'forms.
#'
#'5. Define the percentage of pheronome evaporation, if any. Leite, Huang and
#'Marcoulides (2008) used 5\%.
#'
#'6. Define convergence criterion. Leite, Huang and Marcoulides (2008) set the
#'algorithm to converge if the short form did not improve in 100 x number of
#'short forms in step 4.
#'
#'IMPLEMENTATION: Once these decisions are made, the ACO algorithm
#'selects short forms with the following steps:
#'
#'Step 1. All items are assigned an initial weight of 1.
#'
#'Step 2. A set of n short forms is selected by sampling with probability
#'proportional to the items' weights.
#'
#'Step 3. Fit latent variable model to the n short forms.
#'
#'Step 4. Calculate the pheronome levels for the n short forms. Define the
#'best-so-far pheronome level (if iteration 1) or compare the current best
#'pheronome from the set of n short forms to the best-so-far pheronome.
#'
#'Step 5. If the pheronome level of the best short form from step 4 exceeds the
#'best-so-far pheronome level, update the best-so-far pheromone level and add it
#'to the current weight of the items of the best short form.
#'
#'Step 6. Return to step 2 until convergence criterion is reached.
#'
#'@param ants A numeric value indicating the number of ants to send send (short
#'  forms to evaluate) per iteration. Default value is 20.
#'@param evaporation A numeric value which sets the percentage of the pheremone
#'  that is retained after evaporation between steps of the algorithm. Default
#'  value is 0.9, indicating 10% evaporation. Should be within the range of
#'  (0,1), exclusive.
#'@param mplus When \code{Mplus.Automation=FALSE}, this is a character value
#'  indicating the name of the MPlus input file without the file extension
#'  ".inp". If not in the current working directory, include the full file path
#'  where it is located. This file will be changed during the ant colony search,
#'  so it's suggested to make a backup of the original file before running the
#'  function. When \code{Mplus.Automation=TRUE}, this is an object of class
#'  \link[MplusAutomation]{mplusObject} created by
#'  \link[MplusAutomation]{MplusAutomation} and containing the initial model.
#'@param list.items A list containing one or more character vectors of item
#'  names for each factor, where each factor is a separate element of the list.
#'  The items should be input in the order in which the factors are input in
#'  \code{i.per.f} and \code{factors}.
#'@param full A numeric value indicating the total number of unique items in the
#'  test or scale.
#'@param i.per.f A vector with number of items per factor (e.g. target number),
#'  in the same order of \code{list.items} and \code{factors}.
#'@param factors A character vector with the names of the factors in the same
#'  order of \code{list.items} and \code{i.per.f}.
#'@param steps A numeric value that sets the stopping rule, which is the number
#'  of ants in a row for which the model does not change.
#'@param max.run The maximum number of ants to run before the algorithm stops.
#'  This includes failed iterations as well. Default is 1000.
#'@param resultfile A character vector containing the file path where the MPlus
#'  results for the current ant model is saved. If the file is not in the
#'  current working directery, the full path must be specified. Not used when
#'  \code{Mplus.Automation=FALSE}.
#'@param min.CFI A numeric value indicating the minimum CFI for "acceptable"
#'  model fit. Models with CFI less than this value are automatically rejected.
#'  Default is 0.95.
#'@param min.TLI A numeric value indicating the minimum TLI for "acceptable"
#'  model fit. Models with TLI less than this value are automatically rejected.
#'  Default is 0.95.
#'@param max.RMSEA A numeric value indicating the maximum RMSEA for "acceptable"
#'  model fit. Models with RMSEA greater than this value are automatically
#'  rejected. Default is 0.06
#'@param summaryfile A character vector containing the name of the summary file
#'  generated. A .txt file is suggested. Default is "summary.txt" and writes
#'  into the current working directory. This file writes a line for each ant
#'  within each step and includes (a) a vector of a 0/1 value for each item
#'  indicating whether the item was selected by that ant, (b) the run number,
#'  (c) the count number, (d) the ant number, and (e) the current pheromone
#'  level.
#'@param feedbackfile A character vector containing the name of the feedback
#'  file generated. An .html file is suggested. Default is "iteration.html" and
#'  writes into the current working directory. This file saves the result of
#'  each run, which includes (a) the run number, (b) the count number, (c) the
#'  ant number, (d) the step number (if the current run is successful) or
#'  "Failure" (if the current run is unsuccessful), and for successful runs (f)
#'  the value of CFI, TLI, and RMSEA fit indices, the average of the gammas
#'  (standardized regression coefficients), and the overall variance explained
#'  of the current run.
#'@param loc.gammas A numeric vector with the line numbers where the regression
#'  coefficients of the MIMIC model start and end (locations). Not used with
#'  \code{Mplus.Automation=TRUE}
#'@param loc.variances A numeric ector with the line numbers of the residual
#'  variances of the latent factors. Not used with \code{Mplus.Automation=TRUE}
#'@param predictors Character vector with names of predictor variables, if any.
#'@param var.predictors A numeric ector with variances of the predictor(s), if
#'  any. Not used with \code{Mplus.Automation=TRUE}
#'@param Mplus.Automation Logical. If \code{TRUE}, uses the
#'  \code{MplusAutomation} package to modify the model as the algorithm
#'  procedes. The "mplus" option will then be used as Defaults to \code{FALSE}
#'  as it is in the process of being built.
#'@param dataOut A character vector specifying the location and name of the data
#'  file generated by \code{MplusAutomation} for each iteration of the
#'  algorithm. Default is "tempData.dat" and saves to the current working
#'  directory. When specifying the name, be sure to use a data format that Mplus
#'  can read. You must change the working directory to the location in which
#'  this file will be saved. Only used when \code{Mplus.Automation=TRUE}.
#'@param modelOut A character vector specifying the location and name of the
#'  Mplus model file generated by \code{MplusAutomation} for each iteration of
#'  the algorithm. Default is "tempModel.inp" and saves to the current working
#'  directory. When specifying the name of the model file, it must be a ".inp"
#'  extension. You must change the working directory to the location in which
#'  this file will be saved. Only used when \code{Mplus.Automation=TRUE}.
#'@return A named matrix containing final model's best RMSEA, CFI, and TLI
#'  values, the final pheromone level (the mean of the standardized regression
#'  coefficients (gammas)), and a series of 0/1 values indicating the items
#'  selected in the final solution.
#'@family Ant Colony Algorithms
#'@seealso \code{\link{antcolony.lavaan}}
#' @examples
#' \dontrun{
#' # use MplusAutomation to find a 15-item short form of a simulated 56-item unidimensional test
#' # first, create the list of the items by the factors
#' # in this case, all items load onto the general 'Ability' factor
#' list.items <- list(c('Item1','Item2', 'Item3', 'Item4', 'Item5',
#'                      'Item6', 'Item7', 'Item8', 'Item9', 'Item10',
#'                      'Item11','Item12','Item13','Item14','Item15',
#'                      'Item16','Item17','Item18','Item19','Item20',
#'                      'Item21','Item22','Item23','Item24','Item25',
#'                      'Item26','Item27','Item28','Item29','Item30',
#'                      'Item31','Item32','Item33','Item34', 'Item35',
#'                      'Item36','Item37','Item38','Item39','Item40',
#'                      'Item41','Item42','Item43','Item44','Item45',
#'                      'Item46','Item47','Item48','Item49','Item50',
#'                      'Item51','Item52','Item53','Item54','Item55',
#'                      'Item56'))
#' # then, load the data
#' data(simulated_test_data)
#'
#' # Create the mplusObject with MplusAutomation
#' # notice the explicit call of each item, instead of the shorthand "Item1-Item56"
#' initial.MplusAutomation.model <- MplusAutomation::mplusObject(
#'   TITLE = "Trial ACO MpluAutomation with FERA 2016 Data;",
#'   MODEL = "Ability BY Item1 Item2 Item3 Item4 Item5
#'   Item6 Item7 Item8 Item9 Item10 Item11 Item12
#'   Item13 Item14 Item15 Item16 Item17 Item18
#'   Item19 Item20 Item21 Item22 Item23 Item24
#'   Item25 Item26 Item27 Item28 Item29 Item30
#'   Item31 Item32 Item33 Item34 Item35 Item36
#'   Item37 Item38 Item39 Item40 Item41 Item42
#'   Item43 Item44 Item45 Item46 Item47 Item48
#'   Item49 Item50 Item51 Item52 Item53 Item54
#'   Item55 Item56;",
#'   ANALYSIS = "ESTIMATOR = WLSMV;",
#'   VARIABLE = "CATEGORICAL = Item1 Item2 Item3 Item4 Item5
#'   Item6 Item7 Item8 Item9 Item10 Item11 Item12
#'   Item13 Item14 Item15 Item16 Item17 Item18
#'   Item19 Item20 Item21 Item22 Item23 Item24
#'   Item25 Item26 Item27 Item28 Item29 Item30
#'   Item31 Item32 Item33 Item34 Item35 Item36
#'   Item37 Item38 Item39 Item40 Item41 Item42
#'   Item43 Item44 Item45 Item46 Item47 Item48
#'   Item49 Item50 Item51 Item52 Item53 Item54
#'   Item55 Item56;",
#'   OUTPUT = "stdyx;",
#'   rdata = simulated_test_data
#' )
#'
#' # finally, call the function with some minor changes to the default values.
#' abilityShortForm = antcolony.mplus(ants = 3, evaporation = 0.7,
#' mplus = initial.MplusAutomation.model,list.items = list.items, full = 56,
#' i.per.f = 15, factors = 'Ability', steps = 3, max.run = 50, resultfile = NULL,
#' summaryfile = 'C:/Users/lordmaxwell/Desktop/summary.txt',
#' min.CFI = 0.95, min.TLI = 0.95, max.RMSEA = 0.06,
#' feedbackfile = 'C:/Users/lordmaxwell/Desktop/iteration.html', Mplus.Automation=TRUE,
#' dataOut = 'exampleModel.dat',
#' modelOut = 'exampleModel.inp')
#' }
#'@export
#'

antcolony.mplus = function(ants = 20, evaporation = 0.95, mplus = NULL, list.items = NULL, full = NULL,
                           i.per.f = NULL, factors = NULL, steps = 50, max.run = 1000,
                           resultfile = NULL, summaryfile = 'summary.txt',
                           min.CFI = 0.95, min.TLI = 0.95, max.RMSEA = 0.06,
                           feedbackfile = 'iteration.html', loc.gammas,
                           loc.variances, predictors, var.predictors,
                           Mplus.Automation=FALSE, dataOut = "tempModel.dat", modelOut = "tempModel.inp") {

  # create initial, empty summaryfile to be used
  write(x = "", file = summaryfile)

  #creates the table of initial pheromone levels.
  include = rep(2,full)
  #puts initial best solution (all items selected).
  best.so.far.solution = include
  #starts counting the iterations
  count = 1
  #starts counting continuous runs regardless of result.
  run = 1
  #creates a vector with all items.
  item.vector = unlist(list.items, use.names = F)
  #defines initial best so far (overall) pheromone
  best.so.far.pheromone = 0
  #defines initial best pheromone for the current trial of n ants.
  best.pheromone = 0
  #defines initial solutions.
  previous.solution = include
  step = 0
  #creates selected.items list
  selected.items = list()

  #check whether to use MplusAutomation or to call Mplus directly through R
  if(Mplus.Automation==FALSE){

    #reads mplus syntax.
    #setwd("C:/Program Files/Mplus")
    input = scan(file = paste(mplus,".inp",sep=""),what="character", sep ="\n", quote = NULL)

    #creates a list to store factors.
    selected.items = list.items

    #starts loop through iterations.
    while (count <= steps) {
      if(run <= max.run){

      #sends a number of ants per time.
      ant  = 1
      while (ant <= ants) {

        print(paste0("Count Number ", count, " and Ant Number", ant, " and Run Number ", run, "."))

        #selects items for all factors.
        for (factor in 1:length(list.items)) {

          #selects the items for a short form for the factor
          positions = is.element(item.vector,list.items[[factor]])
          prob = include[positions]/sum(include[positions])


          items = sample(list.items[[factor]], size = i.per.f[factor],replace = F,prob)

          #stores selected items.
          selected.items[[factor]] = items


          #replaces the mplus syntax for factor specification.
          #make sure the factor name provided is the same as the one in the MPLUS file.
          factor.position = grep(paste(factors[factor],"BY"),input,ignore.case=T)
          input[factor.position]  = paste(c(factors[factor],"BY",items,";"),collapse =" ")

          #finishes loop
        }

        selected.items = lapply(selected.items,sort)
        selected.vector = unlist(selected.items, use.names = F)
        select.indicator = is.element(item.vector,selected.vector)
        notselect.indicator = (select.indicator == FALSE)

        #MODIFY MPLUS SYNTAX.
        #when constructing mplus file, leave two lines blank after the usevariables command.


        #make sure there are at least three lines of space between them.
        #defines variables to be used.
        position1 = grep("usevariables",input,ignore.case=T)
        input[position1] = paste("usevariables are ",predictors)
        input[position1+1] = paste(c(selected.vector[1:round(sum(i.per.f)/2,0)]),
                                   collapse = " ")
        input[position1+2] = paste(c(selected.vector[(round(sum(i.per.f)/2,0)+1):sum(i.per.f)],";"),
                                   collapse = " ")
        position2 = grep("categorical",input,ignore.case=T)
        input[position2+1] = paste(c(selected.vector[1:round(sum(i.per.f)/2,0)]),
                                   collapse = " ")
        input[position2+2] = paste(c(selected.vector[(round(sum(i.per.f)/2,0)+1):sum(i.per.f)],";"),
                                   collapse = " ")


        #writes the mplus syntax for fitting the short form.
        write.table(input, file = paste(mplus,".inp",sep=""), quote = F,
                    col.names = F, row.names = F)


        #Run MPLUS with the syntax file specified.
        system(paste("mplus.exe ",mplus,".inp",sep=""), wait = TRUE)
        outfile = c(paste(mplus,".out",sep=""))

        #read the output file to verify the outcome of the analysis.
        output = scan(file = outfile, what = character(), sep = "\n")
        #check if the outcome was a non-positive definite matrix.
        outcome1 = grep("WARNING",output,ignore.case=T)
        #check if there was non-convergence.
        outcome0 = grep("EXCEEDED",output,ignore.case=T)
        outcome2 = grep("Model estimation did not terminate normally.  No results were saved", output,ignore.case = T)
        #set pheromone to zero if the there was non-convergence or a non-positive definite solution.
        if ((length(outcome1) == 1) || (length(outcome0) == 1) || (length(outcome2) == 1)) {
          pheromone = 0

          #writes summary about non-convergence and non-positive definite.
          fit.info = matrix(c(select.indicator,run,count,ant,999,999,round((include),5)),1,)
          write.table(fit.info, file = summaryfile, append = T,
                      quote = F, sep = " ", row.names = F, col.names = F)

          #provide feedback about search.
          feedback = c(paste("<h1>",run,"-",count,"-",ant,"-",step,"- Failure", "</h1>" ) )
          write(feedback, file = feedbackfile)

          #finishes if for non-convergent cases.
        } else {

          #The SAVE command at the MPLUS syntax specifies that results should be
          #saved to results.txt.
          #scans all results.
          results = scan(resultfile)
          #read the CFI,TLI and RMSEA.
          CFI.row <- output[grep("CFI[ ]{1,}", output)]
          CFI <- as.numeric(regmatches(CFI.row, regexpr("[0-9\\.]{5}",CFI.row)))

          TLI.row <- output[grep("TLI[ ]{1,}", output)]
          TLI <- as.numeric(regmatches(TLI.row, regexpr("[0-9\\.]{5}",TLI.row)))

          RMSEA.row <- output[grep("RMSEA", output)+1]
          RMSEA <- as.numeric(regmatches(RMSEA.row, regexpr("[0-9\\.]{5}",RMSEA.row)))

          #reads the regression coefficients (gammas)
          gammas = results[loc.gammas]

          #reads the residual variances of the latent variables.
          res.variances = results[loc.variances]


          #obtains the variance explained by the predictors using path tracing
          var.explained = t(t(gammas)^2*var.predictors)
          var.explained = apply(var.explained,1,sum)

          #obtains the total variance of the latent variable
          #residual variance + variance explained
          variances = res.variances + var.explained

          #standardizes the gammas.
          std.gammas = gammas/sqrt(variances)

          #saves summary information about the selected items and the RMSEA they generated.
          fit.info = matrix(c(select.indicator,run,count,ant,CFI,TLI,RMSEA,mean(std.gammas),
                              round(include,2)),1,)

          write.table(fit.info, file = summaryfile, append = T,
                      quote = F, sep = " ", row.names = F, col.names = F)

          #provide feedback about search.
          feedback = c(paste("<h1>","run:",run,"count:",count,"ant:",ant,"step:",step,"<br>",
                             "CFI:",CFI,"TLI:",TLI,"RMSEA:",RMSEA,"<br>",
                             "GAMMA:",mean(std.gammas), "</h1>" ) )
          write(feedback, file = feedbackfile)

          print(feedback)
          #implements fit requirement.
          if ((CFI < min.CFI)|(TLI < min.TLI)|(RMSEA > max.RMSEA)) {
            pheromone = 0 } else {
              #calculate pheromone (mean of standardized gammas).
              pheromone = mean(std.gammas)
            }
          #adjusts count based on outcomes and selects best solution.
          if (pheromone >= best.pheromone) {

            #updates solution.
            best.solution = select.indicator
            #updates best fit indices
            best.RMSEA = RMSEA
            best.CFI = CFI
            best.TLI = TLI
            #updates best pheromone
            best.pheromone = pheromone
          }

          #Move to next ant.
          ant = ant + 1

          #end else clause for converged solutions
        }
        #ends loop through ants.
        run = run + 1

      }

      #adjusts pheromone only if the current pheromone is best than the previous.
      if (best.pheromone > best.so.far.pheromone) {

        #implements pheromone evaporation.
        include = include*evaporation

        #Adjusts the pheromone levels.
        include.pheromone = best.solution * best.pheromone*(0.1*run)


        #updates pheromone.
        include = include + include.pheromone

        #updates best so far solution and pheromone, with corresponding RMSEA.
        best.so.far.solution = best.solution
        best.so.far.pheromone = best.pheromone
        best.so.far.RMSEA = best.RMSEA
        best.so.far.CFI = best.CFI
        best.so.far.TLI = best.TLI
        #re-starts count.
        count = 1

        #end if clause for pheromone adjustment.
      } else {

        #advances count.
        count = count + 1


        #adds more pheromone to the best so far solution.
        #include.pheromone = best.so.far.solution * best.so.far.pheromone

        #updates pheromone.
        #include = include + include.pheromone

        #finish else clause.
      }

      #ends loop.
      } else{
        stop("Maximum number of runs reached.")
        }
    }
  } else if (Mplus.Automation==TRUE){
    if(!requireNamespace("MplusAutomation", quietly = TRUE)){
      stop("The `MplusAutomation` package is required to use this function. Please install `MplusAutomation`, then try to use this function again.")
    }

    #starts loop through iterations.
    while (count <= steps) {
      if(run <= max.run){

      #sends a number of ants per time.
      ant  = 1
      while (ant <= ants) {
        print(paste0("Count Number ", count, " and Ant Number ", ant, " and Run Number ", run, "."))

        #selects items for all factors.
        for (factor in 1:length(list.items)) {
          #selects the items for a short form for the factor
          positions = is.element(item.vector,list.items[[factor]])
          prob = include[positions]/sum(include[positions])
          items = sample(list.items[[factor]], size = i.per.f[factor],replace = F,prob)
          #stores selected items.
          selected.items[[factor]] = items
          #finishes loop
        }

        #replaces the mplus syntax for factor specification.
        #make sure the factor name provided is the same as the one in the mplusObject!
        measurement.pattern = c()
        for(i in 1:length(factors)) measurement.pattern[i] = paste0("(",factors[i], " BY)[ A-z0-9]*")
        updated.model =  gsub("\\n", " ", mplus$MODEL)
        for(i in 1:length(factors)) updated.model = gsub(measurement.pattern[i], paste(c(factors[i], "BY", selected.items[[i]]), collapse = " "), updated.model)
        updated.model = gsub('(;)(\\s|$)', '\\1\n', updated.model)
        updated.model = gsub('(.{1,80})(\\s|$)', '\\1\n', updated.model)
        updated.model = gsub(';', ';\n', updated.model)

        #writes the mplus syntax for fitting the short form.
        new.input = mplus
        new.input$MODEL = updated.model

        factor.patterns = c()
        for(i in 1:length(factors)) factor.patterns[i] = paste0("(", factors[i], " BY|", factors[i], " ON| ON ", factors[i], ")", collapse = "")

        new.usevariables = updated.model
        for(i in 1:length(factors)) new.usevariables <- gsub(factor.patterns[i], "", new.usevariables)
        new.usevariables = gsub(";\\n", "", new.usevariables)
        new.usevariables = gsub("\\n", " ", new.usevariables)
        new.usevariables = unlist(lapply(strsplit(new.usevariables, " "), unique))
        new.usevariables = new.usevariables[new.usevariables != ""]
        new.usevariables = new.usevariables[new.usevariables!="ON"]
        new.usevariables = new.usevariables[!(new.usevariables %in% factors)]
        new.input$usevariables = new.usevariables

        outcome.vars = unlist(regmatches(updated.model, gregexpr("[A-z0-9 ]{1,} (ON) [A-z0-9 ]{1,}", updated.model)))
        for(i in 1:length(factors)) outcome.vars = gsub(factors[i], "", outcome.vars)
        outcome.vars = gsub("( ON){1,}", "", outcome.vars);outcome.vars = unique(gsub(" ", "", outcome.vars))
        if(length(outcome.vars)==0) outcome.vars = "none"
        dependent.vars = new.usevariables[new.usevariables!=outcome.vars]
        if(grepl(outcome.vars, mplus$VARIABLE)){
          new.input$VARIABLE = gsub('(.{1,80})(\\s|$)', '\\1\n', gsub('(?<= ){1}[A-z0-9 - \\n]{1,}', paste(c(outcome.vars, dependent.vars), collapse = " "), new.input$VARIABLE, perl = TRUE))
        } else {
          new.input$VARIABLE = gsub('(.{1,80})(\\s|$)', '\\1\n', gsub('(?<= ){1}[A-z0-9 - \\n]{1,}', paste(dependent.vars, collapse = " "), new.input$VARIABLE, perl = TRUE))
        }

        selected.items = lapply(selected.items,sort)
        selected.vector = unlist(selected.items, use.names = F)
        select.indicator = is.element(item.vector,selected.vector)
        notselect.indicator = (select.indicator == FALSE)

        #Run MPLUS with the updated model just specified.
        current.ant.model = MplusAutomation::mplusModeler(new.input, dataout = dataOut, modelout = modelOut, run = 1, writeData = "always", hashfile = FALSE)

        #check if the outcome was a non-positive definite matrix.
        output = paste(c(unlist(current.ant.model$results$warnings),unlist(current.ant.model$results$errors)), collapse = " ")
        outcome1 = grep("WARNING",output,ignore.case=T)
        #check if there was non-convergence.
        outcome0 = grep("EXCEEDED",output,ignore.case=T)
        outcome2 = grep("Model estimation did not terminate normally.  No results were saved", output,ignore.case = T)
        #set pheromone to zero if the there was non-convergence or a non-positive definite solution.
        if ((length(outcome1) == 1) || (length(outcome0) == 1) || (length(outcome2) == 1)) {
          pheromone = 0

          #writes feedback about non-convergence and non-positive definite.
          fit.info = matrix(c(select.indicator,run,count,ant,999,999,round((include),5)),1,)
          write.table(fit.info, file = summaryfile, append = T,
                      quote = F, sep = " ", row.names = F, col.names = F)

          #provide feedback about search.
          feedback = c(paste("<h1> Run",run,"- Count",count,"- Ant",ant,"- Step",step,"- Failure", "</h1>" ) )
          write(feedback, file = feedbackfile)

          #finishes if for non-convergent cases.
        } else {

          #read the CFI,TLI and RMSEA.
          CFI = current.ant.model$results$summaries$CFI
          TLI = current.ant.model$results$summaries$TLI
          RMSEA = current.ant.model$results$summaries$RMSEA_Estimate

          # extract the standardized regression coefficients
          std.gammas = current.ant.model$results$parameters$std[grep(".ON", current.ant.model$results$parameters$unstandardized$paramHeader),"est"]
          if(length(std.gammas)==0){
            std.gammas = current.ant.model$results$parameters$r2$est
          }

          #saves information about the selected items and the RMSEA they generated.
          fit.info = matrix(c(select.indicator,run,count,ant,CFI,TLI,RMSEA,mean(std.gammas),
                              round(include,2)),1,)

          write.table(fit.info, file = summaryfile, append = T,
                      quote = F, sep = " ", row.names = F, col.names = F)

          #provide feedback about search.
          feedback = c(paste("<h1>","run:",run,"count:",count,"ant:",ant,"step:",step,"<br>",
                             "CFI:",CFI,"TLI:",TLI,"RMSEA:",RMSEA,"<br>",
                             "GAMMA:",mean(std.gammas), "</h1>" ) )
          write(feedback, file = feedbackfile)

          print(feedback)
          #implements fit requirement.
          if ((CFI < min.CFI)|(TLI < min.TLI)|(RMSEA > max.RMSEA)) {
            pheromone = 0 } else {
              #calculate pheromone (mean of standardized gammas).
              pheromone = mean(std.gammas)
            }
          #adjusts count based on outcomes and selects best solution.
          if (pheromone >= best.pheromone) {

            #updates solution.
            best.solution = select.indicator
            #updates best RMSEA.
            best.RMSEA = RMSEA
            best.CFI = CFI
            best.TLI = TLI
            #updates best pheromone
            best.pheromone = pheromone
          }

          #Move to next ant.
          ant = ant + 1

          #end else clause for converged solutions
        }

        #ends loop through ants.
        run = run + 1

      }

      #adjusts pheromone only if the current pheromone is best than the previous.
      if (best.pheromone > best.so.far.pheromone) {

        #implements pheromone evaporation.
        include = include*evaporation

        #Adjusts the pheromone levels.
        include.pheromone = best.solution * best.pheromone*(0.1*run)


        #updates pheromone.
        include = include + include.pheromone

        #updates best so far solution and pheromone, with corresponding RMSEA.
        best.so.far.solution = best.solution
        best.so.far.pheromone = best.pheromone
        best.so.far.RMSEA = best.RMSEA
        best.so.far.CFI = best.CFI
        best.so.far.TLI = best.TLI
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
      }

      } else  {
        stop("Maximum number of runs reached.")
        }
      #ends loop.

    }
  }


  final.solution = matrix(c(best.so.far.RMSEA,best.so.far.CFI,best.so.far.TLI,best.so.far.pheromone,best.so.far.solution),1, dimnames = list(NULL, c("RMSEA","CFI","TLI","mean_gamma",item.vector)))

  #FINISH FUNCTION.
  return(final.solution)
}

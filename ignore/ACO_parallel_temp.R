start.time <- Sys.time()
cl<-makeCluster(6,type="PSOCK")
registerDoParallel(cl)

foreach(ant = 1:ants, .inorder = F, .combine = rbind) %dopar% {
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
  # notselect.indicator = (select.indicator == FALSE)
  
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
    
    # adjusts count based on outcomes and selects best solution.
    if (pheromone >= best.pheromone) {
      
      # updates solution.
      best.solution <- select.indicator
      # updates best RMSEA.
      best.fit.indices <- antcolony.lavaan.env$model.fit
      # updates best pheromone
      best.pheromone <- pheromone
    }
    
    # Move to next ant.
    
    # end else clause for converged solutions
  }
  
  # evaluates the criterion to stop the interactions
  # the criterion is that the scale selected cannot change in the specified
  # number of steps.
  if (sum(previous.solution != select.indicator) == 0) {
    step <- step + 1
  } else {
    step <- 1
  }
  
  previous.solution <- select.indicator
  return = matrix(c(
    select.indicator,
    run, count, ant,
    antcolony.lavaan.env$model.fit,
    pheromone,
    mean(antcolony.lavaan.env$std.gammas),
    mean(antcolony.lavaan.env$std.betas),
    mean(antcolony.lavaan.env$variance.explained)
  ), 1, )
  colnames(return) = c(item.vector, "run", "count", "ant",
                       names(antcolony.lavaan.env$model.fit), "pheromone", "mean.std.gammas",
                       "mean.std.betas", "mean.var.exp")
  return
}
registerDoSEQ()
stopCluster(cl)
Sys.time() - start.time
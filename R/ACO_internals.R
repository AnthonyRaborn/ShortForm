antcolonyNewModel <- function(itemList, itemVector, includedItems, model, itemCount, factorNames, bifactor = NULL) {
  all.items <- c()

  selected.items <- vector("list", length(itemList))

  if (!is.character(bifactor)) {
    for (factor in 1:length(itemList)) {

      # selects the items for a short form for the factor
      positions <- is.element(itemVector, itemList[[factor]])
      prob <- includedItems[positions] / sum(includedItems[positions])

      items <- sample(itemList[[factor]], size = itemCount[factor], replace = F, prob)

      # stores selected items.
      selected.items[[factor]] <- items


      # replaces the lavaan syntax for factor specification.
      factor.position <- grep(paste(factorNames[factor], "[ ]{0,}=~"), model, ignore.case = T)
      model[factor.position] <- paste(factorNames[factor], "=~", paste(items, collapse = " + "))
      all.items <- c(all.items, items)
    }
  } else {
    bifactor.items <- c()
    for (factor in 1:(length(itemList) - 1)) {
      # selects the items for a short form for the factor
      positions <- is.element(itemVector, itemList[[factor]])
      prob <- includedItems[positions] / sum(includedItems[positions])

      items <- sample(itemList[[factor]], size = itemCount[factor], replace = F, prob)

      # stores selected items.
      selected.items[[factor]] <- items
      bifactor.items <- c(bifactor.items, items)

      # replaces the lavaan syntax for factor specification.
      factor.position <- grep(paste(factorNames[factor], "=~"), model, ignore.case = T)
      model[factor.position] <- paste(factorNames[factor], "=~", paste(items, collapse = " + "))
      all.items <- c(all.items, items)
    }

    # add bifactor items
    model[grep(paste(bifactor, "=~"), model)] <- paste(bifactor, "=~", paste(all.items, collapse = " + "))
    selected.items[[length(itemList)]] <- bifactor.items
  }

  return(list(
    "input" = model,
    "selected.items" = selected.items,
    "all.items" = all.items
  ))
}

modelWarningCheck <- function(expr) {
  warn <- err <- c()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      err <<- append(err, regmatches(paste(e), gregexpr("ERROR: [A-z ]{1,}", paste(e))))
      NULL
    }),
    warning = function(w) {
      warn <<- append(warn, regmatches(paste(w), gregexpr("WARNING: [A-z ]{1,}", paste(w))))
      invokeRestart("muffleWarning")
    }
  )
  list(lavaan.output = value, warnings <- as.character(unlist(warn)), errors <- as.character(unlist(err)))
}

modelInfoExtract <- function(modelCheckObj, fitIndices) {

  # first, fit indices
  model.fit <- lavaan::fitMeasures(modelCheckObj$lavaan.output, fitIndices)

  # next, gamma/beta/variances
  # estimate the standardized coefficients of the variables
  standard.coefs <- lavaan::standardizedSolution(modelCheckObj$lavaan.output, se = FALSE, zstat = FALSE, pvalue = FALSE, remove.def = TRUE)
  # extract the regression coefficients
  std.gammas <- standard.coefs[which(standard.coefs[, 2] == "=~"), ]$est.std
  std.betas <- standard.coefs[which(standard.coefs[, 2] == "~"), ]$est.std
  std.reg.coef <- standard.coefs[which(standard.coefs[, 2] == "~" | standard.coefs[, 2] == "=~"), ]$est.std

  # obtains the variance explained ("rsquare") from lavaan
  variance.explained <- lavaan::lavInspect(modelCheckObj$lavaan.output, "rsquare")

  return(list(
    "model.fit" = model.fit,
    "std.gammas" = std.gammas,
    "std.betas" = std.betas,
    "std.reg.coef" = std.reg.coef,
    "variance.explained" = variance.explained
  ))
}

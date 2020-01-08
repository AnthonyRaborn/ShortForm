start.time = Sys.time()
mapply(
  assign,
  names(lavaan.model.specs),
  lavaan.model.specs,
  MoreArgs = list(envir = environment())
)

allItems <-
  colnames(originalData)

# extract the latent factor syntax
mapply(
  assign,
  c("factors", "itemsPerFactor"),
  syntaxExtraction(initialModelSyntaxFile = initialModel, items = allItems),
  MoreArgs = list(envir = parent.frame())
)

# save the external relationships
vectorModel <- unlist(strsplit(x = initialModel, split = "\\n"))
externalRelation <- vectorModel[grep("[ ]{0,}(?<!=)~ ", vectorModel, perl = T)]
factorRelation <- vectorModel[grep("[ ]{0,}~~ ", vectorModel)]

randomInitialModel <- function(init.model = initialModel,
                               maxItems = numItems,
                               initialData = originalData,
                               bifactorModel = bifactor) {
  # extract the latent factor syntax
  mapply(
    assign,
    c("factors", "itemsPerFactor"),
    syntaxExtraction(initialModelSyntaxFile = initialModel, items = allItems),
    MoreArgs = list(envir = parent.frame())
  )
  
  # save the external relationships
  vectorModel <- unlist(strsplit(x = initialModel, split = "\\n"))
  externalRelation <- vectorModel[grep("[ ]{0,}(?<!=)~ ", vectorModel, perl = T)]
  factorRelation <- vectorModel[grep("[ ]{0,}~~ ", vectorModel)]
  
  # reduce the number of items for each factor according to maxItems
  newItemsPerFactor <- list()
  for (i in 1:length(itemsPerFactor)) {
    newItemsPerFactor[[i]] <- sample(x = unique(unlist(itemsPerFactor[i])), size = unlist(maxItems[i]))
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
      factors[i], "=~",
      paste(newItemsPerFactor[[i]], collapse = " + ")
    )
  }
  newModelSyntax <- c(newModelSyntax, externalRelation, factorRelation)
  newModelSyntax <- paste(newModelSyntax, collapse = "\n")
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
    )
  )
  newModel$model.syntax <- newModelSyntax
  
  return(newModel)
}

init.model <-
  randomInitialModel()

best.obj <- all.obj <- current.obj <- criterion(init.model$model.output)
best.mod <- current.model <- init.model$model.output
best.syntax <- current.syntax <- init.model$model.syntax
names(itemsPerFactor) <- factors

if (verbose == TRUE) {
  print("Initial short form selected:")
  cat(current.syntax)
}

tabu.list <- vector("list")

all.syntax <-
  vector(length = niter + 1)
all.syntax[1] <-
  current.syntax

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

# Do iterations
for (it in 1:niter) {
  cat(paste0("\rRunning iteration ", it, " of ", niter, ".   "))
  # Loop through all neighbors
  tmp.obj <- vector("numeric")
  tmp.mod <- vector("list", length(factors))
  tmp.syntax <- vector("list", length(factors))
  
  for (j in 1:length(factors)) {
    currentModelSyntax <-
      strsplit(current.syntax, split = "\n")[[1]]
    
    currentFactor <-
      factors[j]
    otherFactors <-
      factors[-j]
    
    currentItems <-
      itemsPerFactor[[ factors[j] ]][
        itemsPerFactor[[ factors[j] ]] %in%
          unlist(
            strsplit(
              grep(paste0(factors[j], ".*=~"), currentModelSyntax, value = T),
              split = " "
            )
          )
        ]
    removedItems <-
      itemsPerFactor[[ factors[j] ]][
        !itemsPerFactor[[ factors[j] ]] %in%
          unlist(
            strsplit(
              grep(paste0(factors[j], ".*=~"), currentModelSyntax, value = T),
              split = " "
            )
          )
        ]
    
    itemChange <-
      expand.grid(currentItems, removedItems)
    
    tmp.mod[[j]] <- vector("list", length = nrow(itemChange))
    tmp.syntax[[j]] <- vector("list", length = nrow(itemChange))
    
    
    tmp <-
      foreach::foreach(k = 1:nrow(itemChange), .inorder = F) %dopar% {
      newItems <-
        gsub(pattern = paste0(itemChange[k, 1]), replacement = itemChange[k, 2], currentItems)
      newCurrentFactor <-
        paste(
          factors[j], "=~",
          paste(newItems, collapse = " + ")
        )
      
      currentModelSyntax[grepl(paste0(factors[j], ".*=~"), currentModelSyntax)] <-
        newCurrentFactor
      
      newModelSyntax <-
        paste(currentModelSyntax,
              collapse = " \n"
        )
      
      fitmodel <-
        modelWarningCheck(
          lavaan::lavaan(
            model = newModelSyntax,
            data = originalData,
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
            warn = FALSE
          )
        )$model.output
      
      if (is.null(fitmodel)) {
        fit.val <- NA
      } else if (fitmodel@Fit@converged & !any(is.na(fitmodel@Fit@se))) {
        fit.val <- criterion(fitmodel)
      } else {
        fit.val <- NA
      }
      
      tmp <-
        list(
          tmp.obj = fit.val,
          tmp.mod = fitmodel,
          tmp.syntax = newModelSyntax
          )
      # tmp.obj <- c(tmp.obj, fit.val)
      # tmp.mod[[j]][[k]] <- fitmodel
      # tmp.syntax[[j]][[k]] <- newModelSyntax
    }
  }
  tmp.mod <- lapply(tmp, '[[', 'tmp.mod')
  tmp.syntax <- lapply(tmp, '[[', 'tmp.syntax')
  tmp.obj <- unlist(lapply(tmp, '[[', 'tmp.obj'))
  # tmp.mod <- unlist(tmp.mod)
  # tmp.syntax <- unlist(tmp.syntax)
  # Check which indices result in a valid objective function
  valid <- which(!is.na(tmp.obj))
  
  # Get just models not on Tabu list
  if (length(tabu.list) != 0) {
    newValid <- c()
    for (i in valid) {
      notTabu <- c()
      for (j in 1:length(tabu.list)) {
        notTabu <-
          c(
            notTabu,
            !identical(tmp.mod[[i]], tabu.list[[j]])
          )
      }
      newValid <- c(newValid, TRUE %in% notTabu)
    }
  } else {
    newValid <- valid
  }
  
  # Out of valid models, pick model with best objective function value
  indx <- which.min(tmp.obj[newValid])
  
  # Move current state to next model
  current.obj <- (tmp.obj[newValid])[indx]
  all.obj <- c(all.obj, current.obj)
  current.mod <- (tmp.mod[newValid])[[indx]]
  current.syntax <- (tmp.syntax[newValid])[[indx]]
  all.syntax[it + 1] <- current.syntax
  
  if (verbose == TRUE) {
    print(current.mod)
    cat("\nCurrent Model: \n")
    cat(current.syntax)
    cat("\n\n")
  }
  
  # Update Tabu list
  tabu.list <- c(current.mod, tabu.list)
  if (length(tabu.list) > tabu.size) {
    tabu.list <- tabu.list[1:tabu.size]
  }
  
  # Update if the current model is better than the best model
  if (current.obj < best.obj) {
    best.obj <- current.obj
    best.mod <- current.mod
    best.syntax <- current.syntax
    tabu.list <- vector("numeric") # Clear Tabu list
  }
}

ret <-
  new("TS",
      function_call = match.call(),
      all_fit = all.obj,
      best_fit = best.obj,
      best_model = best.mod,
      best_syntax = best.syntax,
      runtime = Sys.time() - start.time
  )

ret

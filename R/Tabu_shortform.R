#' Short Form Tabu Search
#' 
#' Given an initial (full) lavaan model string, the original data, a criterion
#' function to minimize, and some additional specifications,
#' performs a Tabu model specification search. Currently only supports
#' neighbors that are 1 move away from the current model.
#'
#'
#' @param initialModel The initial model (typically the full form) as a character vector with lavaan model.syntax.
#' @param originalData The original data frame with variable names. 
#' @param numItems A numeric vector indicating the number of items to retain for each factor.
#' @param allItems For unidimensional models, a character vector of the item names. For multifactor models, a list of the item names, where each element of the list is a factor.
#' @param criterion A function calculating the objective criterion to minimize. Default is to use the built-in `rmsea` value from `lavaan::fitmeasures()`.
#' @param niter A numeric value indicating the number of iterations (model specification selections) 
#' to perform. Default is 50.
#' @param tabu.size A numeric value indicating the size of Tabu list. Default is 5.
#' @param lavaan.model.specs A list which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#' @param bifactor Logical. Indicates if the latent model is a bifactor model. If `TRUE`, assumes that the last latent variable in the provided model syntax is the bifactor (i.e., all of the retained items will be set to load on the last latent variable).
#'
#' @return A named list with the best value of the objective function (`best.obj`) and the best lavaan model object (`best.mod`).
#' @export
#'
#' @examples 
#' data(exampleAntModel)
#' data(simulated_test_data)
#' tabuResult <- tabuShortForm(initialModel = exampleAntModel,
#'                              originalData = simulated_test_data, numItems = 10,
#'                              allItems = colnames(simulated_test_data)[3:58],
#'                              niter = 10, tabu.size = 3)
#' # lavaan::summary(tabuResult$best.mod) # shows the resulting model

tabuShortForm <-
  function(initialModel,
           originalData,
           numItems,
           allItems,
           criterion = function(x) lavaan::fitmeasures(object = x, fit.measures = 'rmsea'),
           niter = 30,
           tabu.size = 5,
           lavaan.model.specs = list(
             int.ov.free = TRUE,
             int.lv.free = FALSE,
             std.lv = TRUE,
             auto.fix.single = TRUE,
             auto.var = TRUE,
             auto.cov.lv.x = TRUE,
             auto.th = TRUE,
             auto.delta = TRUE,
             auto.cov.y = TRUE,
             ordered = NULL,
             model.type = "cfa"
           ),
           bifactor = FALSE) {
    
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = environment())
    )
    # Initialize objective function and best model
    randomInitialModel = function(init.model = initialModel,
                                  maxItems = numItems,
                                  initialData = originalData,
                                  bifactorModel = bifactor) {
      # extract the latent factor syntax
      mapply(
        assign,
        c("factors", "itemsPerFactor"),
        syntaxExtraction(initialModelSyntaxFile = initialModel),
        MoreArgs = list(envir = parent.frame())
      )
      
      # save the external relationships
      vectorModel =  unlist(strsplit(x = initialModel, split = "\\n"))
      externalRelation = vectorModel[grep(" ~ ", vectorModel)]
      # reduce the number of items for each factor according to maxItems
      newItemsPerFactor = list()
      for (i in 1:length(itemsPerFactor)) {
        newItemsPerFactor[[i]] = sample(x = unique(unlist(itemsPerFactor[i])), size = unlist(maxItems[i]))
      }
      
      if (bifactorModel == TRUE) {
        # if bifactorModel == TRUE, fix the items so the newItems all load on the bifactor
        # assumes that the bifactor latent variable is the last one
        newItemsPerFactor[[length(itemsPerFactor)]] = unlist(newItemsPerFactor[1:(length(itemsPerFactor) - 1)])
      }
      
      # create the new model syntax
      
      newModelSyntax = c()
      for (i in 1:length(factors)) {
        newModelSyntax[i] = paste(factors[i], "=~",
                                  paste(newItemsPerFactor[[i]], collapse = " + "))
      }
      newModelSyntax = paste(newModelSyntax, "\n", externalRelation)
      
      # fit the new model
      newModel = modelWarningCheck(
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
      newModel$model.syntax = newModelSyntax
      
      return(newModel)
    }
    
    initialShortModel <- randomInitialModel()
    best.obj <- current.obj <- criterion(initialShortModel$lavaan.output)
    best.model <- current.model <- initialShortModel$lavaan.output
    
    factors = unique(lavaan::lavaanify(initialModel)[lavaan::lavaanify(initialModel)$op ==
                                                "=~", 'lhs'])
    externalRelation = unlist(strsplit(x = initialModel, split = "\\n"))[grep(" ~ ", unlist(strsplit(x = initialModel, split = "\\n")))]
    
    if (is.list(allItems)) {
      included.items <-
        stringr::str_extract_all(string = initialShortModel$model.syntax,
                                 pattern = paste0("(\\b", paste0(
                                   paste0(unlist(allItems), collapse = "\\b)|(\\b"), "\\b)"
                                 )))
    } else {
      included.items <-
        as.vector(stringr::str_extract_all(
          string = initialShortModel$model.syntax,
          pattern = paste0("(\\b", paste0(
            paste0(allItems, collapse = "\\b)|(\\b"), "\\b)"
          )),
          simplify = TRUE
        ))
    }
    
    # Do iterations
    for (it in 1:niter) {
      print(paste0("Running iteration ", it, "."))
      # Loop through all neighbors
      tmp.obj <- vector("numeric")
      tmp.mod <- list()
      tmp.vec <- list()
      
      if (is.list(allItems)) {
        excluded.items = list()
        for (l in 1:length(included.items)) {
          temp.items = included.items[[l]]
          excluded.items[[l]] = allItems[[l]][which(!(allItems[[l]] %in% temp.items))]
        }
      } else {
        excluded.items = allItems[which(!(allItems %in% included.items))]
      }
      
      if (is.list(allItems)) {
        
        for (f in 1:length(factors)) {
          for (i in 1:numItems[[f]]) {
            for (j in 1:length(excluded.items[[f]])) {
            new.items = included.items
            new.items[[f]][i] = excluded.items[[f]][j]
            newModelSyntax = c()
            for (k in 1:length(factors)) {
              newModelSyntax[k] = paste(factors[k], "=~",
                                        paste(new.items[[k]], collapse = " + "))
            }
            print(newModelSyntax)
            newModelSyntax = stringr::str_flatten(newModelSyntax, " \n ")
            newModelSyntax = paste(newModelSyntax, "\n", externalRelation)
            fitmodel = modelWarningCheck(
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
                estimator = estimator
              )
            )
            
            if (fitmodel$lavaan.output@Fit@converged &
                !any(is.na(fitmodel$lavaan.output@Fit@se))) {
              fit.val <- criterion(fitmodel$lavaan.output)
            } else {
              fit.val <- NA
            }
            
            tmp.obj <- c(tmp.obj, fit.val)
            tmp.mod <- c(tmp.mod, fitmodel$lavaan.output)
            
            
            }}}
        
      # Check which indices result in a valid objective function
      valid <- which(!is.na(tmp.obj))
      
      # Get just models not on Tabu list
      valid <- valid[!(valid %in% tabu.list)]
      
      # Out of valid models, pick model with best objective function value
      indx <- which.min(tmp.obj[valid])
      
      # Move current state to next model
      current.obj <- (tmp.obj[valid])[indx]
      current.mod <- (tmp.mod[valid])[[indx]]
      # current.binvec<-(tmp.vec[valid])[[indx]]
      
      # Update Tabu list
      tabu.list <- c(valid[indx], tabu.list)
      if (length(tabu.list) > tabu.size) {
        tabu.list <- tabu.list[1:tabu.size]
      }
      
      # Update if the current model is better than the best model
      if (current.obj <= best.obj) {
        best.obj <- current.obj
        best.mod <- current.mod
        tabu.list <- vector("numeric") # Clear Tabu list
      }
      }
    }
    
    ret <- list()
    ret$best.obj <- best.obj
    ret$best.mod <- best.mod
    
    return(ret)
  }

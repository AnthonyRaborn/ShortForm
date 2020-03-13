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
#' @param criterion A function calculating the objective criterion to minimize. Default is to use the built-in `rmsea` value from `lavaan::fitmeasures()`.
#' @param niter A numeric value indicating the number of iterations (model specification selections)
#' to perform. Default is 50.
#' @param tabu.size A numeric value indicating the size of Tabu list. Default is 5.
#' @param lavaan.model.specs A list which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#' @param bifactor Logical. Indicates if the latent model is a bifactor model. If `TRUE`, assumes that the last latent variable in the provided model syntax is the bifactor (i.e., all of the retained items will be set to load on the last latent variable).
#' @param verbose Logical. If `TRUE`, prints out the initial short form and the selected short form at the end of each iteration.
#' @param parallel An option for using parallel processing. If \code{TRUE}, the 
#'  function will utilize all available cores. Default is \code{TRUE}.
#'
#' @return A named list with the best value of the objective function (`best.obj`) and the best lavaan model object (`best.mod`).
#' @export
#'
#' @examples
#' shortAntModel <- "
#' Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8
#' Ability ~ Outcome
#' "
#'
#' data(simulated_test_data)
#' tabuResult <- tabuShortForm(
#'   initialModel = shortAntModel,
#'   originalData = simulated_test_data, numItems = 7,
#'   niter = 1, tabu.size = 3
#' )
#' summary(tabuResult) # shows the resulting model
#' \dontrun{
#' # create simulation data from the `psych` package
#' # four factors, 12 items each, 48 total items
#' # factor loading matrix - not quite simple structure
#' fxMatrix <-
#'   matrix(
#'     data = c(
#'       rep(x = c(.9, .7, .5, .3), times = 3),
#'       rep(0.2, times = 3 * 4 * 3), # first factor loadings
#'
#'       rep(0.2, times = 3 * 4),
#'       rep(x = c(.9, .7, .5, .3), times = 3),
#'       rep(0.2, times = 3 * 4 * 2), # second factor loadings
#'
#'       rep(0.2, times = 3 * 4 * 2),
#'       rep(x = c(.9, .7, .5, .3), times = 3),
#'       rep(0.2, times = 3 * 4), # third factor loadings
#'
#'       rep(0.2, times = 3 * 4 * 3),
#'       rep(x = c(.9, .7, .5, .3), times = 3) # fourth factor loadings
#'     ),
#'     ncol = 4
#'   )
#' # factor correlation matrix - all factors uncorrelated
#' PhiMatrix <-
#'   matrix(data = c(
#'     1, 0, 0, 0,
#'     0, 1, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 1
#'   ), ncol = 4)
#' tabuData <-
#'   psych::sim(
#'     fx = fxMatrix,
#'     Phi = PhiMatrix,
#'     n = 1000,
#'     raw = TRUE
#'   )$observed # observed is the simulated observed data
#'
#' # NOTE: you must specify the model such that each factor is on a single line!
#' # otherwise, the algorithm will not work correctly!
#' tabuModel <- "
#' Trait1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 +
#' Item7 + Item8 + Item9 + Item10 + Item11 + Item12
#' Trait2 =~ Item13 + Item14 + Item15 + Item16 + Item17 +
#' Item18 + Item19 + Item20 + Item21 + Item22 + Item23 + Item24
#' Trait3 =~ Item25 + Item26 + Item27 + Item28 + Item29 + Item30 +
#' Item31 + Item32 + Item33 + Item34 + Item35 + Item36
#' Trait4 =~ Item37 + Item38 + Item39 + Item40 + Item41 +
#' Item42 + Item43 + Item44 + Item45 + Item46 + Item47 + Item48
#' "
#'
#' colnames(tabuData) <- paste0("Item", 1:48)
#' # specify the criterion function that the Tabu Search minimizes
#' # wrap this in a tryCatch in case a model does not converge!
#' # specify an appropriate error value: if minimizing, error value must be large
#' tabuCriterion <- function(x) {
#'   tryCatch(lavaan::fitmeasures(object = x, fit.measures = "chisq"),
#'     error = function(e) Inf
#'   )
#' }
#'
#' # use the tabuShortForm function
#' # reduce form to the best 12 items
#' tabuShort <- tabuShortForm(
#'   initialModel = tabuModel, originalData = tabuData,
#'   numItems = c(3, 3, 3, 3),
#'   criterion = tabuCriterion,
#'   niter = 20, tabu.size = 10
#' )
#' }
#'
tabuShortForm <- function(originalData,
                           initialModel,
                           numItems,
                           criterion = function(x) {
                             tryCatch(-lavaan::fitmeasures(object = x, fit.measures = "cfi"),
                               error = function(e) Inf
                             )
                           },
                           niter = 20,
                           tabu.size = 5,
                           lavaan.model.specs = list(
                             int.ov.free = TRUE,
                             int.lv.free = FALSE,
                             std.lv = TRUE,
                             auto.fix.first = FALSE,
                             auto.fix.single = TRUE,
                             auto.var = TRUE,
                             auto.cov.lv.x = TRUE,
                             auto.th = TRUE,
                             auto.delta = TRUE,
                             auto.cov.y = TRUE,
                             ordered = NULL,
                             model.type = "cfa",
                             estimator = "default"
                           ),
                           bifactor = FALSE,
                           verbose = FALSE,
                           parallel = T) {
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
  
  init.model <-
    randomInitialModel(init.model = initialModel,
                       maxItems = numItems,
                       allItems = allItems,
                       initialData = originalData,
                       bifactorModel = bifactor,
                       lavaan.model.specs = lavaan.model.specs)

  best.obj <- all.obj <- current.obj <- criterion(init.model@model.output)
  best.mod <- current.model <- init.model@model.output
  best.syntax <- current.syntax <- init.model@model.syntax
  names(itemsPerFactor) <- factors
  
  convergence <-
    function(x) {
      converge <-
        x@Fit@converged
      se <-
        !any(is.na(x@Fit@se))
      
      ifelse (converge==TRUE & se == TRUE, criterion(x), NA)
    }
  
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
  
  if (parallel) {
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      num_workers <- 2L
    } else {
      # use all cores in devtools::test()
      num_workers <- parallel::detectCores()
    }
  } else {
    num_workers = 1
    
  }
  
  cl <- parallel::makeCluster(num_workers,type="PSOCK", outfile = "")
  doSNOW::registerDoSNOW(cl)
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

      for (k in 1:nrow(itemChange)) {
        newItems <-
          gsub(pattern = paste0(itemChange[k, 1], "\\b"), replacement = itemChange[k, 2], currentItems)
        newCurrentFactor <-
          paste(
            factors[j], "=~",
            paste(newItems, collapse = " + ")
          )

        currentModelSyntax[grepl(paste0(factors[j], ".*=~"), currentModelSyntax)] <-
          newCurrentFactor

        newModelSyntax <-
          paste(stringr::str_trim(currentModelSyntax),
            collapse = " \n"
          )
        tmp.syntax[[j]][[k]] <- newModelSyntax
      }
    }
    
    tmp.syntax <-
      unlist(tmp.syntax)
    model = 0L
    tmp.mod <-
      foreach::foreach(model = 1:length(tmp.syntax), .inorder = T) %dopar% {
        fitmodel <-
          modelWarningCheck(
            lavaan::lavaan(
              model = tmp.syntax[model],
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
            ),
            newModelSyntax
          )@model.output

      }

    tmp.obj <- 
      sapply(tmp.mod, convergence)
    tmp.mod <- 
      unlist(tmp.mod)
    tmp.tables <-
      lapply(tmp.mod, 
             lavaan::partable)
    # Check which indices result in a valid objective function
    valid <- which(!is.na(tmp.obj))

    # Get just models not on Tabu list
    if (length(tabu.list) > 0) {
      notTabu <- rep(TRUE, length = length(tmp.tables))
      for (i in valid) {
        for (j in 1:length(tabu.list)) {
          notTabu[i] <-
            ifelse(notTabu[i] == FALSE, 
                   FALSE, 
                   !identical(tmp.tables[[i]], 
                         lavaan::partable(tabu.list[[j]])
                         )
            )
        }
      }
      newValid <- notTabu
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
      print(current.syntax)
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
      tabu.list <- vector("list") # Clear Tabu list
    }
  }
  
  foreach::registerDoSEQ()
  parallel::stopCluster(cl)

  if (class(best.obj)[1] == "lavaan.vector") {
    temp = as.numeric(best.obj)
    names(temp) = names(best.obj)
    best.obj = temp
  }

  ret <-
    new("TS",
        function_call = match.call(),
        all_fit = all.obj,
        best_fit = best.obj,
        best_model = best.mod,
        best_syntax = best.syntax,
        runtime = Sys.time() - start.time,
        final_tabu_list = tabu.list
    )
  
  ret
}

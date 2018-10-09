#' Given a fitted lavaan model, a search table, and an objective criterion,
#' performs a Tabu model specification search. Currently only supports
#' neighbors that are 1 move away from the current model.
#'
#' @param init.model initial fitted model of class lavaan
#' @param ptab search table (e.g., created by search.prep) that lists candidate
#'  parameters that can be modified as part of the search and how the parameters 
#'  can be modified (fixed to what values)
#' @param obj objective function to be MINIMIZED. Any function that takes a 
#' lavaan object as the sole argument and returns a numeric value can be used.
#' @param niter number of Tabu iterations to perform
#' @param tabu.size size of Tabu list
#'
#' @return A list with three elements: best.obj, the numerical value of the best (minimal) objective function achieved; best.mod, the final lavaan model, and best.binvec, a data.frame of the lavaan-formatted parameter table for the final model.
#' @export
#'
#' @examples
#'# load simulation data and select columns used in this example
#'data(simulated_test_data) 
#'tabuData <- simulated_test_data[,c(1:10)]
#'
#'# specify an improper model (improper because data is unidimensional)
#'tabuModel <- "
#'Ability =~ Item1 + Item2 + Item3 + Item4
#'FakeAbility =~ Item5 + Item6 + Item7 + Item8
#'Ability ~ Outcome
#'FakeAbility ~ 0*Outcome"
#'
#'# run the initial misspecified model for Tabu
#'
#'init.model <- lavaan::lavaan(model = tabuModel, data = tabuData, 
#'auto.var=TRUE, auto.fix.first=FALSE, std.lv=TRUE,auto.cov.lv.x=TRUE)
#'
#'# Use search.prep to prepare for the Tabu search
#'ptab <- search.prep(fitted.model = init.model,loadings=TRUE,fcov=TRUE,errors=FALSE)
#'
#'# Perform Tabu Search
#'trial <- tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 2, tabu.size = 5)
#'
#'@author Carl F. Falk
#'@references \url{https://doi.org/10.1080/10705511.2017.1409074}

tabu.sem<-function(init.model,ptab,obj,niter=30,tabu.size=5){
  
  # source(Tabu_internal.R)
  # Initialize objective function and best model
  best.obj<-current.obj<-obj(init.model)
  best.model<-current.model<-init.model
  best.binvec<-current.binvec<-ptab
  
  tabu.list<-vector("numeric")
  
  # Do iterations
  for(it in 1:niter){
    cat(paste0("\rRunning iteration ", it, " of ", niter, ".   "))
    # Loop through all neighbors
    tmp.obj<-vector("numeric")
    tmp.mod<-list()
    tmp.vec<-list()
    for(j in 1:nrow(current.binvec)){
      tmp.binvec<-current.binvec
      bin<- 1-tmp.binvec$free[j]
      tmp.binvec$free[j]<-bin
      fitmodel<-refit.model(init.model,tmp.binvec)
      
      if(fitmodel@Fit@converged&!any(is.na(fitmodel@Fit@se))){
        fit.val<-obj(fitmodel)
      } else {
        fit.val<-NA
      }
      
      tmp.obj<-c(tmp.obj,fit.val)
      tmp.mod[[j]]<-fitmodel
      tmp.vec[[j]]<-tmp.binvec
    }
    
    # Check which indices result in a valid objective function
    valid<-which(!is.na(tmp.obj))
    
    # Get just models not on Tabu list
    valid<-valid[!(valid %in% tabu.list)]
    
    # Out of valid models, pick model with best objective function value
    indx<-which.min(tmp.obj[valid])
    
    # Move current state to next model
    current.obj<-(tmp.obj[valid])[indx]
    current.mod<-(tmp.mod[valid])[[indx]]
    current.binvec<-(tmp.vec[valid])[[indx]]
    
    # Update Tabu list
    tabu.list<-c(valid[indx],tabu.list)
    if(length(tabu.list)>tabu.size){
      tabu.list<-tabu.list[1:tabu.size]
    }
    
    # Update if the current model is better than the best model
    if(current.obj<=best.obj){
      best.obj<-current.obj
      best.mod<-current.mod
      best.binvec<-current.binvec
      tabu.list<-vector("numeric") # Clear Tabu list
    }
  }
  
  ret<-list()
  ret$best.obj<-best.obj
  ret$best.mod<-best.mod
  ret$best.binvec<-best.binvec
  
  return(ret)
}
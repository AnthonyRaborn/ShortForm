#' Given a fitted lavaan model and a search table, refits the model using the search
#' table as specifying what changes should be done (parmeters fixed/freed).
#' 
#' This is not meant to be called explicity as \link{tabu.sem} uses this
#' internally for model refitting.
#'
#' @param fitted.model fitted model of class lavaan
#' @param ptab search table
#'
#' @return An object of class \code{lavaan} if the new model fits, or an object of class \code{try-error} if the model update fails.
#' @family Tabu Search
#' @import lavaan
#' @export
#' 
#'@author Carl F. Falk
#'@references \url{https://doi.org/10.1080/10705511.2017.1409074}

refit.model<-function(fitted.model, ptab){
  
  # Extract parameter table
  tab<-lavaan::parTable(fitted.model)
  
  # Expand to get full table
  fulltab<-lavaan:::lav_partable_full(tab)
  mergetab<-lavaan:::lav_partable_merge(tab,fulltab, remove.duplicated=TRUE, warn=FALSE)
  
  # Modify parameter table based on new input matrix
  
  # Obtain indices for matching
  mergetab$labels<-paste(mergetab$lhs,mergetab$op,mergetab$rhs,sep="")
  ptab$labels<-paste(ptab$lhs,ptab$op,ptab$rhs,sep="")
  midx<-vector("numeric")
  nidx<-vector("numeric")
  for(j in 1:nrow(ptab)){
    idx<-which(ptab$labels[j]==mergetab$labels & ptab$block[j]==mergetab$block)
    if(length(idx)>0){
      midx<-c(midx,idx)
      nidx<-c(nidx,j)
    }
  }
  
  # Replace free parameters
  mergetab$free[midx]<-ptab$free[nidx]
  
  # If some parameters are now fixed; what are they fixed to?<-function
  mergetab$ustart[mergetab$free!=0]<-NA
  mergetab[midx,]$ustart[mergetab[midx,]$free==0]<-ptab[nidx,]$nullval[ptab[nidx,]$free==0]
  
  # Get rid of old parameter estimates and starting values
  mergetab$est<-NULL
  mergetab$se<-NULL
  mergetab$start<-NULL
  mergetab$labels<-NULL
  
  # Re-order values in mergetab
  lvnames<-lavaan::lavNames(mergetab,"lv")
  ovnames<-lavaan::lavNames(mergetab,"ov")
  mergetab<-mergetab[order(mergetab$block, # by group (or block)
                           is.na(match(mergetab$lhs,c(lvnames))), # put latent vars first
                           mergetab$op, # by op w/in lv and ov sections
                           match(mergetab$lhs,c(lvnames,ovnames)), # then by lv and ov names on lhs
                           match(mergetab$rhs,c(lvnames,ovnames))),] # then by same on rhs
  
  # Refit model
  prevmodel<-as.list(fitted.model@call)
  prevmodel$model<-mergetab
  newmod<-try(do.call(eval(parse(text="lavaan::lavaan")),prevmodel[-1]),silent=TRUE)
  
  return(newmod)
}

#' A series of internal functions used by the Tabu search.
#' Directly accessing these functions is not advised unless you know what you are doing.
#' 
#' @keywords internal
#' 

# Internal function used to match parameters in label against
# those in a search table (ptab)
par.matches<-function(ptab,label,block=NULL){
  
  # Collapse whitespace in label (if any)
  label<-gsub("[[:space:]]","", label)
  
  # Look through lhsoprhs
  lhsoprhs<-paste(ptab$lhs,ptab$op,ptab$rhs,sep="")
  
  # Look through labels
  matches<-label==lhsoprhs | label==ptab$label
  
  # Check against block
  if(!is.null(block)){
    matches <- matches & ptab$block==block
  }
  return(matches)
}

# Internal function that modifies search table
manip.ptab<-function(ptab,label,task,nullval=NULL,block=NULL){
  
  if(!task %in% c("remove","free","fix","nullval")){
    stop()
  }
  
  matches<-par.matches(ptab,label,block=block)
  
  nmatches<-sum(matches)
  if(nmatches>1){
    warning("More than one match for parameter found")
  } else if (nmatches<1){
    warning("No matches for parameter found")
  }
  
  if(task=="remove"){
    # Remove from table
    ptab<-ptab[!matches,]
    
  } else if (task=="free"){
    # Set free
    ptab$free[matches]<-1
    
  } else if (task=="fix"){
    # Fix
    ptab$free[matches]<-0
    
    if(!is.null(nullval)){
      ptab$nullval[matches]<-nullval
    }
  } else if (task=="nullval"){
    # Change nullval
    ptab$nullval[matches]<-nullval
  }
  
  return(ptab)
}

# Given a search table and a label, sets the parameter free in the search table
#
# ptab - search table
# label - combination of lhs, op, and rhs as would appear in lavaan model.syntax
#    or a parameter label from the label column of a lavaan parameter table
# block - optional numeric value specifying the group number to which the parameter corresponds
free.param<-function(ptab, label, block=NULL){
  ptab<-manip.ptab(ptab,label,"free",block=block)
  return(ptab)
}

# Given a search table and a label, sets the parameter fixed in the search table
#
# ptab - search table
# label - combination of lhs, op, and rhs as would appear in lavaan model.syntax
#    or a parameter label from the label column of a lavaan parameter table
# nullval - optional numeric value specifying what the parameter should be set to when fixed (existing value in ptab is otherwise used)
# block - optional numeric value specifying the group number to which the parameter corresponds
fix.param<-function(ptab, label, nullval=NULL, block=NULL){
  ptab<-manip.ptab(ptab,label,"fix",nullval=nullval,block=block)
  return(ptab)
}

# Given a search table and a label, removes the parameter from the search table
#
# ptab - search table
# label - combination of lhs, op, and rhs as would appear in lavaan model.syntax
#    or a parameter label from the label column of a lavaan parameter table
# block - optional numeric value specifying the group number to which the parameter corresponds
rm.param<-function(ptab, label, block=NULL){
  ptab<-manip.ptab(ptab,label,"remove",block=block)
  return(ptab)
}

# Given a search table and a label, sets the value that the parameter would be
# fixed to if the parameter is fixed
#
# ptab - search table
# label - combination of lhs, op, and rhs as would appear in lavaan model.syntax
#    or a parameter label from the label column of a lavaan parameter table
# nullval - numeric value specifying what the parameter should be set to when fixed
# block - optional numeric value specifying the group number to which the parameter corresponds
nullval.param<-function(ptab, label, nullval, block=NULL){
  ptab<-manip.ptab(ptab,label,"nullval",nullval=nullval,block=block)
  return(ptab)
}

# Given a fitted lavaan model and a search table, checks whether any of the
# parameters in the search table are involved in (in)equality constraints
check.const<-function(fitted.model,pars){
  ptab<-lavaan::parTable(fitted.model)
  const<-ptab[ptab$op=="=="|ptab$op==">"|ptab$op=="<",]
  if(nrow(const)>0){
    plabels<-c(pars$label,pars$plabel)
    plabels<-plabels[plabels!=""]
    flag<-FALSE
    for(j in 1:nrow(const)){
      var.lhs<-all.vars(parse(file="", text=const$lhs[j]))
      var.rhs<-all.vars(parse(file="", text=const$rhs[j]))
      if(any(plabels %in% c(var.lhs,var.rhs) ))
        flag<-TRUE
    }
  } else {
    flag<-FALSE
  }
  return(flag)
}

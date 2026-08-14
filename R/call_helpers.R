# fills in any arguments missing from mc (typically match.call(), called
# directly in the target function's own body) using defaults (typically
# formals(), also called directly in the target function's own body) --
# giving a call with every argument present, whether the caller supplied it
# explicitly or it fell back to its default.
#
# mc/defaults must be computed via bare match.call()/formals() at the call
# site, not via sys.call(-1)/sys.function(-1)-style relative frame
# indexing -- that approach breaks the moment the outer call is wrapped in
# tryCatch()/withCallingHandlers()/suppressWarnings(), which insert their
# own frames onto the call stack between the true caller and this function.
#
# also note two R call-object gotchas this works around:
# - formals()[[argName]] for an argument with no default (this includes
#   "..." itself) returns R's internal "missing argument" marker. Merely
#   *referencing* a variable bound to that marker -- even inside identical()
#   -- raises "argument ... is missing, with no default", so the identical()
#   check below is done directly on defaults[[arg]] rather than on an
#   intermediate variable.
# - mc[[arg]] <- value throws "subscript out of bounds" if value is NULL
#   and arg is not already present in mc (this is genuinely different from
#   deleting an *existing* element, which works fine) -- a real risk here
#   since NULL is a legitimate default for several arguments across this
#   package's functions (e.g. bifactor = NULL). mc[arg] <- list(value) sets
#   it correctly either way.
resolvedCall <- function(mc, defaults) {
  missingArgs <- setdiff(names(defaults), names(mc))
  for (arg in missingArgs) {
    if (!identical(defaults[[arg]], quote(expr = ))) {
      mc[arg] <- list(defaults[[arg]])
    }
  }
  mc
}

# pulls a single named argument's value out of a call object (e.g. one
# captured by resolvedCall()) and evaluates it. Intended for simple,
# self-contained argument values (logicals, strings, small literal lists) --
# not for arguments like data or ptab that reference large objects only
# meaningful in the original caller's environment. Returns NULL if the
# argument isn't present in the call at all.
extractCallArg <- function(call, argName, envir = baseenv()) {
  if (!argName %in% names(call)) {
    return(NULL)
  }
  eval(call[[argName]], envir = envir)
}

# normalizes a `criterion` argument (shared by simulatedAnnealing and
# tabuSearch) into a function(fittedModel) -> numeric. `criterion` may be
# either a character lavaan::fitmeasures() name (e.g. "cfi") or an arbitrary
# function. negateCriterion controls the error-fallback sentinel for the
# character case, so a failed refit always compares as "worse" regardless of
# search direction: -Inf when negateCriterion (maximizing), Inf otherwise.
resolveCriterion <- function(criterion, negateCriterion) {
  if (is.character(criterion)) {
    measure <- criterion
    worstValue <- if (isTRUE(negateCriterion)) -Inf else Inf
    return(function(fittedModel) {
      # unclass() strips lavaan::fitmeasures()'s "lavaan.vector" class (kept
      # only for its own pretty-printing) while preserving the name -- some
      # S4 slots downstream (e.g. TS's best_fit) require a plain "numeric"
      tryCatch(
        unclass(lavaan::fitmeasures(fittedModel, measure)),
        error = function(e) worstValue
      )
    })
  }
  if (is.function(criterion)) {
    return(criterion)
  }
  stop("`criterion` must be a character fit-measure name from lavaan::fitmeasures (e.g. \"cfi\") or a function.")
}

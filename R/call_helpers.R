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

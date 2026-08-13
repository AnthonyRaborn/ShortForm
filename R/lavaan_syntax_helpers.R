replaceItem <- function(items, oldItem, newItem) {
  gsub(pattern = paste0("\\b", oldItem, "\\b"), replacement = newItem, x = items)
}

buildFactor <- function(factorName, items) {
  paste(factorName, "=~", paste(items, collapse = " + "))
}

parTableToSyntax <- function(parTable) {
  parTable <- parTable[parTable$user == 1, ]

  syntaxLines <- c()
  for (currentOp in c("=~", "~", "~~")) {
    opRows <- parTable[parTable$op == currentOp, ]
    if (nrow(opRows) == 0) next

    for (currentLhs in unique(opRows$lhs)) {
      lhsRows <- opRows[opRows$lhs == currentLhs, ]

      fixedValue <- ifelse(is.na(lhsRows$ustart), 0, lhsRows$ustart)
      terms <- ifelse(
        lhsRows$free == 0,
        paste0(fixedValue, "*", lhsRows$rhs),
        lhsRows$rhs
      )

      syntaxLines <- c(
        syntaxLines,
        paste(currentLhs, currentOp, paste(terms, collapse = " + "))
      )
    }
  }

  paste(syntaxLines, collapse = "\n")
}

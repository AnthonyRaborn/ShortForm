# replaceItem ####
test_that(
  "replaceItem replaces an exact item match but not a suffix collision", {
    items <- c("Item1", "SubItem1", "Item2", "Item2b")

    expect_equal(
      replaceItem(items, "Item1", "Item3"),
      c("Item3", "SubItem1", "Item2", "Item2b")
    )

    # unrelated items, including one that merely ends in the old item name,
    # are left untouched
    expect_equal(
      replaceItem(c("x1", "x2", "x3"), "x2", "x9"),
      c("x1", "x9", "x3")
    )
  }
)

# buildFactor ####
test_that(
  "buildFactor formats a factor line from a name and item vector", {
    expect_equal(
      buildFactor("visual", c("x1", "x2", "x3")),
      "visual =~ x1 + x2 + x3"
    )

    expect_equal(
      buildFactor("f", "onlyItem"),
      "f =~ onlyItem"
    )
  }
)

# parTableToSyntax ####
test_that(
  "parTableToSyntax rebuilds valid, refittable lavaan syntax from a parameter table", {
    defaultModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        visual ~~ textual'
    fit <- lavaan::cfa(model = defaultModel, data = lavaan::HolzingerSwineford1939)

    syntaxText <- parTableToSyntax(lavaan::parTable(fit))

    expect_type(syntaxText, "character")
    expect_length(syntaxText, 1)

    # the rebuilt syntax should refit to an equivalent model
    refit <- lavaan::cfa(model = syntaxText, data = lavaan::HolzingerSwineford1939)
    expect_equal(
      lavaan::fitted(fit),
      lavaan::fitted(refit)
    )
  }
)

test_that(
  "parTableToSyntax defaults a freed-to-fixed parameter with no ustart to 0", {
    defaultModel <-
      ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6'
    fit <- lavaan::cfa(model = defaultModel, data = lavaan::HolzingerSwineford1939)
    parTable <- lavaan::parTable(fit)

    # simulate randomNeighborFull()'s free-flip, which does not set ustart
    freeRow <- which(parTable$lhs == "visual" & parTable$rhs == "x2")
    parTable$free[freeRow] <- 0

    syntaxText <- parTableToSyntax(parTable)

    expect_true(grepl("0\\*x2", syntaxText))
  }
)

test_that(
  "parTableToSyntax excludes auto-added (non-user) rows like default variances", {
    defaultModel <- ' visual  =~ x1 + x2 + x3'
    fit <- lavaan::cfa(model = defaultModel, data = lavaan::HolzingerSwineford1939)
    parTable <- lavaan::parTable(fit)

    syntaxText <- parTableToSyntax(parTable)

    # auto.var-generated "x1 ~~ x1" style variance terms should not appear
    expect_false(grepl("x1 ~~ x1", syntaxText))
  }
)

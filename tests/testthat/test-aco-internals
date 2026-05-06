test_that("antcolonyNewModel returns expected structure for non-bifactor model", {
  set.seed(123)

  itemList <- list(
    F1 = c("x1", "x2", "x3"),
    F2 = c("y1", "y2", "y3")
  )

  itemVector <- c("x1", "x2", "x3", "y1", "y2", "y3")
  includedItems <- c(1, 1, 1, 1, 1, 1)

  model <- c(
    "F1 =~ x1 + x2 + x3",
    "F2 =~ y1 + y2 + y3"
  )

  out <- antcolonyNewModel(
    itemList = itemList,
    itemVector = itemVector,
    includedItems = includedItems,
    model = model,
    itemCount = c(2, 2),
    factorNames = c("F1", "F2")
  )

  expect_type(out, "list")
  expect_named(out, c("input", "selected.items", "all.items"))

  expect_length(out$input, 2)
  expect_length(out$selected.items, 2)
  expect_length(out$all.items, 4)

  expect_true(all(out$selected.items[[1]] %in% itemList[[1]]))
  expect_true(all(out$selected.items[[2]] %in% itemList[[2]]))

  expect_length(out$selected.items[[1]], 2)
  expect_length(out$selected.items[[2]], 2)

  expect_match(out$input[1], "^F1 =~ ")
  expect_match(out$input[2], "^F2 =~ ")
})

test_that("antcolonyNewModel is reproducible with a fixed seed", {
  itemList <- list(
    F1 = c("x1", "x2", "x3"),
    F2 = c("y1", "y2", "y3")
  )

  itemVector <- c("x1", "x2", "x3", "y1", "y2", "y3")
  includedItems <- rep(1, 6)

  model <- c(
    "F1 =~ x1 + x2 + x3",
    "F2 =~ y1 + y2 + y3"
  )

  set.seed(123)
  out1 <- antcolonyNewModel(
    itemList = itemList,
    itemVector = itemVector,
    includedItems = includedItems,
    model = model,
    itemCount = c(2, 2),
    factorNames = c("F1", "F2")
  )

  set.seed(123)
  out2 <- antcolonyNewModel(
    itemList = itemList,
    itemVector = itemVector,
    includedItems = includedItems,
    model = model,
    itemCount = c(2, 2),
    factorNames = c("F1", "F2")
  )

  expect_identical(out1, out2)
})

test_that("antcolonyNewModel respects includedItems probabilities", {
  set.seed(123)

  itemList <- list(
    F1 = c("x1", "x2", "x3"),
    F2 = c("y1", "y2", "y3")
  )

  itemVector <- c("x1", "x2", "x3", "y1", "y2", "y3")

  includedItems <- c(
    1, 0, 0,
    0, 1, 0
  )

  model <- c(
    "F1 =~ x1 + x2 + x3",
    "F2 =~ y1 + y2 + y3"
  )

  out <- antcolonyNewModel(
    itemList = itemList,
    itemVector = itemVector,
    includedItems = includedItems,
    model = model,
    itemCount = c(1, 1),
    factorNames = c("F1", "F2")
  )

  expect_equal(out$selected.items[[1]], "x1")
  expect_equal(out$selected.items[[2]], "y2")
  expect_equal(out$all.items, c("x1", "y2"))

  expect_equal(out$input[1], "F1 =~ x1")
  expect_equal(out$input[2], "F2 =~ y2")
})

# test_that("antcolonyNewModel replaces factor lines with optional whitespace before =~", {
#   set.seed(123)

#   itemList <- list(
#     F1 = c("x1", "x2"),
#     F2 = c("y1", "y2")
#   )

#   itemVector <- c("x1", "x2", "y1", "y2")
#   includedItems <- c(1, 0, 0, 1)

#   model <- c(
#     "F1      =~ x1 + x2",
#     "F2=~ y1 + y2"
#   )

#   out <- antcolonyNewModel(
#     itemList = itemList,
#     itemVector = itemVector,
#     includedItems = includedItems,
#     model = model,
#     itemCount = c(1, 1),
#     factorNames = c("F1", "F2")
#   )

#   expect_equal(out$input, c(
#     "F1 =~ x1",
#     "F2=~ y2"
#   ))
# })

# test_that("antcolonyNewModel handles bifactor model syntax", {
#   set.seed(123)

#   itemList <- list(
#     F1 = c("x1", "x2", "x3"),
#     F2 = c("y1", "y2", "y3"),
#     G = c("x1", "x2", "x3", "y1", "y2", "y3")
#   )

#   itemVector <- c("x1", "x2", "x3", "y1", "y2", "y3")

#   includedItems <- c(
#     1, 0, 0,
#     0, 1, 0
#   )

#   model <- c(
#     "F1=~ x1 + x2 + x3",
#     "F2=~ y1 + y2 + y3",
#     "G=~ x1 + x2 + x3 + y1 + y2 + y3"
#   )

#   out <- antcolonyNewModel(
#     itemList = itemList,
#     itemVector = itemVector,
#     includedItems = includedItems,
#     model = model,
#     itemCount = c(1, 1, 2),
#     factorNames = c("F1", "F2", "G"),
#     bifactor = "G"
#   )

#   expect_equal(out$selected.items[[1]], "x1")
#   expect_equal(out$selected.items[[2]], "y2")

#   expect_equal(out$all.items, c("x1", "y2"))

#   expect_equal(out$selected.items[[3]], c("x1", "y2"))

#   expect_equal(out$input, c(
#     "F1 =~ x1",
#     "F2 =~ y2",
#     "G =~ x1 + y2"
#   ))
# })

test_that("antcolonyNewModel errors when all inclusion weights for a factor are zero", {
  itemList <- list(
    F1 = c("x1", "x2"),
    F2 = c("y1", "y2")
  )

  itemVector <- c("x1", "x2", "y1", "y2")
  includedItems <- c(0, 0, 1, 1)

  model <- c(
    "F1 =~ x1 + x2",
    "F2 =~ y1 + y2"
  )

  expect_error(
    antcolonyNewModel(
      itemList = itemList,
      itemVector = itemVector,
      includedItems = includedItems,
      model = model,
      itemCount = c(1, 1),
      factorNames = c("F1", "F2")
    )
  )
})

test_that("modelInfoExtract returns expected components from fitted lavaan model", {
  skip_if_not_installed("lavaan")

  setClass(
    "mockModelCheckObj",
    slots = list(model.output = "ANY"),
    where = topenv()
  )

  data("HolzingerSwineford1939", package = "lavaan")

  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  fit <- lavaan::cfa(
    model,
    data = HolzingerSwineford1939,
    std.lv = TRUE
  )

  modelCheckObj <- new("mockModelCheckObj", model.output = fit)

  out <- modelInfoExtract(
    modelCheckObj = modelCheckObj,
    fitIndices = c("chisq", "df", "cfi", "rmsea")
  )

  expect_type(out, "list")
  expect_named(
    out,
    c(
      "model.fit",
      "std.gammas",
      "std.betas",
      "std.reg.coef",
      "variance.explained"
    )
  )

  expect_named(out$model.fit, c("chisq", "df", "cfi", "rmsea"))

  expect_true(is.numeric(out$model.fit))
  expect_true(is.numeric(out$std.gammas))
  expect_true(is.numeric(out$std.betas))
  expect_true(is.numeric(out$std.reg.coef))

  expect_gt(length(out$std.gammas), 0)
  expect_equal(length(out$std.betas), 0)

  expect_equal(out$std.reg.coef, out$std.gammas)

  expect_true(is.numeric(out$variance.explained))
  expect_gt(length(out$variance.explained), 0)
})

test_that("modelInfoExtract extracts standardized regression coefficients", {
  skip_if_not_installed("lavaan")

  setClass(
    "mockModelCheckObjRegression",
    slots = list(model.output = "ANY"),
    where = topenv()
  )

  data("HolzingerSwineford1939", package = "lavaan")

  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    textual ~ visual
  "

  fit <- lavaan::sem(
    model,
    data = HolzingerSwineford1939,
    std.lv = TRUE
  )

  modelCheckObj <- new("mockModelCheckObjRegression", model.output = fit)

  out <- modelInfoExtract(
    modelCheckObj = modelCheckObj,
    fitIndices = c("cfi", "rmsea")
  )

  expect_named(out$model.fit, c("cfi", "rmsea"))

  expect_gt(length(out$std.gammas), 0)
  expect_gt(length(out$std.betas), 0)

  expect_equal(
    length(out$std.reg.coef),
    length(out$std.gammas) + length(out$std.betas)
  )
})

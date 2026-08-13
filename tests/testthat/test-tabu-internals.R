test_that("par.matches matches lavaan-style parameter syntax", {
  ptab <- data.frame(
    lhs = c("f", "f", "x1", "x2", "f"),
    op = c("=~", "=~", "~~", "~~", "=~"),
    rhs = c("x1", "x2", "x1", "x2", "x1"),
    label = c("lam1", "lam2", "var1", "var2", "lam1_dup"),
    free = c(0, 0, 1, 1, 0),
    nullval = c(NA, NA, 1, 1, 0),
    block = c(1, 1, 1, 2, 2)
  )

  expect_equal(
    par.matches(ptab, "f=~x2"),
    c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_equal(
    par.matches(ptab, " f =~ x2 "),
    c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})


test_that("par.matches matches parameter labels", {
  ptab <- data.frame(
    lhs = c("f", "f", "x1"),
    op = c("=~", "=~", "~~"),
    rhs = c("x1", "x2", "x1"),
    label = c("lam1", "lam2", "var1"),
    free = c(0, 0, 1),
    nullval = c(NA, NA, 1),
    block = c(1, 1, 1)
  )

  expect_equal(
    par.matches(ptab, "lam2"),
    c(FALSE, TRUE, FALSE)
  )
})


test_that("par.matches respects block argument", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x1"),
    label = c("lam1_g1", "lam1_g2"),
    free = c(0, 0),
    nullval = c(NA, NA),
    block = c(1, 2)
  )

  expect_equal(
    par.matches(ptab, "f=~x1"),
    c(TRUE, TRUE)
  )

  expect_equal(
    par.matches(ptab, "f=~x1", block = 2),
    c(FALSE, TRUE)
  )
})


test_that("manip.ptab rejects invalid tasks", {
  ptab <- data.frame(
    lhs = "f",
    op = "=~",
    rhs = "x1",
    label = "lam1",
    free = 0,
    nullval = NA_real_,
    block = 1
  )

  expect_error(
    manip.ptab(ptab, "lam1", task = "bad_task")
  )
})


test_that("free.param sets matched parameter free", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x2"),
    label = c("lam1", "lam2"),
    free = c(0, 0),
    nullval = c(NA, NA),
    block = c(1, 1)
  )

  out <- free.param(ptab, "lam2")

  expect_equal(out$free, c(0, 1))
  expect_equal(out$nullval, ptab$nullval)
})


test_that("fix.param fixes matched parameter and preserves nullval when not supplied", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x2"),
    label = c("lam1", "lam2"),
    free = c(1, 1),
    nullval = c(0.5, 0.7),
    block = c(1, 1)
  )

  out <- fix.param(ptab, "lam2")

  expect_equal(out$free, c(1, 0))
  expect_equal(out$nullval, c(0.5, 0.7))
})


test_that("fix.param fixes matched parameter and updates nullval when supplied", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x2"),
    label = c("lam1", "lam2"),
    free = c(1, 1),
    nullval = c(0.5, 0.7),
    block = c(1, 1)
  )

  out <- fix.param(ptab, "lam2", nullval = 1)

  expect_equal(out$free, c(1, 0))
  expect_equal(out$nullval, c(0.5, 1))
})


test_that("rm.param removes matched parameter", {
  ptab <- data.frame(
    lhs = c("f", "f", "x1"),
    op = c("=~", "=~", "~~"),
    rhs = c("x1", "x2", "x1"),
    label = c("lam1", "lam2", "var1"),
    free = c(1, 1, 1),
    nullval = c(NA, NA, 1),
    block = c(1, 1, 1)
  )

  out <- rm.param(ptab, "lam2")

  expect_equal(nrow(out), 2)
  expect_false("lam2" %in% out$label)
  expect_equal(out$label, c("lam1", "var1"))
})


test_that("nullval.param updates nullval without changing free status", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x2"),
    label = c("lam1", "lam2"),
    free = c(1, 0),
    nullval = c(0.5, 0.7),
    block = c(1, 1)
  )

  out <- nullval.param(ptab, "lam1", nullval = 2)

  expect_equal(out$free, c(1, 0))
  expect_equal(out$nullval, c(2, 0.7))
})


test_that("manip.ptab warns when more than one parameter matches", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x1"),
    label = c("lam1_g1", "lam1_g2"),
    free = c(0, 0),
    nullval = c(NA, NA),
    block = c(1, 2)
  )

  expect_warning(
    free.param(ptab, "f=~x1"),
    "More than one match for parameter found"
  )
})


test_that("manip.ptab warns when no parameter matches", {
  ptab <- data.frame(
    lhs = c("f", "f"),
    op = c("=~", "=~"),
    rhs = c("x1", "x2"),
    label = c("lam1", "lam2"),
    free = c(0, 0),
    nullval = c(NA, NA),
    block = c(1, 1)
  )

  expect_warning(
    free.param(ptab, "f=~x3"),
    "No matches for parameter found"
  )
})

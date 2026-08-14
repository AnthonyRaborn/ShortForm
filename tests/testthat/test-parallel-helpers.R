# setupParallelCluster() / teardownParallelCluster() ####
# Shared by antColony, simulatedAnnealing, and tabuSearch, which
# previously each hand-rolled their own copy of this CRAN-check-aware
# cluster bootstrap. simulatedAnnealing's copy always used `%dopar%` even
# when running serially (no cluster registered), which produced a spurious
# "executing %dopar% sequentially: no parallel backend registered" warning
# on every serial run -- the shared helper fixes that as a side effect,
# since the serial branch always returns `%do%`.

test_that(
  "setupParallelCluster returns a serial (%do%) setup when useParallel = FALSE", {
    result <- setupParallelCluster(FALSE, 4)

    expect_null(result$cluster)
    expect_equal(result$num_workers, 1L)
    expect_identical(result$dopar, foreach::`%do%`)
  }
)

test_that(
  "setupParallelCluster creates a real cluster and %dopar% when useParallel = TRUE", {
    result <- setupParallelCluster(TRUE, 2)
    on.exit(teardownParallelCluster(result$cluster))

    expect_s3_class(result$cluster, "cluster")
    expect_equal(result$num_workers, 2L)
    expect_identical(result$dopar, foreach::`%dopar%`)
  }
)

test_that(
  "setupParallelCluster caps workers at 2 under a CRAN-style core limit", {
    previousLimitCores <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
    Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
    on.exit(
      if (is.na(previousLimitCores)) {
        Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
      } else {
        Sys.setenv("_R_CHECK_LIMIT_CORES_" = previousLimitCores)
      }
    )

    result <- setupParallelCluster(TRUE, 8)
    on.exit(teardownParallelCluster(result$cluster), add = TRUE)

    expect_equal(result$num_workers, 2L)
  }
)

test_that(
  "teardownParallelCluster is a no-op when cluster is NULL", {
    expect_no_error(teardownParallelCluster(NULL))
  }
)

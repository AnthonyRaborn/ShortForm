# sets up a parallel PSOCK cluster for foreach (or a serial fallback),
# handling the standard CRAN-check core limit (_R_CHECK_LIMIT_CORES_) the
# same way across all three algorithms. Pair with teardownParallelCluster()
# once the foreach loop(s) using it are done.
#
# useParallel: TRUE to actually set up a cluster, FALSE for a serial `%do%`
#   fallback with num_workers = 1.
# requestedWorkers: how many workers to use if useParallel is TRUE and not
#   under a CRAN-style core limit (e.g. parallel::detectCores(), or
#   min(setChains, parallel::detectCores())) -- the caller decides this,
#   since the algorithms differ in how they want to size the cluster.
#
# returns a list with:
#   cluster - the PSOCK cluster object (or NULL if useParallel is FALSE,
#     which teardownParallelCluster() uses to know there's nothing to do)
#   num_workers - the number of workers actually in use
#   dopar - the foreach operator to use (`%dopar%` or `%do%`); assign this
#     to `` `%dopar%` `` at the call site, since foreach's infix syntax
#     requires that exact name in scope
setupParallelCluster <- function(useParallel, requestedWorkers) {
  if (useParallel) {
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    num_workers <- if (nzchar(chk) && chk == "TRUE") {
      2L # use 2 cores in CRAN/Travis/AppVeyor
    } else {
      requestedWorkers
    }
    cluster <- parallel::makeCluster(num_workers, type = "PSOCK", outfile = "")
    doSNOW::registerDoSNOW(cluster)
    dopar <- foreach::`%dopar%`
  } else {
    cluster <- NULL
    num_workers <- 1L
    dopar <- foreach::`%do%`
  }

  list(cluster = cluster, num_workers = num_workers, dopar = dopar)
}

# tears down a cluster created by setupParallelCluster(), if any (cluster is
# NULL when the algorithm ran serially, in which case there's nothing to do)
teardownParallelCluster <- function(cluster) {
  if (!is.null(cluster)) {
    foreach::registerDoSEQ()
    parallel::stopCluster(cluster)
  }
}

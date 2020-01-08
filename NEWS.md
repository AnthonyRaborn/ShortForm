# Package v0.5.0
## Introduction of S4 Function Classes
* This version (re-)introduces classes to each of the main function outputs using S4 methods (instead of the S3 methods present in the prior version)
* Each method has a `show` (print), `summary`, and `plot` method that provides revamped, concise information about the algorithm
* Functions have been updated to accomodate these changes

## Parallelized Short Form Functions
* Some short form functions have been modified to work with parallel processors, resulting in noticeably faster results particularly when the solution space is larger (e.g., more starting items) or with more complex models.
  * Currently, this only works for `antcolony.lavaan`.

# Package v0.4.5
## Extra arugment checks
* There are now additional checks for the function arguments related to the fit statistics (ACO, SA) and the fit statistic tests (ACO) with informative warnings for when the function arguments are not valid.

# Package v0.4.2
## Introduction of Function Classes
* This version introduces classes to each of the main function outputs (`antcolony.lavaan` == "antcolony", `simulatedAnnealing` == "simulatedAnnealing", `tabuShortForm` == "tabu")
* This will allow for `plot` and `print` methods for each of these functions

## Broken functionality: plot.antcolony
* The function/method `plot.antcolony` was completely removed

## Changed functionality: Tabu Short Form
* The `tabuShortForm()` function has been revamped using `tabu.sem()` as the base and utilizing some internal functions from the other algorithms.

## Internal function changes
* Many functions have been refactored to be more concise and easier to maintain. This is an ongoing effort and will continue for the foreseeable future, with a goal towards allowing for parallel functionality.

## Bugfixes
* Minor bugs have been fixed, mostly in the intermediate v0.4.5.


# ShortForm

# *News*

# ShortForm v0.6.0

## Bugfixes

- Fixed tabuShortForm crashing with “could not find function ‘%dopar%’”
  when run with parallel = TRUE under CRAN-style core-limited
  environments (e.g., when `_R_CHECK_LIMIT_CORES_` is set)
- Fixed simulatedAnnealing hanging indefinitely when a candidate
  short-form change asked for more item swaps than a factor had
  available capacity
- Fixed tabu.sem crashing when a candidate neighbor failed to refit
  entirely, rather than just failing to converge
- Fixed tabu.sem crashing when every candidate neighbor was invalid or
  already on the tabu list
- Fixed antcolony.lavaan crashing when a partial lavaan.model.specs
  override was supplied, due to an internal default for `ordered` that
  didn’t match the function’s own documented default
- Fixed antcolony.lavaan silently dropping the
  `group`/`group.label`/`group.equal`/`group.partial`/`group.w.free`
  elements of lavaan.model.specs, due to a duplicated `auto.var` entry
  in its internal defaults and those elements never being wired into the
  underlying `lavaan()` call
- Fixed simulatedAnnealing producing a spurious “multiple local function
  definitions for ‘progressCallback’” NOTE under R CMD check, and
  restored its per-step “Current Step” progress output (previously
  assigned to an unreachable code path) for both serial and parallel
  runs

## Updates

- Reworked negateCriterion (tabu.sem, tabuShortForm) so it genuinely
  controls whether the search looks for the largest or smallest value of
  the objective/criterion function, rather than requiring users to
  pre-negate their own criterion function beforehand
- Renamed tabu.sem’s `obj` argument to `criterion`, matching
  tabuShortForm
- Changed tabuShortForm’s default criterion from a pre-negated `-cfi` to
  plain `cfi`
- Extended partial lavaan.model.specs overrides (previously only
  supported by antcolony.lavaan) to simulatedAnnealing and
  tabuShortForm, with new typo detection for unrecognized element names
  across all three algorithms
- Added a burn_in argument and whole-number axis ticks to
  simulatedAnnealing’s plot method
- Consolidated TS’s show/summary model-syntax reconstruction into one
  shared internal helper
- Function calls for simulatedAnnealing, tabuShortForm, tabu.sem, and
  antcolony.lavaan are now captured with every argument resolved
  (specified or defaulted), including the actual merged
  lavaan.model.specs used
- Corrected @return documentation for tabu.sem, tabuShortForm,
  simulatedAnnealing, and antcolony.lavaan to describe their actual S4
  return types instead of stale list descriptions
- Consolidated the parallel cluster bootstrap/teardown boilerplate
  shared by antcolony.lavaan, simulatedAnnealing, and tabuShortForm into
  shared internal helpers
- Replaced internal `mapply(assign, ...)` variable-splatting with
  `do.call()` (where the values were only ever fed into a single
  downstream `lavaan()` call) or plain named-list access (where a couple
  of values were unpacked for general use), removing several ad hoc
  internal environments in the process
- Added the selected criterion/fit-statistic and its final-model value
  to the show/summary output for SA, TS, and ACO objects

## To Do

Note–these are not required for 0.6.0, but are on the roadmap for future
updates.

- standardize function inputs, arguments, and outputs (0.7.0)

# ShortForm v0.5.9

Note: v0.5.9 was an internal version only and was not released to CRAN;
the changes below were included in the v.0.6.0 release.

## Bugfixes

- Fixed simulatedAnnealing crashing when run without maxItems (i.e.,
  full-model rather than short-form usage)
- Fixed tabu.sem and tabuShortForm erroring instead of returning a
  result when no candidate model improved on the initial model
- Fixed simulatedAnnealing’s restart-after-stagnation logic throwing an
  error instead of restarting
- Fixed antcolony.lavaan’s stopping rule comparing the wrong ant
  solutions, and a warning/error check that could crash a run
- Fixed a bug where swapping an item for a short form could silently
  corrupt an unrelated item whose name happened to share a suffix
  (affected simulatedAnnealing and tabuShortForm)

## Updates

- Consolidated the lavaan model syntax-building logic shared by the ACO,
  SA, and Tabu algorithms into common internal helper functions
- Added substantial unit test coverage for the top-level
  antcolony.lavaan, simulatedAnnealing, tabuShortForm, and tabu.sem
  functions
- Deprecated the shortForm argument in simulatedAnnealing; this is now
  determined automatically from maxItems

# ShortForm v0.5.8

## Bugfixes

- Fixed issue in tabuShortForm for certain parallel workflows
- Fixed bug in tabuShortForm when using multidimensional models

## Updates

- Removed dependencies on ggplot2, ggrepel, and tidyr (ACO and TS plot
  methods)
- Added some additional unit tests

=======

# ShortForm v0.5.7

## Bugfixes

- Fixed issue in ACO algorithm where the best model was not properly
  updating
- Corrected use of ggplot within the ACO plot method due to depreciated
  ggplot2 functions
- Fixed issue in SA algorithm where models including factor
  relationships or outcome variables were not specified correctly after
  the initial model

## Updates

- Modernized the README file
- Fixed CI/CD badge since Travis CI no longer works for this project

# ShortForm v0.5.6

- Updates focused on `{antcolony.lavaan}`.
  - lavaan.model.specs now defaults to the default arugments. If a user
    removes the arguments, they will be provided for them. If a user
    overwrites a specific argument (e.g., `estimator = 'wls'`), that
    will be respected.
  - Some minor adjustments to checking fitted models for specific
    warnings/errors that should result in better functioning when these
    do not exist.

# ShortForm v0.5.5

- Further updates to handle changes to `lavaan` error/warning messages
  more gracefully. These should continue into the future for the SA and
  ACo methods without additional issues.

# ShortForm v0.5.4

- Updates to handle changes to `lavaan` error/warning messages
- Fixed internal documentation to match current standards

# ShortForm v0.5.3

- Updates to antcolony.lavaan default `lavaan.model.specs`
- Spelling fixes
- Roxygen manual fixes

# ShortForm v0.5.2

## Bugfixes

- This version fixes minor bugs, most associated with CRAN messages and
  warnings

# ShortForm v0.5.0

## Introduction of S4 Function Classes

- This version (re-)introduces classes to each of the main function
  outputs using S4 methods (instead of the S3 methods present in the
  prior version)
- Each method has a `show` (print), `summary`, and `plot` method that
  provides revamped, concise information about the algorithm
- Functions have been updated to accommodate these changes

## Parallelized Short Form Functions

- Some short form functions have been modified to work with parallel
  processors, resulting in noticeably faster results particularly when
  the solution space is larger (e.g., more starting items) or with more
  complex models
  - This has resulted in some function argument changes. Please check
    any code that was written with older versions!

## Minor cleanup

- Removed some unneeded code

# ShortForm 0.4.6

## Updated argument check

- The argument check for fit statistics (ACO, SA) were updated to
  include fit statistics related to the “WLSMV” estimator (i.e, for
  ordered data).

## Bugfixes

- A bug in the SA function that sometimes resulted in improper items has
  been fixed.

- A bug in the Tabu short form function that caused the Tabu list to be
  erased if the best criterion value was reached has been fixed.

# ShortForm 0.4.5

## Extra arugment checks

- There are now additional checks for the function arguments related to
  the fit statistics (ACO, SA) and the fit statistic tests (ACO) with
  informative warnings for when the function arguments are not valid.

# ShortForm 0.4.2

## Introduction of Function Classes

- This version introduces classes to each of the main function outputs
  (`antcolony.lavaan` == “antcolony”, `simulatedAnnealing` ==
  “simulatedAnnealing”, `tabuShortForm` == “tabu”)

- This will allow for `plot` and `print` methods for each of these
  functions

## Broken functionality: antcolony_plot

- The function `antcolony_plot` was replaced with `plot.antcolony` (aka,
  a plot method)

## New functionality: S3 method for plotting

- With the addition of classes, a plot method and accompanying
  documentation has been added.

- Future updates will expand S3 methods for `print()` and `summary()`

## Bugfixes

- Bugfixes to the main functions when using them to create shortforms of
  bifactor models.
- They should now produce actual bifactor shortforms (in 0.4.1,
  sometimes the item names would be cut off in later iterations
  $$FIXED$$ and the relationship between latent variables would be
  changed when they should have been kept constant $$FIXED$$).

## Added a package loading message

- Now, when using an interactive R session, a package message is printed
  out (with a little Penguin holding it all up)!

# ShortForm 0.4.1

## New functionality: Simulated Annealing

- The Simulated Annealing (SA) algorithm has been added to the package,
  with a single user-facing function `simulatedAnnealing()`.
- SA can be used on anything lavaan can run, though the current
  implementation focuses on traditional confirmatory models using
  `lavaan::cfa()`. However, there are plans for more user control in the
  modeling process to allow for other model defaults.

## New functionality: Tabu Search (Short Form)

- An adaptation of the Tabu search to short form creation has been
  added.

## New functionality: ACO Plots

- The `antcolony_lavaan()` function has been modified so that it no
  longer will print a `summaryfile.txt`. Rather, the function maintains
  the old summary file as an internal object and returns it after
  completion,
- The new function `antcolony_plot()` takes the results from
  `antcolony_lavaan()` and creates three graphs: (a) a plot showing how
  pheremone levels change as the algorithm progresses, (b) a plot
  showing how the mean value of the regression coefficients changes as
  the algorithm progresses, and (c) a plot showing how the mean variance
  explained changes as the algorithm progresses.

## New functionality: Bifactor Models

- Each of the short form functions utilizing lavaan
  (`antcolony.lavaan()`, `simulatedAnnealing()`, `tabuShortForm()`) are
  capable of handling bifactor models.
- In theory, this means relatively arbitrary models can be specified as
  well, but the functions utilize the “~” and “\~~” operators for the
  additional functionality as well as the `bifactor` logical option. No
  guarantee that an arbitrary model will work beyond those currently
  tested.

### Minor revisions

- The lavaan-based functions all have more controlled output to the R
  console. This is most noticeable with the `antcolony.lavaan()`
  function.

# ShortForm 0.4.0

## New functionality: Tabu Search

- The Tabu search has been added to the package, with user-facing
  functions `tabu.sem()` and `search.prep()` for most of the heavy
  lifting and various helper and internal functions. The code was taken
  primarily from Carl Falk and Katerina Marcoulides (see Marcoulides, K.
  M., & Falk, C. F. (2018). Model Specification Searches in Structural
  Equation Modeling with R. Structural Equation Modeling: A
  Multidisciplinary Journal, 1-8.). These have been tested in limited
  circumstances, so please report any bugs as you find them!

## Minor changes

- NEWS.md created.
- Description file updated.
- Authorship has been updated and URLs have been added to the ant colony
  functions.
- No bugs have been found in the package thus far, but just a reminder
  for any users that bug reports are helpful and welcome.

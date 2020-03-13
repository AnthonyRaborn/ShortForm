
# ShortForm

# *News*

# ShortForm 0.4.6

## Updated argument check

  - The argument check for fit statistics (ACO, SA) were updated to
    include fit statistics related to the “WLSMV” estimator (i.e, for
    ordered data).

## Bugfixes

  - A bug in the SA function that sometimes resulted in improper items
    has been fixed.

  - A bug in the Tabu short form function that caused the Tabu list to
    be erased if the best criterion value was reached has been fixed.

# ShortForm 0.4.5

## Extra arugment checks

  - There are now additional checks for the function arguments related
    to the fit statistics (ACO, SA) and the fit statistic tests (ACO)
    with informative warnings for when the function arguments are not
    valid.

# ShortForm 0.4.2

## Introduction of Function Classes

  - This version introduces classes to each of the main function outputs
    (`antcolony.lavaan` == “antcolony”, `simulatedAnnealing` ==
    “simulatedAnnealing”, `tabuShortForm` == “tabu”)

  - This will allow for `plot` and `print` methods for each of these
    functions

## Broken functionality: antcolony\_plot

  - The function `antcolony_plot` was replaced with `plot.antcolony`
    (aka, a plot method)

## New functionality: S3 method for plotting

  - With the addition of classes, a plot method and accompanying
    documentation has been added.

  - Future updates will expand S3 methods for `print()` and `summary()`

## Bugfixes

  - Bugfixes to the main functions when using them to create shortforms
    of bifactor models.
  - They should now produce actual bifactor shortforms (in 0.4.1,
    sometimes the item names would be cut off in later iterations
    \[FIXED\] and the relationship between latent variables would be
    changed when they should have been kept constant \[FIXED\]).

## Added a package loading message

  - Now, when using an interactive R session, a package message is
    printed out (with a little Penguin holding it all up)\!

# ShortForm 0.4.1

## New functionality: Simulated Annealing

  - The Simulated Annealing (SA) algorithm has been added to the
    package, with a single user-facing function `simulatedAnnealing()`.
  - SA can be used on anything lavaan can run, though the current
    implementation focuses on traditional confirmatory models using
    `lavaan::cfa()`. However, there are plans for more user control in
    the modelling process to allow for other model defaults.

## New functionality: Tabu Search (Short Form)

  - An adaptation of the Tabu search to short form creation has been
    added.

## New functionality: ACO Plots

  - The `antcolony_lavaan()` function has been modified so that it no
    longer will print a `summaryfile.txt`. Rather, the function
    maintains the old summary file as an internal object and returns it
    after completion,
  - The new function `antcolony_plot()` takes the results from
    `antcolony_lavaan()` and creates three graphs: (a) a plot showing
    how pheremone levels change as the algorithm progresses, (b) a plot
    showing how the mean value of the regression coefficients changes as
    the algorithm progresses, and (c) a plot showing how the mean
    variance explained changes as the algorithm progresses.

## New functionality: Bifactor Models

  - Each of the short form functions utilizing lavaan
    (`antcolony.lavaan()`, `simulatedAnnealing()`, `tabuShortForm()`)
    are capable of handling bifactor models.
  - In theory, this means relatively arbitrary models can be specified
    as well, but the functions utilize the “\~” and “\~\~” operators for
    the additional functionality as well as the `bifactor` logical
    option. No guarantee that an arbitrary model will work beyond those
    currently tested.

### Minor revisions

  - The lavaan-based functions all have more controlled output to the R
    console. This is most noticeable with the `antcolony.lavaan()`
    function.

# ShortForm 0.4.0

## New functionality: Tabu Search

  - The Tabu search has been added to the package, with user-facing
    functions `tabu.sem()` and `search.prep()` for most of the heavy
    lifting and various helper and internal functions. The code was
    taken primarily from Carl Falk and Katerina Marcoulides (see
    Marcoulides, K. M., & Falk, C. F. (2018). Model Specification
    Searches in Structural Equation Modeling with R. Structural Equation
    Modeling: A Multidisciplinary Journal, 1-8.). These have been tested
    in limited circumstances, so please report any bugs as you find
    them\!

## Minor changes

  - NEWS.md created.
  - Description file updated.
  - Authorship has been updated and URLs have been added to the ant
    colony functions.
  - No bugs have been found in the package thus far, but just a reminder
    for any users that bug reports are helpful and welcome.

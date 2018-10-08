# Package v0.4.2
## Introduction of Function Classes
* This version introduces classes to each of the main function outputs (`antcolony.lavaan` == "antcolony", `simulatedAnnealing` == "simulatedAnnealing", `tabuShortForm` == "tabu")
* This will allow for `plot` and `print` methods for each of these functions

## Broken functionality: antcolony_plot
* The function `antcolony_plot` was replaced with `plot.antcolony` (aka, a plot method)

## Bugfixes
* Bugfixes to the main functions when using them to create shortforms of bifactor models. 
* They should now produce actual bifactor shortforms (in v0.4.1, sometimes the item names would be cut off in later iterations [FIXED] and the relationship between latent variables would be changed when they should have been kept constant [FIXED]).

# Package v0.4.1
## New functionality: Simulated Annealing

* The Simulated Annealing (SA) algorithm has been added to the package, with a single user-facing function `simulatedAnnealing()`. 
* SA can be used on anything lavaan can run, though the current implementation focuses on traditional confirmatory models using `lavaan::cfa()`. However, there are plans for more user control in the modelling process to allow for other model defaults.

## New functionality: Tabu Search (Short Form)
* An adaptation of the Tabu search to short form creation has been added.

## New functionality: ACO Plots

* The `antcolony_lavaan()` function has been modified so that it no longer will print a `summaryfile.txt`. Rather, the function maintains the old summary file as an internal object and returns it after completion,
* The new function `antcolony_plot()` takes the results from `antcolony_lavaan()` and creates three graphs: (a) a plot showing how pheremone levels change as the algorithm progresses, (b) a plot showing how the mean value of the regression coefficients changes as the algorithm progresses, and (c) a plot showing how the mean variance explained changes as the algorithm progresses.

## New functionality: Bifactor Models

* Each of the short form functions utilizing lavaan (`antcolony.lavaan()`, `simulatedAnnealing()`, `tabuShortForm()`) are capable of handling bifactor models.
* In theory, this means relatively arbitrary models can be specified as well, but the functions utilize the "~" and "~~" operators for the additional functionality as well as the `bifactor` logical option. No guarantee that an arbitrary model will work beyond those currently tested.

### Minor revisions

* The lavaan-based functions all have more controlled output to the R console. This is most noticeable with the `antcolony.lavaan()` function.

# Package v0.4.0
## New functionality: Tabu Search

* The Tabu search has been added to the package, with user-facing functions `tabu.sem()` and `search.prep()` for most of the heavy lifting and various helper and internal functions. The code was taken primarily from Carl Falk and Katerina Marcoulides (see Marcoulides, K. M., & Falk, C. F. (2018). Model Specification Searches in Structural Equation Modeling with R. Structural Equation Modeling: A Multidisciplinary Journal, 1-8.). These have been tested in limited circumstances, so please report any bugs as you find them!

## Minor changes

* NEWS.md created.
* Description file updated.
* Authorship has been updated and URLs have been added to the ant colony functions.
* No bugs have been found in the package thus far, but just a reminder for any users that bug reports are helpful and welcome.

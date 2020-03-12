
# ShortForm

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ShortForm)](http://cran.r-project.org/package=ShortForm)
[![Travis-CI Build
Status](http://travis-ci.org/AnthonyRaborn/ShortForm.svg?branch=master)](http://travis-ci.org/AnthonyRaborn/ShortForm)
[![CRAN Downloads Per
Month](https://cranlogs.r-pkg.org/badges/ShortForm)](https://cran.r-project.org/package=ShortForm)
[![CRAN Downloads
Total](https://cranlogs.r-pkg.org/badges/grand-total/ShortForm?color=orange)](https://cran.r-project.org/package=ShortForm)

Automatic Short Form Creation for scales. Currently, the Ant Colony
Optimization (ACO) Algorithm and the Tabu search are implemented. The
original R implementation for the ACO algorithm is from [Leite, Huang, &
Marcoulides (2008)](doi:10.1080/00273170802285743), while the Tabu
search function was taken from [Marcoulides & Falk
(2018)](doi:10.1080/10705511.2017.1409074). There does not yet seem to
be an application of Simulated Annealing (SA) within psychometrics, but
Drezner & Marcoulides, 1999 (in *Multiple Linear Regression Viewpoints*,
Volume 25(2); not available online) used SA for multiple regression
model selection; this package appears to be the first to implement SA
for psychometric models.

## Installation

``` r
install.packages("ShortForm") # the CRAN-approved version
# install.packages("devtools")
devtools::install_github("AnthonyRaborn/ShortForm", branch = "devel") # the developmental version
```

## Usage

Here are some (slightly modified) examples from the help documentation
using lavaan. Be warned, the algorithms may take some time to converge,
particularly with large forms, multiple dimensions, and different
settings. The time for these examples to converge on a low-end laptop is
printed at the bottom.

### ACO Algorithm

``` r
start.time.ACO <- Sys.time()
library(ShortForm, quietly = T)
##    #####                             #######                      
##   #     # #    #  ####  #####  ##### #        ####  #####  #    # 
##   #       #    # #    # #    #   #   #       #    # #    # ##  ## 
##    #####  ###### #    # #    #   #   #####   #    # #    # # ## # 
##         # #    # #    # #####    #   #       #    # #####  #    # 
##   #     # #    # #    # #   #    #   #       #    # #   #  #    # 
##    #####  #    #  ####  #    #   #   #        ####  #    # #    # 
##   
##           Version 0.4.6
##               (o<
##               //\
##               V_/_
##  Package 'ShortForm' version 0.4.6
# using simulated test data and the default values for lavaan.model.specs
set.seed(1)
# create simulation data from the `psych` package
# four factors, 12 items each, 48 total items
# factor loading matrix - not quite simple structure
fxMatrix <- 
 matrix(data = c(rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4*3), # first factor loadings
                 
                 rep(0.2, times = 3*4),
                 rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4*2), # second factor loadings
                 
                 rep(0.2, times = 3*4*2),
                 rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4), # third factor loadings
                 
                 rep(0.2, times = 3*4*3),
                 rep(x = c(.8, .8, .4, .3), times = 3) # fourth factor loadings
 ),
 ncol = 4)
# factor correlation matrix - all factors uncorrelated
PhiMatrix <-
 matrix(data = c(1,0,0,0, 
                 0,1,0,0, 
                 0,0,1,0, 
                 0,0,0,1), ncol = 4) 
antData <- 
 psych::sim(
   fx = fxMatrix,
   Phi = PhiMatrix,
   n = 600,
   mu = c(-2, -1, 1, 2),
   raw = TRUE
 )$observed # observed is the simulated observed data
colnames(antData) = paste0("Item", 1:48)
antModel <- '
Trait1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12
Trait2 =~ Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20 + Item21 + Item22 + Item23 + Item24
Trait3 =~ Item25 + Item26 + Item27 + Item28 + Item29 + Item30 + Item31 + Item32 + Item33 + Item34 + Item35 + Item36
Trait4 =~ Item37 + Item38 + Item39 + Item40 + Item41 + Item42 + Item43 + Item44 + Item45 + Item46 + Item47 + Item48
'
# then, create the list of the items by the factors
list.items <- 
  list(
    paste0("Item",  1:12),
    paste0("Item", 13:24),
    paste0("Item", 25:36),
    paste0("Item", 37:48)
       )
# finally, call the function with some minor changes to the default values.
abilityShortForm = 
  antcolony.lavaan(data = antData,
                   ants = 10, evaporation = 0.9, antModel = antModel,
                   list.items = list.items, full = 48, i.per.f = c(6,6,6,6),
                   lavaan.model.specs = 
                     list(model.type = "cfa", auto.var = T, estimator = "default", 
                          ordered = NULL, int.ov.free = TRUE,
                          int.lv.free = FALSE, auto.fix.first = TRUE, 
                          auto.fix.single = TRUE, std.lv = FALSE, 
                          auto.cov.lv.x = TRUE, auto.th = TRUE, 
                          auto.delta = TRUE, auto.cov.y = TRUE),
                   factors = c("Trait1", "Trait2", "Trait3", "Trait4"), 
                   steps = 20, 
                   fit.indices = c('cfi', 'rmsea'),
                   fit.statistics.test = "(cfi > 0.90)&(rmsea < 0.10)",
                   summaryfile = NULL,
                   feedbackfile = NULL,
                   max.run = 1000, 
                   verbose = FALSE)
##   Run number 1.            Run number 2.            Run number 3.            Run number 4.            Run number 5.            Run number 6.            Run number 7.            Run number 8.            Run number 9.            Run number 10.            Run number 11.            Run number 12.            Run number 13.            Run number 14.            Run number 15.            Run number 16.            Run number 17.            Run number 18.            Run number 19.            Run number 20.            Run number 21.            Run number 22.            Run number 23.            Run number 24.            Run number 25.            Run number 26.            Run number 27.            Run number 28.            Run number 29.            Run number 30.            Run number 31.            Run number 32.            Run number 33.            Run number 34.            Run number 35.            Run number 36.            Run number 37.            Run number 38.            Run number 39.            Run number 40.            Run number 41.            Run number 42.            Run number 43.            Run number 44.            Run number 45.            Run number 46.            Run number 47.            Run number 48.            Run number 49.            Run number 50.            Run number 51.            Run number 52.            Run number 53.            Run number 54.            Run number 55.            Run number 56.            Run number 57.            Run number 58.            Run number 59.            Run number 60.            Run number 61.            Run number 62.            Run number 63.            Run number 64.            Run number 65.            Run number 66.            Run number 67.            Run number 68.            Run number 69.            Run number 70.            Run number 71.            Run number 72.            Run number 73.            Run number 74.            Run number 75.            Run number 76.            Run number 77.            Run number 78.            Run number 79.            Run number 80.            Run number 81.            Run number 82.            Run number 83.            Run number 84.            Run number 85.            Run number 86.            Run number 87.            Run number 88.            Run number 89.            Run number 90.            Run number 91.            Run number 92.            Run number 93.            Run number 94.            Run number 95.            Run number 96.            Run number 97.            Run number 98.            Run number 99.           [1] "Compiling results."
abilityShortForm[[1]] # print the results of the final short form
##             cfi      rmsea mean_gamma Item1 Item2 Item3 Item4 Item5 Item6 Item7
##  [1,] 0.9980948 0.01356062      0.875     1     1     0     0     1     1     0
##       Item8 Item9 Item10 Item11 Item12 Item13 Item14 Item15 Item16 Item17 Item18
##  [1,]     0     1      1      0      0      1      1      0      0      1      1
##       Item19 Item20 Item21 Item22 Item23 Item24 Item25 Item26 Item27 Item28
##  [1,]      0      0      1      1      0      0      1      1      0      0
##       Item29 Item30 Item31 Item32 Item33 Item34 Item35 Item36 Item37 Item38
##  [1,]      1      1      0      0      1      1      0      0      1      1
##       Item39 Item40 Item41 Item42 Item43 Item44 Item45 Item46 Item47 Item48
##  [1,]      0      0      1      1      0      0      1      1      0      0
plot(abilityShortForm) # the plots for class "antcolony"
##  $Pheromone
```

![](README-ACO%20example-1.png)<!-- -->

    ##  
    ##  $Gamma
    ##  `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](README-ACO%20example-2.png)<!-- -->

    ##  
    ##  $Beta
    ##  `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](README-ACO%20example-3.png)<!-- -->

    ##  
    ##  $Variance
    ##  `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](README-ACO%20example-4.png)<!-- -->

A similar example can be found in the `antcolony.mplus` function, but
requires you to have a valid Mplus installation on the computer. It took
a total of 1.86 minutes to run this example.

### Tabu Search Algorithm

This example demonstrates how to use the Tabu search for model
specification searches when the original model may be misspecified in
some way.

``` r
start.time.Tabu <- Sys.time()
library(ShortForm, quietly = T)
set.seed(2)
# create simulation data from the `psych` package
# two factors, 12 items total
# factor loading matrix - not quite simple structure
fxMatrix <- 
  matrix(data = c(
    # first factor loadings
    rep(x = c(.8, .8, .6, .6), times = 3),
    # second factor loadings
    rep(x = c(.2), times = 12)
    ),
  ncol = 2)
# factor correlation matrix - all factors uncorrelated
PhiMatrix <-
  matrix(data = c(1,0, 
                  0,1
                  ), ncol = 2) 
tabuData <- 
  psych::sim(
    fx = fxMatrix,
    Phi = PhiMatrix,
    n = 600,
    raw = TRUE
  )$observed # observed is the simulated observed data
colnames(tabuData) = paste0("Item", 1:12)
tabuModel <- '
Trait1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + 0*Item7 + 0*Item8 + 0*Item9 + 0*Item10 + 0*Item11 + 0*Item12
Trait2 =~ 0*Item1 + 0*Item2 + 0*Item3 + 0*Item4 + 0*Item5 + 0*Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12
'
# fit the initial misspecified model for Tabu
init.model <- lavaan::lavaan(model = tabuModel, data = tabuData, 
                             auto.var=TRUE, auto.fix.first=FALSE, std.lv=TRUE,
                             auto.cov.lv.x=FALSE)
# use search.prep to prepare for the Tabu search
ptab <- 
  search.prep(fitted.model = init.model, 
              loadings=TRUE, 
              fcov=FALSE, 
              errors=FALSE)
Tabu_example <- 
  suppressWarnings(
    tabu.sem(init.model = init.model, 
             ptab = ptab, 
             obj = AIC, 
             niter = 20, 
             tabu.size = 10)
    ) # the suppressWarning wrapping hides the lavaan WARNING output from improper models
##  Running iteration 1 of 20.   Running iteration 2 of 20.   Running iteration 3 of 20.   Running iteration 4 of 20.   Running iteration 5 of 20.   Running iteration 6 of 20.   Running iteration 7 of 20.   Running iteration 8 of 20.   Running iteration 9 of 20.   Running iteration 10 of 20.   Running iteration 11 of 20.   Running iteration 12 of 20.   Running iteration 13 of 20.   Running iteration 14 of 20.   Running iteration 15 of 20.   Running iteration 16 of 20.   Running iteration 17 of 20.   Running iteration 18 of 20.   Running iteration 19 of 20.   Running iteration 20 of 20.

# check the final model
lavaan::summary(Tabu_example$best.mod)
##  lavaan 0.6-5 ended normally after 43 iterations
##  
##    Estimator                                         ML
##    Optimization method                           NLMINB
##    Number of free parameters                         29
##                                                        
##    Number of observations                           600
##                                                        
##  Model Test User Model:
##                                                        
##    Test statistic                                49.967
##    Degrees of freedom                                49
##    P-value (Chi-square)                           0.435
##  
##  Parameter Estimates:
##  
##    Information                                 Expected
##    Information saturated (h1) model          Structured
##    Standard errors                             Standard
##  
##  Latent Variables:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Trait1 =~                                           
##      Item1             0.868    0.035   24.971    0.000
##      Item2             0.875    0.034   25.455    0.000
##      Item3             0.630    0.036   17.264    0.000
##      Item4             0.580    0.037   15.595    0.000
##      Item5             0.889    0.034   25.941    0.000
##      Item6             0.884    0.035   25.198    0.000
##      Item7             0.654    0.038   17.254    0.000
##      Item8             0.688    0.037   18.400    0.000
##      Item9             0.828    0.034   24.247    0.000
##      Item10            0.835    0.035   24.138    0.000
##      Item11            0.581    0.037   15.507    0.000
##      Item12            0.632    0.039   16.231    0.000
##    Trait2 =~                                           
##      Item1             0.000                           
##      Item2             0.000                           
##      Item3             0.000                           
##      Item4             0.000                           
##      Item5             0.051    0.063    0.815    0.415
##      Item6             0.070    0.082    0.850    0.396
##      Item7             0.053    0.069    0.775    0.439
##      Item8             0.000                           
##      Item9             0.083    0.096    0.867    0.386
##      Item10            0.000                           
##      Item11            0.754    0.800    0.943    0.346
##      Item12            0.000                           
##  
##  Covariances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Trait1 ~~                                           
##      Trait2            0.000                           
##   .Item1 ~~                                            
##     .Item2             0.000                           
##     .Item3             0.000                           
##     .Item4             0.000                           
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item2 ~~                                            
##     .Item3             0.000                           
##     .Item4             0.000                           
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item3 ~~                                            
##     .Item4             0.000                           
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item4 ~~                                            
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item5 ~~                                            
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item6 ~~                                            
##     .Item7             0.000                           
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item7 ~~                                            
##     .Item8             0.000                           
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item8 ~~                                            
##     .Item9             0.000                           
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item9 ~~                                            
##     .Item10            0.000                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item10 ~~                                           
##     .Item11            0.000                           
##     .Item12            0.000                           
##   .Item11 ~~                                           
##     .Item12            0.000                           
##  
##  Variances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##      Trait1            1.000                           
##      Trait2            1.000                           
##     .Item1             0.324    0.022   14.895    0.000
##     .Item2             0.303    0.021   14.671    0.000
##     .Item3             0.565    0.034   16.614    0.000
##     .Item4             0.624    0.037   16.779    0.000
##     .Item5             0.280    0.019   14.624    0.000
##     .Item6             0.311    0.021   14.709    0.000
##     .Item7             0.601    0.036   16.646    0.000
##     .Item8             0.567    0.034   16.477    0.000
##     .Item9             0.315    0.022   14.282    0.000
##     .Item10            0.344    0.023   15.225    0.000
##     .Item11            0.039    1.207    0.032    0.975
##     .Item12            0.669    0.040   16.720    0.000
```

It took a total of 2.1 minutes to run this example.

The next Tabu example demonstrates how to use it to find a short form of
a prespecified length with the same data.

``` r
start.time.Tabu <- Sys.time()
library(ShortForm, quietly = T)
# set the seed to reproduce this example
set.seed(3)
# create simulation data from the `psych` package
# four factors, 12 items each, 48 total items
# factor loading matrix - not quite simple structure
fxMatrix <- 
 matrix(data = c(rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4*3), # first factor loadings
                 
                 rep(0.2, times = 3*4),
                 rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4*2), # second factor loadings
                 
                 rep(0.2, times = 3*4*2),
                 rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4), # third factor loadings
                 
                 rep(0.2, times = 3*4*3),
                 rep(x = c(.8, .8, .4, .3), times = 3) # fourth factor loadings
 ),
 ncol = 4)
# factor correlation matrix - all factors uncorrelated
PhiMatrix <-
 matrix(data = c(1,0,0,0, 
                 0,1,0,0, 
                 0,0,1,0, 
                 0,0,0,1), ncol = 4) 
tabuData <- 
 psych::sim(
   fx = fxMatrix,
   Phi = PhiMatrix,
   n = 600,
   mu = c(-2, -1, 1, 2),
   raw = TRUE
 )$observed # observed is the simulated observed data
colnames(tabuData) = paste0("Item", 1:48)
tabuModel <- '
Trait1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12
Trait2 =~ Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20 + Item21 + Item22 + Item23 + Item24
Trait3 =~ Item25 + Item26 + Item27 + Item28 + Item29 + Item30 + Item31 + Item32 + Item33 + Item34 + Item35 + Item36
Trait4 =~ Item37 + Item38 + Item39 + Item40 + Item41 + Item42 + Item43 + Item44 + Item45 + Item46 + Item47 + Item48
'
# specify the criterion function that the Tabu Search minimizes
# wrap this in a tryCatch in case a model does not converge!
# specify an appropriate error value: since we're minimizing, error value must be large
tabuCriterion = function(x) {
 tryCatch(lavaan::fitmeasures(object = x, fit.measures = 'chisq'),
          error = function(e) Inf)
}
# use the tabuShortForm function
# reduce form to the best 12 items, 3 per factor
tabuShort <- 
  tabuShortForm(initialModel = tabuModel, originalData = tabuData,
                numItems = c(3,3,3,3), criterion = tabuCriterion,
                niter = 20, tabu.size = 10,
                allItems =   list(paste0("Item",  1:12),
                                  paste0("Item", 13:24),
                                  paste0("Item", 25:36),
                                  paste0("Item", 37:48))
                )
##  Creating initial short form.
##  The initial short form is: 
##  Trait1 =~ Item3 + Item4 + Item12
##  Trait2 =~ Item24 + Item16 + Item15
##  Trait3 =~ Item31 + Item29 + Item30
##  Trait4 =~ Item41 + Item45 + Item40
##   Current Progress:  |                                                                              |===                                                                   |   5%  |                                                                              |=======                                                               |  10%  |                                                                              |==========                                                            |  14%  |                                                                              |=============                                                         |  19%  |                                                                              |=================                                                     |  24%  |                                                                              |====================                                                  |  29%  |                                                                              |=======================                                               |  33%  |                                                                              |===========================                                           |  38%  |                                                                              |==============================                                        |  43%  |                                                                              |=================================                                     |  48%  |                                                                              |=====================================                                 |  52%  |                                                                              |========================================                              |  57%  |                                                                              |===========================================                           |  62%  |                                                                              |===============================================                       |  67%  |                                                                              |==================================================                    |  71%  |                                                                              |=====================================================                 |  76%  |                                                                              |=========================================================             |  81%  |                                                                              |============================================================          |  86%  |                                                                              |===============================================================       |  90%  |                                                                              |===================================================================   |  95%  |                                                                              |======================================================================| 100%
# check the chosen model
lavaan::summary(tabuShort$best.mod)
##  lavaan 0.6-5 ended normally after 21 iterations
##  
##    Estimator                                         ML
##    Optimization method                           NLMINB
##    Number of free parameters                         30
##                                                        
##    Number of observations                           600
##                                                        
##  Model Test User Model:
##                                                        
##    Test statistic                                35.164
##    Degrees of freedom                                48
##    P-value (Chi-square)                           0.916
##  
##  Parameter Estimates:
##  
##    Information                                 Expected
##    Information saturated (h1) model          Structured
##    Standard errors                             Standard
##  
##  Latent Variables:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Trait1 =~                                           
##      Item3             0.554    0.050   11.161    0.000
##      Item4             0.470    0.050    9.450    0.000
##      Item12            0.421    0.049    8.570    0.000
##    Trait2 =~                                           
##      Item15            0.494    0.047   10.561    0.000
##      Item16            0.426    0.047    9.097    0.000
##      Item24            0.457    0.048    9.551    0.000
##    Trait3 =~                                           
##      Item29            0.876    0.036   24.062    0.000
##      Item30            0.827    0.035   23.310    0.000
##      Item31            0.584    0.042   13.992    0.000
##    Trait4 =~                                           
##      Item40            0.524    0.042   12.462    0.000
##      Item41            0.744    0.042   17.529    0.000
##      Item43            0.598    0.043   13.820    0.000
##  
##  Covariances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Trait1 ~~                                           
##      Trait2            0.878    0.080   10.949    0.000
##      Trait3            0.702    0.053   13.202    0.000
##      Trait4            0.879    0.058   15.256    0.000
##    Trait2 ~~                                           
##      Trait3            0.805    0.052   15.383    0.000
##      Trait4            0.868    0.060   14.502    0.000
##    Trait3 ~~                                           
##      Trait4            0.642    0.039   16.530    0.000
##  
##  Variances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##     .Item3             0.750    0.055   13.574    0.000
##     .Item4             0.885    0.058   15.345    0.000
##     .Item12            0.908    0.057   15.856    0.000
##     .Item15            0.738    0.051   14.592    0.000
##     .Item16            0.833    0.053   15.779    0.000
##     .Item24            0.845    0.055   15.489    0.000
##     .Item29            0.263    0.031    8.388    0.000
##     .Item30            0.280    0.029    9.503    0.000
##     .Item31            0.745    0.046   16.071    0.000
##     .Item40            0.670    0.044   15.129    0.000
##     .Item41            0.472    0.044   10.662    0.000
##     .Item43            0.668    0.046   14.430    0.000
##      Trait1            1.000                           
##      Trait2            1.000                           
##      Trait3            1.000                           
##      Trait4            1.000
# plot the changes in the objective function over each iteration
plot(tabuShort)
```

![](README-Tabu%20short%20form-1.png)<!-- -->

It took a total of 1.75 minutes to run this example.

### Simulated Annealing

This example demonstrates the use of simulated annealing for creating
short forms.

``` r
start.time.SA <- Sys.time()
library(ShortForm, quietly = T)
# create simulation data from the `psych` package
# four factors, 12 items each, 48 total items
# factor loading matrix - not quite simple structure
set.seed(4)
fxMatrix <- 
 matrix(data = c(rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4*3), # first factor loadings
                 
                 rep(0.2, times = 3*4),
                 rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4*2), # second factor loadings
                 
                 rep(0.2, times = 3*4*2),
                 rep(x = c(.8, .8, .4, .3), times = 3),
                 rep(0.2, times = 3*4), # third factor loadings
                 
                 rep(0.2, times = 3*4*3),
                 rep(x = c(.8, .8, .4, .3), times = 3) # fourth factor loadings
 ),
 ncol = 4)
# factor correlation matrix - all factors uncorrelated
PhiMatrix <-
 matrix(data = c(1,0,0,0, 
                 0,1,0,0, 
                 0,0,1,0, 
                 0,0,0,1), ncol = 4) 
annealData <- 
 psych::sim(
   fx = fxMatrix,
   Phi = PhiMatrix,
   n = 600,
   mu = c(-2, -1, 1, 2),
   raw = TRUE
 )$observed # observed is the simulated observed data
colnames(annealData) = paste0("Item", 1:48)
annealModel <- '
Trait1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12
Trait2 =~ Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20 + Item21 + Item22 + Item23 + Item24
Trait3 =~ Item25 + Item26 + Item27 + Item28 + Item29 + Item30 + Item31 + Item32 + Item33 + Item34 + Item35 + Item36
Trait4 =~ Item37 + Item38 + Item39 + Item40 + Item41 + Item42 + Item43 + Item44 + Item45 + Item46 + Item47 + Item48
'
lavaan.model.specs <-
  list(model.type = "cfa",
       auto.var = TRUE, estimator = "default", ordered = NULL,
       int.ov.free = TRUE, int.lv.free = FALSE, std.lv = TRUE, auto.fix.first = FALSE, 
       auto.fix.single = TRUE, auto.cov.lv.x = TRUE, auto.th = TRUE, 
       auto.delta = TRUE, auto.cov.y = TRUE)
# perform the SA algorithm
SA_example <- 
  simulatedAnnealing(initialModel = annealModel, originalData = annealData, 
                     maxSteps = 500, fitStatistic = 'cfi', maximize = TRUE, 
                     temperature = "logistic", 
                     items = list(paste0("Item", 1:12),
                                  paste0("Item", 13:24),
                                  paste0("Item", 25:36),
                                  paste0("Item", 37:48)),
                     lavaan.model.specs = lavaan.model.specs, 
                     maxChanges = 3, maxItems = c(6,6,6,6))
##  Initializing short form creation.
##  The initial short form is:
##   Trait1 =~ Item11 + Item10 + Item6 + Item4 + Item1 + Item5
##  Trait2 =~ Item17 + Item15 + Item13 + Item14 + Item18 + Item24
##  Trait3 =~ Item25 + Item32 + Item35 + Item31 + Item33 + Item27
##  Trait4 =~ Item46 + Item38 + Item39 + Item41 + Item48 + Item43
##  Using the short form randomNeighbor function.
##  Finished initializing short form options.
##   Current Progress:  |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%
lavaan::summary(SA_example$bestModel)
##             Length Class  Mode     
##  bestModel  1      lavaan S4       
##  bestSyntax 1      -none- character
plot(SA_example) # plot showing how the fit value changes at each step
```

![](README-Simulated%20Annealing%20example-1.png)<!-- -->

It took a total of 1.05 minutes to run the SA example, and a total of
6.76 minutes to run all three together.


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
settings. The time for these examples to converge on a laptop with an
Intel Core i7 8th Gen processor is printed at the bottom.

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
##           Version 0.4.4
##               (o<
##               //\
##               V_/_
##  Package 'ShortForm' version 0.4.4
# using simulated test data and the default values for lavaan.model.specs
# (with one exception), fit a 10-item short form
# first, read in the original or "full" model
data(exampleAntModel) # a character vector for a lavaan model

# then, create the list of the items by the factors
# in this case, all items load onto the general 'Ability' factor
list.items <- list(paste0("Item", 1:56))

# load the data
data(simulated_test_data)
# finally, call the function with some minor changes to the default values.
# since the data is binary, let lavaan know by putting the items in the
# 'ordered' element of the lavaan.model.specs list.
set.seed(1)
abilityShortForm = antcolony.lavaan(data = simulated_test_data,
ants = 5, evaporation = 0.7, antModel = exampleAntModel,
list.items = list.items, full = 56, i.per.f = 15,
lavaan.model.specs = list(model.type = "cfa", auto.var = T, estimator = "default", 
                          ordered = unlist(list.items), int.ov.free = TRUE,
                          int.lv.free = FALSE, auto.fix.first = TRUE, 
                          auto.fix.single = TRUE, std.lv = FALSE, auto.cov.lv.x = TRUE, 
                          auto.th = TRUE, auto.delta = TRUE, 
                          auto.cov.y = TRUE),
factors = 'Ability', steps = 3, fit.indices = c('cfi', 'rmsea'),
fit.statistics.test = "(cfi > 0.90)&(rmsea < 0.10)",
summaryfile = NULL,
feedbackfile = NULL,
max.run = 50, verbose = FALSE)
##  
 Run number 1.           
 Run number 2.           
 Run number 3.           
 Run number 4.           
 Run number 5.           
 Run number 6.           
 Run number 7.           
 Run number 8.           
 Run number 9.           
 Run number 10.           
 Run number 11.           
 Run number 12.           
 Run number 13.           
 Run number 14.           
 Run number 15.           
 Run number 16.           
 Run number 17.           
 Run number 18.           
 Run number 19.           
 Run number 20.           
 Run number 21.           
 Run number 22.           
 Run number 23.           
 Run number 24.           
 Run number 25.           
 Run number 26.           
 Run number 27.           
 Run number 28.           
 Run number 29.           
 Run number 30.           
 Run number 31.           
 Run number 32.           
 Run number 33.           
 Run number 34.           
 Run number 35.           
 Run number 36.           
 Run number 37.           [1] "Compiling results."
abilityShortForm[[1]] # print the results of the final short form
##       cfi rmsea mean_gamma Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8
##  [1,]   1     0      0.616     0     1     0     0     0     0     0     0
##       Item9 Item10 Item11 Item12 Item13 Item14 Item15 Item16 Item17 Item18
##  [1,]     0      0      0      1      0      0      1      1      0      1
##       Item19 Item20 Item21 Item22 Item23 Item24 Item25 Item26 Item27 Item28
##  [1,]      0      0      0      0      0      0      0      1      1      0
##       Item29 Item30 Item31 Item32 Item33 Item34 Item35 Item36 Item37 Item38
##  [1,]      1      1      0      0      0      0      0      0      0      0
##       Item39 Item40 Item41 Item42 Item43 Item44 Item45 Item46 Item47 Item48
##  [1,]      1      0      0      1      0      1      0      0      0      0
##       Item49 Item50 Item51 Item52 Item53 Item54 Item55 Item56
##  [1,]      0      1      0      0      0      0      1      1
plot(abilityShortForm, type = 'pheromone') # the pheromone plot for class "antcolony"
```

![](README-ACO%20example-1.png)<!-- -->

A similar example can be found in the `antcolony.mplus` function, but
requires you to have a valid Mplus installation on the computer. It took
a total of 1.77 minutes to run this example.

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
                             auto.var=TRUE, auto.fix.first=FALSE, std.lv=TRUE,auto.cov.lv.x=FALSE)

# use search.prep to prepare for the Tabu search
ptab <- search.prep(fitted.model = init.model, loadings=TRUE, fcov=FALSE, errors=FALSE)

Tabu_example <- suppressWarnings(tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 20, tabu.size = 10)) # the suppressWarning wrapping hides the lavaan WARNING output from improper models
##  
Running iteration 1 of 20.   
Running iteration 2 of 20.   
Running iteration 3 of 20.   
Running iteration 4 of 20.   
Running iteration 5 of 20.   
Running iteration 6 of 20.   
Running iteration 7 of 20.   
Running iteration 8 of 20.   
Running iteration 9 of 20.   
Running iteration 10 of 20.   
Running iteration 11 of 20.   
Running iteration 12 of 20.   
Running iteration 13 of 20.   
Running iteration 14 of 20.   
Running iteration 15 of 20.   
Running iteration 16 of 20.   
Running iteration 17 of 20.   
Running iteration 18 of 20.   
Running iteration 19 of 20.   
Running iteration 20 of 20.

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

# plot the change in the objective/criterion function over each run
plot(Tabu_example)
```

![](README-Tabu%20example-1.png)<!-- -->

It took a total of 2.14 minutes to run this example.

The next Tabu example demonstrates how to use it to find a short form of
a prespecified length with different data.

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
tabuShort <- tabu.sem.short(initialModel = tabuModel, originalData = tabuData,
                           numItems = c(4,4,4,4), 
                           criterion = tabuCriterion,
                           niter = 10, tabu.size = 10, verbose = FALSE
                           )
##  
Running iteration 1 of 10.   
Running iteration 2 of 10.   
Running iteration 3 of 10.   
Running iteration 4 of 10.   
Running iteration 5 of 10.   
Running iteration 6 of 10.   
Running iteration 7 of 10.   
Running iteration 8 of 10.   
Running iteration 9 of 10.   
Running iteration 10 of 10.

# check the chosen model
lavaan::summary(tabuShort$best.mod)
##  lavaan 0.6-5 ended normally after 18 iterations
##  
##    Estimator                                         ML
##    Optimization method                           NLMINB
##    Number of free parameters                         38
##                                                        
##    Number of observations                           600
##                                                        
##  Model Test User Model:
##                                                        
##    Test statistic                                57.638
##    Degrees of freedom                                98
##    P-value (Chi-square)                           1.000
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
##      Item3             0.541    0.046   11.871    0.000
##      Item4             0.454    0.047    9.706    0.000
##      Item12            0.401    0.046    8.634    0.000
##      Item11            0.452    0.042   10.726    0.000
##    Trait2 =~                                           
##      Item15            0.511    0.044   11.705    0.000
##      Item16            0.425    0.045    9.449    0.000
##      Item19            0.512    0.045   11.344    0.000
##      Item20            0.508    0.043   11.866    0.000
##    Trait3 =~                                           
##      Item36            0.492    0.043   11.482    0.000
##      Item31            0.596    0.044   13.509    0.000
##      Item34            0.772    0.041   18.784    0.000
##      Item35            0.502    0.043   11.775    0.000
##    Trait4 =~                                           
##      Item41            0.760    0.040   18.809    0.000
##      Item40            0.515    0.041   12.482    0.000
##      Item43            0.581    0.042   13.707    0.000
##      Item44            0.517    0.042   12.226    0.000
##  
##  Covariances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Trait1 ~~                                           
##      Trait2            0.889    0.056   15.756    0.000
##      Trait3            0.826    0.048   17.202    0.000
##      Trait4            0.892    0.046   19.562    0.000
##    Trait2 ~~                                           
##      Trait3            0.811    0.044   18.267    0.000
##      Trait4            0.842    0.043   19.570    0.000
##    Trait3 ~~                                           
##      Trait4            0.687    0.040   17.019    0.000
##  
##  Variances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##     .Item3             0.764    0.051   14.981    0.000
##     .Item4             0.900    0.056   16.095    0.000
##     .Item12            0.924    0.056   16.435    0.000
##     .Item11            0.699    0.045   15.663    0.000
##     .Item15            0.721    0.047   15.219    0.000
##     .Item16            0.834    0.052   16.148    0.000
##     .Item19            0.783    0.051   15.405    0.000
##     .Item20            0.688    0.045   15.131    0.000
##     .Item36            0.739    0.047   15.730    0.000
##     .Item31            0.731    0.049   14.926    0.000
##     .Item34            0.431    0.042   10.302    0.000
##     .Item35            0.726    0.046   15.630    0.000
##     .Item41            0.448    0.040   11.179    0.000
##     .Item40            0.680    0.044   15.559    0.000
##     .Item43            0.687    0.046   15.085    0.000
##     .Item44            0.721    0.046   15.647    0.000
##      Trait1            1.000                           
##      Trait2            1.000                           
##      Trait3            1.000                           
##      Trait4            1.000
# plot the changes in the objective function over each iteration
plot(tabuShort)
```

![](README-Tabu%20short%20form-1.png)<!-- -->

It took a total of 1.14 minutes to run this example.

### Simulated Annealing

This example demonstrates the use of simulated annealing for creating
short forms.

``` r
start.time.SA <- Sys.time()
library(ShortForm, quietly = T)
# load simulation data and select columns used in this example
data(simulated_test_data) 
saData <- simulated_test_data[,c(1:12)]

# specify the full model
saModel <- "
Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10
Ability ~ Outcome
"
lavaan.model.specs = list(model.type = "cfa",
  auto.var = TRUE, estimator = "default", ordered = paste0("Item", 1:10), int.ov.free = TRUE,
  int.lv.free = FALSE, std.lv = TRUE, auto.fix.first = FALSE, auto.fix.single =
  TRUE, auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE, auto.cov.y =
  TRUE)

# perform the SA algorithm
set.seed(1)
SA_example <- simulatedAnnealing(initialModel = saModel, originalData = saData, maxSteps = 200, fitStatistic = 'cfi', maximize = FALSE, temperature = "logistic", items = paste0("Item", 1:10), lavaan.model.specs = lavaan.model.specs, maxChanges = 3, maxItems = 5, progress = F)
##  Initializing short form creation.
##  The initial short form is:
##   Ability =~ Item9 + Item4 + Item7 + Item1 + Item2
##  Ability ~ Outcome
##  Using the short form randomNeighbor function.
##  Finished initializing short form options.
##   Current Progress:
plot(SA_example) # plot showing how the fit value changes at each step
```

![](README-Simulated%20Annealing%20example-1.png)<!-- -->

It took a total of 2.05 minutes to run the SA example, and a total of
7.1 minutes to run all four together.

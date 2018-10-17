
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
# using simulated test data and the default values for lavaan.model.specs
# (with one exception), fit a 10-item short form
# first, read in the original or "full" model
data(exampleAntModel) # a character vector for a lavaan model

# then, create the list of the items by the factors
# in this case, all items load onto the general 'Ability' factor
list.items <- list(c('Item1','Item2','Item3','Item4','Item5',
'Item6','Item7','Item8','Item9','Item10',
'Item11','Item12','Item13','Item14','Item15',
'Item16','Item17','Item18','Item19','Item20',
'Item21','Item22','Item23','Item24','Item25',
'Item26','Item27','Item28','Item29','Item30'))

# load the data
data(simulated_test_data)
# finally, call the function with some minor changes to the default values.
# since the data is binary, let lavaan know by putting the items in the
# 'ordered' element of the lavaan.model.specs list.
set.seed(1)
abilityShortForm = antcolony.lavaan(data = simulated_test_data,
ants = 5, evaporation = 0.7, antModel = exampleAntModel,
list.items = list.items, full = 30, i.per.f = 10,
lavaan.model.specs = list(model.type = "cfa", auto.var = T, estimator = "default", 
                          ordered = unlist(list.items), int.ov.free = TRUE,
                          int.lv.free = FALSE, auto.fix.first = TRUE, 
                          auto.fix.single = TRUE, auto.cov.lv.x = TRUE, 
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
 Run number 23.           [1] "Compiling results."
abilityShortForm[[1]] # print the results of the final short form
##       cfi rmsea mean_gamma Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8
##  [1,]   1     0      0.613     0     1     0     0     1     0     0     0
##       Item9 Item10 Item11 Item12 Item13 Item14 Item15 Item16 Item17 Item18
##  [1,]     1      1      0      0      0      0      0      1      0      1
##       Item19 Item20 Item21 Item22 Item23 Item24 Item25 Item26 Item27 Item28
##  [1,]      1      0      0      0      0      0      0      1      0      0
##       Item29 Item30
##  [1,]      1      1
plot(abilityShortForm) # the plots for class "antcolony"
##  $Pheromone
```

![](README-ACO%20example-1.png)<!-- -->

    ##  
    ##  $Gamma

![](README-ACO%20example-2.png)<!-- -->

    ##  
    ##  $Variance

![](README-ACO%20example-3.png)<!-- -->

A similar example can be found in the `antcolony.mplus` function, but
requires you to have a valid Mplus installation on the computer. It took
a total of 5.44 minutes to run this example.

### Tabu Search Algorithm

This example demonstrates how to use the Tabu search for model
specification searches when the original model may be misspecified in
some way.

``` r
start.time.Tabu <- Sys.time()
library(ShortForm, quietly = T)
# load simulation data and select columns used in this example
data(simulated_test_data) 
tabuData <- simulated_test_data[,c(1:10)]

# specify an improper model (improper because the data is actually unidimensional)
tabuModel <- "
Ability =~ Item1 + Item2 + Item3 + Item4
FakeAbility =~ Item5 + Item6 + Item7 + Item8
Ability ~ Outcome
FakeAbility ~ 0*Outcome
Ability ~ 0*FakeAbility"

# fit the initial misspecified model for Tabu
init.model <- lavaan::lavaan(model = tabuModel, data = tabuData, 
auto.var=TRUE, auto.fix.first=FALSE, std.lv=TRUE,auto.cov.lv.x=TRUE)

# use search.prep to prepare for the Tabu search
ptab <- search.prep(fitted.model = init.model, loadings=TRUE, fcov=TRUE, errors=FALSE)

# perform the Tabu search with 100 iterations and a Tabu list size of 5
set.seed(1) # reproduceable
Tabu_example <- suppressWarnings(tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 25, tabu.size = 5)) # the suppressWarning wrapping hides the lavaan WARNING output from improper models
##  
Running iteration 1 of 25.   
Running iteration 2 of 25.   
Running iteration 3 of 25.   
Running iteration 4 of 25.   
Running iteration 5 of 25.   
Running iteration 6 of 25.   
Running iteration 7 of 25.   
Running iteration 8 of 25.   
Running iteration 9 of 25.   
Running iteration 10 of 25.   
Running iteration 11 of 25.   
Running iteration 12 of 25.   
Running iteration 13 of 25.   
Running iteration 14 of 25.   
Running iteration 15 of 25.   
Running iteration 16 of 25.   
Running iteration 17 of 25.   
Running iteration 18 of 25.   
Running iteration 19 of 25.   
Running iteration 20 of 25.   
Running iteration 21 of 25.   
Running iteration 22 of 25.   
Running iteration 23 of 25.   
Running iteration 24 of 25.   
Running iteration 25 of 25.

# check the final model
lavaan::summary(Tabu_example$best.mod)
##  lavaan 0.6-3 ended normally after 56 iterations
##  
##    Optimization method                           NLMINB
##    Number of free parameters                         20
##  
##    Number of observations                          1000
##  
##    Estimator                                         ML
##    Model Fit Test Statistic                      20.483
##    Degrees of freedom                                24
##    P-value (Chi-square)                           0.669
##  
##  Parameter Estimates:
##  
##    Information                                 Expected
##    Information saturated (h1) model          Structured
##    Standard Errors                             Standard
##  
##  Latent Variables:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Ability =~                                          
##      Item1             0.158    0.020    7.956    0.000
##      Item2             0.223    0.022   10.282    0.000
##      Item3             0.197    0.021    9.385    0.000
##      Item4             0.159    0.019    8.183    0.000
##      Item5             0.146    0.019    7.740    0.000
##      Item6             0.190    0.018   10.684    0.000
##      Item7             0.144    0.018    7.862    0.000
##      Item8             0.147    0.019    7.867    0.000
##    FakeAbility =~                                      
##      Item1             0.000                           
##      Item2             0.046    0.071    0.646    0.518
##      Item3            -0.052    0.095   -0.543    0.587
##      Item4             0.000                           
##      Item5             0.230    0.381    0.603    0.546
##      Item6             0.000                           
##      Item7             0.000                           
##      Item8             0.000                           
##  
##  Regressions:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Ability ~                                           
##      FakeAbility       0.000                           
##      Outcome           0.468    0.086    5.432    0.000
##    FakeAbility ~                                       
##      Ability           0.000                           
##      Outcome           0.000                           
##    Outcome ~                                           
##      Ability           0.000                           
##      FakeAbility       0.000                           
##  
##  Covariances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##   .Ability ~~                                          
##     .FakeAbility       0.000                           
##   .Item1 ~~                                            
##     .Item2             0.000                           
##     .Item3             0.000                           
##     .Item4             0.000                           
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##   .Item2 ~~                                            
##     .Item3             0.000                           
##     .Item4             0.000                           
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##   .Item3 ~~                                            
##     .Item4             0.000                           
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##   .Item4 ~~                                            
##     .Item5             0.000                           
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##   .Item5 ~~                                            
##     .Item6             0.000                           
##     .Item7             0.000                           
##     .Item8             0.000                           
##   .Item6 ~~                                            
##     .Item7             0.000                           
##     .Item8             0.000                           
##   .Item7 ~~                                            
##     .Item8             0.000                           
##  
##  Variances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##     .Ability           1.000                           
##     .FakeAbility       1.000                           
##     .Item1             0.221    0.011   20.494    0.000
##     .Item2             0.175    0.014   12.210    0.000
##     .Item3             0.170    0.016   10.324    0.000
##     .Item4             0.213    0.010   20.366    0.000
##     .Item5             0.095    0.175    0.544    0.586
##     .Item6             0.156    0.008   18.361    0.000
##     .Item7             0.189    0.009   20.545    0.000
##     .Item8             0.198    0.010   20.542    0.000
```

It took a total of 4.43 minutes to run this example.

The next Tabu example demonstrates how to use it to find a short form of
a prespecified length with the same data.

``` r
start.time.Tabu <- Sys.time()
library(ShortForm, quietly = T)
# load simulation data and select columns used in this example
data(simulated_test_data) 
tabuData <- simulated_test_data

# specify the initial model. For the short form, no need to fit it.
tabuModel <- "
Ability =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20 + Item21 + Item22 + Item23 + Item24 + Item25 + Item26 + Item27 + Item28 + Item29 + Item30 + Item31 + Item32 + Item33 + Item34 + Item35 + Item36 + Item37 + Item38 + Item39 + Item40 + Item41 + Item42 + Item43 + Item44 + Item45 + Item46 + Item47 + Item48 + Item49 + Item50 + Item51 + Item52 + Item53 + Item54 + Item55 + Item56
Ability ~ Outcome"

# specify the criterion function that the Tabu Search minimizes
# since a larger (closer to 1) CFI is better, take the negative of CFI
# wrap this in a tryCatch in case a model does not converge!
tabuCriterion = function(x) {
  tryCatch(-lavaan::fitmeasures(object = x, fit.measures = 'cfi'),
           error = function(e) Inf)
}

# set the seed to reproduce this example
set.seed(1)
# use the tabuShortForm function
# reduce form to the best 10 items
tabuShort <- tabuShortForm(initialModel = tabuModel, originalData = tabuData,
                           numItems = 10, allItems = paste0("Item", 1:56),
                           criterion = tabuCriterion,
                           niter = 20)
##  Creating initial short form.
##  The initial short form is: 
##  Ability =~ Item15 + Item21 + Item31 + Item49 + Item11 + Item46 + Item48 + Item33 + Item54 + Item3
##  Ability ~ Outcome
##   Current Progress:
  |                                                                       
  |===                                                              |   5%
  |                                                                       
  |======                                                           |  10%
  |                                                                       
  |=========                                                        |  14%
  |                                                                       
  |============                                                     |  19%
  |                                                                       
  |===============                                                  |  24%
  |                                                                       
  |===================                                              |  29%
  |                                                                       
  |======================                                           |  33%
  |                                                                       
  |=========================                                        |  38%
  |                                                                       
  |============================                                     |  43%
  |                                                                       
  |===============================                                  |  48%
  |                                                                       
  |==================================                               |  52%
  |                                                                       
  |=====================================                            |  57%
  |                                                                       
  |========================================                         |  62%
  |                                                                       
  |===========================================                      |  67%
  |                                                                       
  |==============================================                   |  71%
  |                                                                       
  |==================================================               |  76%
  |                                                                       
  |=====================================================            |  81%
  |                                                                       
  |========================================================         |  86%
  |                                                                       
  |===========================================================      |  90%
  |                                                                       
  |==============================================================   |  95%
  |                                                                       
  |=================================================================| 100%
# check the chosen model
lavaan::summary(tabuShort$best.mod)
##  lavaan 0.6-3 ended normally after 33 iterations
##  
##    Optimization method                           NLMINB
##    Number of free parameters                         21
##  
##    Number of observations                          1000
##  
##    Estimator                                         ML
##    Model Fit Test Statistic                      31.220
##    Degrees of freedom                                44
##    P-value (Chi-square)                           0.926
##  
##  Parameter Estimates:
##  
##    Information                                 Expected
##    Information saturated (h1) model          Structured
##    Standard Errors                             Standard
##  
##  Latent Variables:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Ability =~                                          
##      Item1             0.149    0.019    7.892    0.000
##      Item21            0.174    0.018    9.519    0.000
##      Item31            0.195    0.018   10.732    0.000
##      Item49            0.193    0.018   10.639    0.000
##      Item11            0.175    0.017   10.462    0.000
##      Item46            0.156    0.018    8.811    0.000
##      Item48            0.153    0.018    8.563    0.000
##      Item33            0.121    0.018    6.587    0.000
##      Item54            0.255    0.018   13.852    0.000
##      Item3             0.176    0.017   10.155    0.000
##  
##  Regressions:
##                     Estimate  Std.Err  z-value  P(>|z|)
##    Ability ~                                           
##      Outcome           0.456    0.082    5.567    0.000
##  
##  Variances:
##                     Estimate  Std.Err  z-value  P(>|z|)
##     .Item1             0.224    0.011   21.074    0.000
##     .Item21            0.203    0.010   20.416    0.000
##     .Item31            0.194    0.010   19.799    0.000
##     .Item49            0.195    0.010   19.851    0.000
##     .Item11            0.167    0.008   19.948    0.000
##     .Item46            0.195    0.009   20.724    0.000
##     .Item48            0.200    0.010   20.824    0.000
##     .Item33            0.218    0.010   21.485    0.000
##     .Item54            0.176    0.010   17.489    0.000
##     .Item3             0.181    0.009   20.108    0.000
##     .Ability           1.000
# plot the changes in the objective function over each iteration
plot(tabuShort)
```

![](README-Tabu%20short%20form-1.png)<!-- -->

It took a total of 10.15 minutes to run this example.

### Simulated Annealing

This example demonstrates the use of simulated annealing for creating
short forms.

``` r
start.time.SA <- Sys.time()
library(ShortForm, quietly = T)
# load simulation data and select columns used in this example
data(simulated_test_data) 
saData <- simulated_test_data[,c(1:10)]

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
SA_example <- simulatedAnnealing(initialModel = saModel, originalData = saData, maxSteps = 500, fitStatistic = 'cfi', maximize = FALSE, temperature = "logistic", items = paste0("Item", 1:10), lavaan.model.specs = lavaan.model.specs, maxChanges = 3, maxItems = 5, progress = F)
##  Initializing short form creation.
##  The initial short form is:
##   Ability =~ Item3 + Item4 + Item5 + Item7 + Item2
##  Ability ~ Outcome
##  Using the short form randomNeighbor function.
##  Finished initializing short form options.
##   Current Progress:
plot(SA_example) # plot showing how the fit value changes at each step
```

![](README-Simulated%20Annealing%20example-1.png)<!-- -->

It took a total of 5.06 minutes to run the SA example, and a total of
25.08 minutes to run all three together.

It took a total of 1.28 minutes to run the SA example, and a total of
11.76 minutes to run all three together.

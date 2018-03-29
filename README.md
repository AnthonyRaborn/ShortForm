
# ShortForm

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ShortForm)](http://cran.r-project.org/package=ShortForm)
[![Travis-CI Build
Status](http://travis-ci.org/AnthonyRaborn/ShortForm.svg?branch=master)](http://travis-ci.org/AnthonyRaborn/ShortForm)

Automatic Short Form Creation for scales. Currently, the Ant Colony
Optimization (ACO) Algorithm and the Tabu search are implemented. The
original R implementation for the ACO algorithm is from Leite, Huang, &
Marcoulides (2008) <doi:10.1080/00273170802285743>, while the Tabu
search function was taken from Marcoulides & Falk (2018)
<doi:10.1080/10705511.2017.1409074>.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("AnthonyRaborn/ShortForm") # the developmental version
install.packages("ShortForm") # the CRAN-approved version
```

## Usage

Here are some (slightly modified) examples from the help documentation
using lavaan.

### ACO Algorithm

``` r
library(ShortForm)
# using simulated test data and the default values for lavaan.model.specs
# (with one exception), fit a 20-item short form
# first, read in the original or "full" model
data(exampleAntModel) # a character vector for a lavaan model

# then, create the list of the items by the factors
# in this case, all items load onto the general 'Ability' factor
list.items <- list(c('Item1','Item2','Item3','Item4','Item5',
'Item6','Item7','Item8','Item9','Item10',
'Item11','Item12','Item13','Item14','Item15',
'Item16','Item17','Item18','Item19','Item20',
'Item21','Item22','Item23','Item24','Item25',
'Item26','Item27','Item28','Item29','Item30',
'Item31','Item32','Item33','Item34','Item35',
'Item36','Item37','Item38','Item39','Item40',
'Item41','Item42','Item43','Item44','Item45',
'Item46','Item47','Item48','Item49','Item50',
'Item51','Item52','Item53','Item54','Item55','Item56'))

# load the data
data(simulated_test_data)
# finally, call the function with some minor changes to the default values.
# since the data is binary, let lavaan know by putting the items in the
# 'orderd' element of the lavaan.model.specs list.
abilityShortForm = antcolony.lavaan(data = simulated_test_data,
ants = 5, evaporation = 0.7, antModel = exampleAntModel,
list.items = list.items, full = 56, i.per.f = 20,
lavaan.model.specs = list(model.type = "cfa", auto.var = T, estimator = "default", ordered = unlist(list.items), 
                          int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = TRUE, 
                          auto.fix.single = TRUE, auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE, 
                          auto.cov.y = TRUE)
factors = 'Ability', steps = 3, fit.indices = c('cfi', 'rmsea'),
fit.statistics.test = "(cfi > 0.95)&(rmsea < 0.05)",
summaryfile = 'summary.txt',
feedbackfile = 'iteration.html',
max.run = 500)
abilityShortForm[[1]] # print the results of the final short form
```

A similar example can be found in the `antcolony.mplus` function, but
requires you to have a valid Mplus installation on the computer.

### Tabu Search Algorithm

``` r
library(ShortForm)
# load simulation data and select columns used in this example
data(simulated_test_data) 
tabuData <- simulated_test_data[,c(1:10)]

# specify an improper model (improper because the data is actually unidimensional)
tabuModel <- "
Ability =~ Item1 + Item2 + Item3 + Item4
FakeAbility =~ Item5 + Item6 + Item7 + Item8
Ability ~ Outcome
FakeAbility ~ 0*Outcome"

# fit the initial misspecified model for Tabu
init.model <- lavaan::lavaan(model = tabuModel, data = tabuData, 
auto.var=TRUE, auto.fix.first=FALSE, std.lv=TRUE,auto.cov.lv.x=TRUE)

# use search.prep to prepare for the Tabu search
ptab <- search.prep(fitted.model = init.model, loadings=TRUE, fcov=TRUE, errors=FALSE)

# perform the Tabu search with 100 iterations and a Tabu list size of 5
Tabu_example <- tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 100, tabu.size = 5)

# check the final model
summary(Tabu_example$best.mod)
```

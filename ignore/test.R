library(psych)
library(ShortForm)
# create simulation data from the `psych` package
 # four factors, 12 items each, 48 total items
 # factor loading matrix - not quite simple structure
 fxMatrix <-
   matrix(
     data = c(
       rep(x = c(.9, .7, .5, .3), times = 3),
       rep(0.2, times = 3 * 4 * 3), # first factor loadings

       rep(0.2, times = 3 * 4),
       rep(x = c(.9, .7, .5, .3), times = 3),
       rep(0.2, times = 3 * 4 * 2), # second factor loadings

       rep(0.2, times = 3 * 4 * 2),
       rep(x = c(.9, .7, .5, .3), times = 3),
       rep(0.2, times = 3 * 4), # third factor loadings

       rep(0.2, times = 3 * 4 * 3),
       rep(x = c(.9, .7, .5, .3), times = 3) # fourth factor loadings
     ),
     ncol = 4
   )
 # factor correlation matrix - all factors uncorrelated
 PhiMatrix <-
   matrix(data = c(
     1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1, 0,
     0, 0, 0, 1
   ), ncol = 4)
 tabuData <-
   psych::sim(
     fx = fxMatrix,
     Phi = PhiMatrix,
     n = 1000,
     raw = TRUE
   )$observed # observed is the simulated observed data

 # NOTE: you must specify the model such that each factor is on a single line!
 # otherwise, the algorithm will not work correctly!
 tabuModel <- "
 Trait1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 +
 Item7 + Item8 + Item9 + Item10 + Item11 + Item12
 Trait2 =~ Item13 + Item14 + Item15 + Item16 + Item17 +
 Item18 + Item19 + Item20 + Item21 + Item22 + Item23 + Item24
 Trait3 =~ Item25 + Item26 + Item27 + Item28 + Item29 + Item30 +
 Item31 + Item32 + Item33 + Item34 + Item35 + Item36
 Trait4 =~ Item37 + Item38 + Item39 + Item40 + Item41 +
 Item42 + Item43 + Item44 + Item45 + Item46 + Item47 + Item48
 "

 colnames(tabuData) <- paste0("Item", 1:48)

 trial1 <- simulatedAnnealing(
    initialModel = tabuModel,
    originalData = tabuData,
    maxSteps = 150, maxItems = c(4,4,4,4), items = paste0("Item", 1:48),
    setChains = 1, maximize = F, maxChanges = 4
 )
 plot(trial1) # shows the resulting model

trial2 <- simulatedAnnealing(
   initialModel = tabuModel,
   originalData = tabuData,
   maxSteps = 100, maxItems = c(4,4,4,4), items = paste0("Item", 1:48),
   setChains = 2, maximize = T, maxChanges = 4
 )
 plot(trial2) # shows the resulting model



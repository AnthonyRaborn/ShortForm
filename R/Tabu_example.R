# data("exampleAntModel")
# data(simulated_test_data)
# 
# tabuModel <- "
# Ability =~ Item1 + Item2 + Item3 + Item4
# FakeAbility =~ Item5 + Item6 + Item7 + Item8
# Ability ~ Outcome
# FakeAbility ~ 0*Outcome"
# tabuData <- simulated_test_data[,c(1:10)]
# 
# init.model <- lavaan::lavaan(model = tabuModel, data = tabuData, auto.var=TRUE, auto.fix.first=FALSE, std.lv=TRUE,auto.cov.lv.x=TRUE)
# 
# ptab <- search.prep(fitted.model = init.model,loadings=TRUE,fcov=TRUE,errors=FALSE)
# # additional.param <- 'Item1 ~~ 0.5*Item3'
# # ptab <- add.param(fitted.model = init.model, ptab = ptab, syntax = additional.param)
# 
# trial <- tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 15, tabu.size = 5)
# 
# 
# HolzingerModel <- '
#  visual  =~ x1 + x2 + x3
#  textual =~ x4 + x5 + x6
#  speed   =~ x7 + x8 + x9
# '
# 
# init.model <- lavaan::lavaan(model = HolzingerModel, data = HolzingerSwineford1939, auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE)
# 
# ptab <- search.prep(fitted.model = init.model,loadings=TRUE,fcov=TRUE,errors=FALSE)
# trial <- tabu.sem(init.model = init.model, ptab = ptab, obj = AIC, niter = 15, tabu.size = 5)
# 

linearTemperature <- function(currentStep, maxSteps) {
  currentTemp <- (maxSteps - (currentStep)) / maxSteps
}

quadraticTemperature <- function(currentStep, maxSteps) {
  currentTemp <- ((maxSteps - (currentStep)) / maxSteps)^2
}

logisticTemperature <- function(currentStep, maxSteps) {
  x <- 1:maxSteps
  x.new <- scale(x, center = T, scale = maxSteps / 12)
  currentTemp <- 1 / (1 + exp((x.new[(currentStep + 1)])))
}

currentStep <- 500
maxSteps <- 5000

test_that(
  "each temperature function returns the correct value given currentStep and maxSteps", {

    # linear temp should equal 4500/5000 = 0.9
    expect_equal(
      linearTemperature(
        currentStep = currentStep,
        maxSteps = maxSteps
        ),
      (1-500/5000)
    )

    # quadratic temp should equal (4500/5000)^2 = 0.81
    expect_equal(
      quadraticTemperature(
        currentStep = currentStep,
        maxSteps = maxSteps
      ),
      (1-500/5000)^2
    )

    # logistic temp should equal the formula given
    expect_equal(
      logisticTemperature(
        currentStep = currentStep,
        maxSteps = maxSteps
      ),
      1/(1 + exp((scale(1:maxSteps, center=T, scale = maxSteps/12)[currentStep+1])))
    )
  }
)
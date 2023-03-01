Power <- function(jaspResults, dataset, options) {
  # Check options and overwrite certain settings if necessary
  options <- .check(jaspResults, options)

  # Run the appropriate test
  if (options$test == "independentSamplesTTest") {
    .runTtestIS(jaspResults, options)
  } else if (options$test == "pairedSamplesTTest") {
    .runTtestPS(jaspResults, options)
  } else if (options$test == "oneSampleTTest") {
    .runTtestOneS(jaspResults, options)
  } else if (options$test == "oneSampleZTest") {
    .runZtestOneS(jaspResults, options)
  } else if (options$test == "oneSampleProportion") {
    .runTest1P(jaspResults, options)
  } else if (options$test == "twoSamplesProportion") {
    .runTest2P(jaspResults, options)
  } else if (options$test == "oneSampleVarianceRatio") {
    .runTest1Var(jaspResults, options)
  } else if (options$test == "twoSamplesVarianceRatio") {
    .runTest2Var(jaspResults, options)
  } else if (options$test == "oneSamplePoisson") {
    .runTest2Var(jaspResults, options)
  } else if (options$test == "twoSamplesPoisson") {
    .runTest2Var(jaspResults, options)
  } else if (options$test == "anova") {
    .runAnova(jaspResults, options)
  }

  return()
}

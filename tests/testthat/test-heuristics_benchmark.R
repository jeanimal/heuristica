########################################
# Benchmark times of key heuristics    #
########################################

context("heuristics_benchmark")

# The asserts have VERY loose standards-- they will fail only
# for a major regression.  More useful are the printouts so we
# could hunt through Travis if there is a major regression.
# Ideally we would write that output to a database so we could
# see stats over time.

test_that("Benchmark ttbModel on city_population", {
  ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
  times <- system.time(allRowPairApply(city_population, heuristicsProb(ttb)))
  print("ttb")
  print(times)

  # On 11 Jan on Jean's macbook:
  #    user  system elapsed
  #   0.137   0.006   0.143
})

test_that("Benchmark ttbModel on city_population NEW simpleRowPairApply", {
  ttb <- ttbModel(city_population, 3, c(4:ncol(city_population)))
  times <- system.time(simpleRowPairApply(city_population,
                                          createHeuristicWrapperFn2(ttb)))
  print("ttb NEW simpleRowPairApply")
  print(times)
  
  # In April 2016 on Jean's macbook:
  #    user  system elapsed
  #   0.118   0.003   0.122
})

test_that("Benchmark regModel on city_population", {
  model <- regModel(city_population, 3, c(4:ncol(city_population)))
  times <- system.time(allRowPairApply(city_population, heuristicsProb(model)))
  print("reg")
  print(times)
  expect_lt(times[[2]], 1)

  # On 11 Jan on Jean's macbook:
  #    user  system elapsed
  #   0.138   0.001   0.139
})

test_that("Benchmark regModel on city_population NEW simpleRowPairApply", {
  model <- regModel(city_population, 3, c(4:ncol(city_population)))
  times <- system.time(simpleRowPairApply(city_population,
                                          createHeuristicWrapperFn2(model)))
  print("regModel NEW simpleRowPairApply")
  print(times)

  # In April 2016 on Jean's macbook:
  #    user  system elapsed
  #   0.115   0.001   0.116
})



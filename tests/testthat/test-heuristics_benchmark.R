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
  times <- system.time(rowPairApply(city_population, heuristics(ttb)))
  print("ttb")
  print(times)

  # 2016-01-11 Jan on Jean's macbook:
  #    user  system elapsed
  #   0.137   0.006   0.143
  #
  # 2016-05-23 Jan on Jean's macbook-- a REGRESSION
  # user  system elapsed 
  # 0.173   0.002   0.182 
})

test_that("Benchmark regModel on city_population", {
  reg <- regModel(city_population, 3, c(4:ncol(city_population)))
  times <- system.time(rowPairApply(city_population, heuristics(reg)))
  print("reg")
  print(times)
  expect_lt(times[[2]], 1)

  # On 11 Jan on Jean's macbook:
  #    user  system elapsed
  #   0.138   0.001   0.139
  #
  # 2016-05-23 Jan on Jean's macbook-- a REGRESSION
  # user  system elapsed 
  # 0.150   0.002   0.158 
})


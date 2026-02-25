test_that("cpue calculates simple ratio correctly", {
  expect_equal(cpue(catch = 100, effort = 10), 10)
  expect_equal(cpue(catch = 50, effort = 25), 2)
})

test_that("cpue works with vectors of data", {
  catches <- c(100, 200, 300)
  efforts <- c(10, 10, 10)
  expected_results <- c(10, 20, 30)

  expect_equal(cpue(catches, efforts), expected_results)
})

test_that("cpue returns numeric values", {
  expect_type(cpue(100, 10), "double")
})

test_that("cpue gear_factor scales correctly", {
  expect_equal(cpue(catch = 100, effort = 10, gear_factor = 0.5), 5)
  expect_equal(
    cpue(catch = 100, effort = 10),
    cpue(catch = 100, effort = 10, gear_factor = 1)
  )
})

test_that("cpue handles zero catch and missing data", {
  expect_equal(cpue(catch = 0, effort = 10), 0)

  expect_true(is.na(cpue(NA_real_, 10)))
  expect_true(is.na(cpue(100, NA_real_)))
})


test_that("cpue works with generated data", {
  data <- generate_fishing_data(n = 5)

  result <- cpue(data$catch, data$effort)

  expect_equal(
    result,
    c(34.053, 9.065, 19.239, 135.640, 6.372), #dput(cpue(data$catch, data$effort)) provides a vector you can paste
    tolerance = 1e-3
  )
})


test_that("cpue matches reference data", {
  result <- cpue(reference_data$catch, reference_data$effort)

  expect_equal(result, reference_data$expected_cpue)
})

test_that("cpue provides informative message when verbose", {
  expect_message(
    cpue(c(100, 200), c(10, 20), verbose = TRUE),
    "Processing 2 records"
  )
    expect_no_message(cpue(100, 10))
})

test_that("cpue errors when input is not numeric", {
  expect_snapshot(
    cpue("five", 10),
    error = TRUE
  )
})

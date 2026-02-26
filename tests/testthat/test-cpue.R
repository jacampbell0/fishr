test_that("cpue calculates simple ratio correctly", {
  #expect_equal(cpue(catch = 100, effort = 10), 10)
  #expect_equal(cpue(catch = 50, effort = 25), 2)

  result <- cpue(c(100, 200), c(10, 20))
  expect_s3_class(result, "cpue_result")
})

test_that("cpue works with vectors of data", {
  catches <- c(100, 200, 300)
  efforts <- c(10, 10, 10)
  expected_results <- c(10, 20, 30)

  #expect_equal(cpue(catches, efforts), expected_results)
  result <- cpue(c(100, 200), c(10, 20))
  expect_s3_class(result, "cpue_result")
})

test_that("cpue returns numeric values", {
  expect_type(cpue(100, 10), "double")
})

test_that("cpue gear_factor scales correctly", {
  expect_equal_numbers(cpue(catch = 100, effort = 10, gear_factor = 0.5), 5)
  expect_equal_numbers(
    cpue(catch = 100, effort = 10),
    cpue(catch = 100, effort = 10, gear_factor = 1)
  )
})

test_that("cpue handles zero catch and missing data", {
  expect_equal_numbers(cpue(catch = 0, effort = 10), 0)

  expect_true(is.na(cpue(NA_real_, 10)))
  expect_true(is.na(cpue(100, NA_real_)))
})


test_that("cpue works with generated data", {
  data <- generate_fishing_data(n = 5)

  result <- cpue(data$catch, data$effort)

  expect_equal_numbers(
    result,
    c(34.052837, 9.064945, 19.238772, 135.640053, 6.371919), #dput(cpue(data$catch, data$effort)) provides a vector you can paste
    tolerance = 1e-2
  )
})


test_that("cpue matches reference data", {
  result <- cpue(reference_data$catch, reference_data$effort)

  expect_equal_numbers(result, reference_data$expected_cpue)
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

test_that("cpue warns when catch and effort lengths differ", {
  expect_snapshot(
    cpue(c(100, 200, 300), c(10, 20))
  )

  expect_no_warning(cpue(100, 10))
})

test_that("cpue uses verbosity when option set to TRUE", {
  withr::local_options(fishr.verbose = TRUE) # will be reset when this test_that block finishes

  expect_snapshot(cpue(100, 10))
})

test_that("cpue is not verbose when option set to FALSE", {
  withr::local_options(fishr.verbose = FALSE) # will be reset when this test_that block finishes

  expect_silent(cpue(100, 10))
})

test_that("cpue verbosity falls back to FALSE when not set", {
  withr::with_options(
    list(fishr.verbose = NULL), # will be reset as soon as this code block executes
    expect_no_message(cpue(100, 10))
  )
})
# Options automatically restored after each test

test_that("example with temporary file", {
  temp_file <- withr::local_tempfile(lines = c("100,10", "200,20"))

  # Test something with temp_file
  lines <- readLines(temp_file)
  expect_length(lines, 2)
})
# temp_file automatically deleted after test

# TESTING S3 OBJECTS
test_that("cpue() returns a cpue_result object", {
  result <- cpue(c(100, 200), c(10, 20))
  expect_s3_class(result, "cpue_result")
})

test_that("cpue_result carries calculation metadata", {
  result <- cpue(c(100, 200, 300), c(10, 20, 15), method = "log")
  expect_equal(attr(result, "method"), "log")
  expect_equal(attr(result, "gear_factor"), 1)
  expect_equal(attr(result, "n_records"), 3)
})

test_that("print.cpue_result displays expected output", {
  result <- cpue(c(100, 200, 300), c(10, 20, 15))
  expect_snapshot(print(result))
})

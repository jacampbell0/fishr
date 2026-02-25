test_that("biomass_index calculates correctly", {
  expect_equal(biomass_index(cpue = 10, area_swept = 5), 50)
  expect_equal(biomass_index(cpue = 20, area_swept = 2.5), 50)
})

test_that("biomass_index handles vectors", {
  cpue_vals <- c(10, 20, 30)
  area_vals <- c(5, 5, 5)

  expect_equal(biomass_index(cpue_vals, area_vals), c(50, 100, 150))
})

test_that("biomass_index throws error on invalid input", {
  expect_snapshot(biomass_index("ten", 5), error = TRUE)
  expect_snapshot(biomass_index(10, "five"), error = TRUE)
})

test_that("biomass handles zero and missing data", {
  expect_equal(biomass_index(cpue = 0, area_swept = 10), 0)

  expect_true(is.na(biomass_index(NA_real_, 10)))
  expect_true(is.na(biomass_index(100, NA_real_)))
})

test_that("biomass_index uses verbosity when option set to TRUE", {
  withr::local_options(fishr.verbose = TRUE) # will be reset when this test_that block finishes

  expect_snapshot(biomass_index(100, 10))
})

test_that("biomass_index is not verbose when option set to FALSE", {
  withr::local_options(fishr.verbose = FALSE) # will be reset when this test_that block finishes

  expect_silent(biomass_index(100, 10))
})

test_that("biomass_index verbosity falls back to FALSE when not set", {
  withr::with_options(
    list(fishr.verbose = NULL), # will be reset as soon as this code block executes
    expect_no_message(biomass_index(100, 10))
  )
})

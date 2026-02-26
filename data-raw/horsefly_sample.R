# data-raw/horsefly_sample.R
# Run this script once to create data/horsefly_sample.rda.

horsefly_sample <- data.frame(
  set_number = c(1L, 2L, 3L, 4L, 5L),
  catch = c(3L, 0L, 5L, 2L, 4L),
  effort = c(1L, 1L, 1L, 1L, 1L),
  stringsAsFactors = FALSE
)

usethis::use_data(horsefly_sample, overwrite = TRUE)

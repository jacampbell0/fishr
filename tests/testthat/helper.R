# generate fake fishing data using a function

generate_fishing_data <- function(n = 10) {
  withr::local_seed(67) #need data to be the same each time so we know what outputs to expect
  data.frame(
    catch = runif(n, 10, 500),
    effort = runif(n, 1, 20),
    gear_factor = runif(n, 1, 5)
  )
}

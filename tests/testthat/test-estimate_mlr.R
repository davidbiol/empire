## data
x1 <- c(1.30, 1.40, NA, 1.54, 1.30, NA, 1.50, 1.60, 1.41, 1.34)
x2 <- c(51, 60, 69, 73, 56, 75, 80, NA, 58, 70)
x3 <- c(4.8, NA, 5.8, 6.5, 5.3, 7.0, 8.1, 7.8, 5.9, 6.1)
x4 <- c(115, 130, 138, 148, 122, 152, 160, 155, 135, 140)
df <- data.frame(x1,x2,x3,x4)

## Check it is accurately estimating

test_that("Values are accurately estimated", {
  object_mlr <- empire::estimate_mlr(df)
  expect_equal(object_mlr$est_values, c(1.386230, 1.499429, 72.574734, 5.633177))
})

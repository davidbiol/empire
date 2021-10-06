## data
x1 <- c(1.30, 1.40, NA, 1.54, 1.30, NA, 1.50, 1.60, 1.41, 1.34)
x2 <- c(51, 60, 69, 73, 56, 75, 80, NA, 58, 70)
x3 <- c(4.8, NA, 5.8, 6.5, 5.3, 7.0, 8.1, 7.8, 5.9, 6.1)
x4 <- c(115, 130, 138, 148, 122, 152, 160, 155, 135, 140)
df <- data.frame(x1,x2,x3,x4)

## Check it is accurately estimating

test_that("Values are accurately estimated", {
  object_mean <- empire::impute_mean(df)
  expect_equal(sapply(df, mean, na.rm = TRUE), sapply(object_mean$new_data, mean)) #The mean shouldn't change with imputation by mean
  expect_equal(object_mean$imp_values, c(1.423750, 1.423750, 65.777778, 6.366667))
})


## Error testing
df_list <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

test_that("Error if data input is not a data frame or a matrix", {
  expect_error(empire::impute_mean(df_list))
})

x5 <- c(117, NA, 118, 128, 145, 161, 170, 155, "hello", 140)
df_with_character <- data.frame(x1,x2,x3,x4,x5)
df_matrix_with_character <- matrix(c(x1,x2,x3,x4,x5), ncol = 5)

test_that("Error if data input is a vector of characters", {
  expect_error(empire::impute_mean(df_with_character))
  expect_error(empire::impute_mean(df_matrix_with_character))
})

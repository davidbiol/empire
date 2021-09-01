## data
x1 <- c(1.30, 1.40, NA, 1.54, 1.30, NA, 1.50, 1.60, 1.41, 1.34)
x2 <- c(51, 60, 69, 73, 56, 75, 80, NA, 58, 70)
x3 <- c(4.8, NA, 5.8, 6.5, 5.3, 7.0, 8.1, 7.8, 5.9, 6.1)
x4 <- c(115, 130, 138, 148, 122, 152, 160, 155, 135, 140)
df <- data.frame(x1,x2,x3,x4)

## Check it is working right

test_that("Positions are accurately calculated", {
  expect_equal(pos_miss(df), data.frame(Row=c(3,6,8,2), Col=c(1,1,2,3)))
})

## Testing that the input is a data frame or matrix
test_that("The function works if the input is a data frame", {
  expect_equal(class(df), "data.frame")
})

df_matrix <- matrix(c(x1,x2,x3,x4), ncol = 4)

test_that("The function works if the input is a matrix", {
  expect_equal(class(df_matrix), c("matrix", "array"))
})

test_that("The function works if the observations are numbers", {
  df_is_numeric <- all(unlist(lapply(df, is.numeric)), TRUE)
  expect_equal(df_is_numeric, TRUE)

  df_matrix_is_numeric <- all(unlist(lapply(df_matrix, is.numeric)), TRUE)
  expect_equal(df_matrix_is_numeric, TRUE)
})


## Error testing
df_list <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

test_that("Error if data input is not a data frame or a matrix", {
  expect_error(empire::pos_miss(df_list))
})

x5 <- c(117, NA, 118, 128, 145, 161, 170, 155, "hello", 140)
df_with_character <- data.frame(x1,x2,x3,x4,x5)
df_matrix_with_character <- matrix(c(x1,x2,x3,x4,x5), ncol = 5)

test_that("Error if data input is a vector of characters", {
  expect_error(empire::pos_miss(df_with_character))
  expect_error(empire::pos_miss(df_matrix_with_character))
})


## Check it is accurately estimating

test_that("Values are accurately estimated", {
  if(requireNamespace(package="VIM", quietly = TRUE)){
    set.seed(1)
    object_ridge <- empire::estimate_ridge(data = VIM::sleep[,1:7], diff = 10)
    expect_equal(object_ridge$est_values, c(0.9995111, 9.9745165, 13.1997367, 3.1194330, 5.2204292, 10.2208011,  10.3676909, 8.6938941, 6.6434951, 7.2781046, 9.9873426, 1.9426418, 8.8345145, 7.7946387, 2.2677372, 2.2523016, 3.2597789, -0.2026194, 2.0173824, 2.4007446, 1.8060663, 2.4095600, 2.2490386, -0.1921541, 1.8585441, 3.5322546, 5.3154582, 7.3892969, 7.2328072, 4.2635906, 10.2583431, 13.1156767, 11.4926358, 11.5012406, 90.2055916, 182.0149551, -28.0189186, 86.5126792))
  } else {
    message("Package \"VIM\" needed for this test to work. Please install it.",
         call. = FALSE)
  }

})

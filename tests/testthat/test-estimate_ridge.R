## Check it is accurately estimating

test_that("Values are accurately estimated", {
  if(requireNamespace(package="VIM", quietly = TRUE)){
    set.seed(1)
    object_ridge <- empire::estimate_ridge(data = VIM::sleep[,1:7], diff = 10)
    expect_equal(object_ridge$est_values, c(1.0040323, 10.0001470,  13.1925366,   3.1997271,   5.2673486,  10.2112291, 10.3850487,   8.7206378,   7.0608961,   7.2792524,  10.0134017,   2.0622996,
                                            8.8775902, 7.6045407,   2.2594985,   2.2423374,   3.2212381,  -0.1216797,
                                            2.0119367,   2.3838146,   1.7998571,   2.8687598,   2.2402257,  -0.1207674,
                                            1.8682556,   3.1821714,   5.8229920,   6.5320063,   7.2972341,   5.9680253,
                                            10.6948587,  10.4470140,  10.4647420,  10.3280916,  58.1896515, 158.8522599,
                                            44.4784188,  33.2502686))
  } else {
    message("Package \"VIM\" needed for this test to work. Please install it.",
         call. = FALSE)
  }

})

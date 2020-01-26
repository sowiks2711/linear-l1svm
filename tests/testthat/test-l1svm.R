test_that("Stop if data not a matrix",
  expect_error(l1svm(1, 1), "Argument data is not a matrix!")
)

test_that("Stop if data has different row nr than length of target",
  expect_error(l1svm(diag(10), c(1:12)), "Argument data has different row nr than length of target argument!")
)

test_that("Preprocess input gives proper values",
  expect_that(preprocess_input(diag(12), c(rep(1,10),2,2)), is_equivalent_to(list(data=diag(12), n=12, p=12, c(rep(1,10),-1,-1))))
)

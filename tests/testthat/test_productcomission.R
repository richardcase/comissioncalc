context("Product Comission")

test_that("Test ProductComission(1109.35)",{
  actual <- product_calc(1109.35)
  expect_equal(actual, 113.90)
})

test_that("Test ProductComission(141.67)",{
  actual <- product_calc(141.67)
  expect_equal(actual, 7.08)
})

test_that("Test ProductComission(428.8)",{
  actual <- product_calc(428.80)
  expect_equal(actual, 27.88)
})

test_that("Test ProductComission(100) Not Equal",{
  actual <- product_calc(100)
  expect_that( actual != 1000, is_true() )
})

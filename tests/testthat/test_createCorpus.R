test_that("corpus proper messages and warnings", {
  expect_error(pv(FV = "1000", .09, n = 5))
  expect_error(pv(FV = list(1, 2, 3), .09, n = 5))
  expect_message(pv(FV = 1000, .36, n = 5))
})
test_that("corpus created", {
  expect_error(pv(FV = "1000", .09, n = 5))
  expect_error(pv(FV = list(1, 2, 3), .09, n = 5))
  expect_message(pv(FV = 1000, .36, n = 5))
})
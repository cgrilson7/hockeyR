context("test dryscrape:st functions")

test_that("numeric absolutes", {
  expect_equal(nabs(5), 5)
  expect_equal(nabs("5"), 5)
  expect_equal(nabs("5."), 5)
})

test_that("Moving average", {
  expect_true(is.na(moving(2)))
  expect_equal(moving(1:5), c(NA, NA, NA, NA, 3))
  expect_equal(moving(rep(5, 7)), c(NA, NA, NA, NA, 5, 5, 5))
})

test_that("Log Loss", {
  expect_equal(log_loss(1, 0.5), 0.6931472, tolerance = .00001)
  expect_equal(log_loss(c(1,1), c(0.5, 0.5)), 0.6931472, tolerance = .00001)
  expect_equal(log_loss(1,1), 0, tolerance = .00001)
  expect_equal(log_loss(1,0), 34.53878, tolerance = .00001)
  expect_equal(log_loss(1,0, allow_inf = TRUE), Inf)
})
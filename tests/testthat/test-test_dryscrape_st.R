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

test_that("Brier Score", {
  expect_equal(brier(1, 0.5), 0.25)
  expect_equal(brier(0, 0.5), 0.25)
  expect_equal(brier(1, 1), 0)
  expect_equal(brier(0, 1), 1)
  expect_equal(brier(1,0), 1)
})

test_that("NA types", {
  expect_equal(na_as_string(c(0, '5', 'test', 'NA', NA)), c('0', '5', 'test', 'NA', 'NA'))
  expect_warning(expect_equal(na_as_zero(c(0, '5', 'test', 'NA', NA)), c(0, 5, 0,0,0)), 'NAs introduced by coercion')
  expect_equal(na_if_null(0), 0)
  expect_equal(na_if_null('test'), 'test')
  expect_equal(na_if_null(NA), NA)
  expect_equal(na_if_null(NULL), NA)
})

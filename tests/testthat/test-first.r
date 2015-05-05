require(testthat)

test_that("first/vector", {

  x <- letters
  expect_identical(first(x), x[1])
  expect_identical(first(x, 3), x[1:3])
  expect_equivalent(res <- first(x, 3, keep = TRUE), x[1:3])
  expect_identical(attributes(res)$keep, x[4:length(x)])

  ## Conditions //
  expect_identical(first(x, 27), x)
  expect_message(expect_identical(first(x, 27, strict = 1), x))
  expect_warning(expect_identical(first(x, 27, strict = 2), x))
  expect_error(first(x, 27, strict = 3))

})

test_that("first/list", {

  x <- as.list(letters)
  expect_identical(first(x), x[1])
  expect_identical(first(x, 3), x[1:3])
  expect_equivalent(res <- first(x, 3, keep = TRUE), x[1:3])
  expect_identical(attributes(res)$keep, x[4:length(x)])

  ## Conditions //
  expect_identical(first(x, 27), x)
  expect_message(expect_identical(first(x, 27, strict = 1), x))
  expect_warning(expect_identical(first(x, 27, strict = 2), x))
  expect_error(first(x, 27, strict = 3))

})

test_that("first/data.frame", {

  x <- data.frame(a= 1:5, b = letters[1:5], stringsAsFactors = FALSE)

  ## Rows //
  expect_identical(first(x), x[1,])
  expect_identical(first(x, 3), x[1:3,])
  expect_equivalent(first(x, 3, keep = TRUE), x[1:3,])

  ## Conditions:
  expect_identical(first(x, 6), x)
  expect_message(expect_identical(first(x, 6, strict = 1), x))
  expect_warning(expect_identical(first(x, 6, strict = 2), x))
  expect_error(first(x, 6, strict = 3))

  ## Columns //
  expect_identical(first(x, margin = 2), x[, 1, drop = FALSE])
  expect_identical(first(x, 2, margin = 2), x[,1:2])
  expect_identical(first(x, 3, margin = 2), x[,1:2])

  expect_identical(first(x, margin = 2, drop = TRUE), x[,1])
  expect_identical(first(x, 2, margin = 2, drop = TRUE), x[,1:2])

  ## Conditions:
  expect_identical(first(x, 3, margin = 2), x)
  expect_message(expect_identical(first(x, 3, margin = 2, strict = 1), x))
  expect_warning(expect_identical(first(x, 3, margin = 2, strict = 2), x))
  expect_error(first(x, 3, margin = 2, strict = 3))

})

require(testthat)

test_that("last/vector", {

  x <- letters
  expect_identical(last(x), "z")
  expect_identical(last(x, 3), c("x", "y", "z"))
  expect_equivalent(last(x, 3, keep = TRUE), c("x", "y", "z"))

  ## Conditions //
  expect_identical(last(x, 30), x)
  expect_message(expect_identical(last(x, 30, strict = 1), x))
  expect_warning(expect_identical(last(x, 27, strict = 2), x))
  expect_error(last(x, 30, strict = 3))

})

test_that("last/list", {

  x <- as.list(letters)
  expect_identical(last(x), list("z"))
  expect_identical(last(x, 3), as.list(c("x", "y", "z")))
  expect_equivalent(last(x, 3, keep = TRUE), as.list(c("x", "y", "z")))

  ## Conditions //
  expect_identical(last(x, 30), x)
  expect_message(expect_identical(last(x, 30, strict = 1), x))
  expect_warning(expect_identical(last(x, 27, strict = 2), x))
  expect_error(last(x, 30, strict = 3))

})


test_that("last/data.frame", {

  x <- data.frame(a= 1:5, b = letters[1:5], stringsAsFactors = FALSE)

  ## Rows //
  expect_identical(last(x), x[5,])
  expect_identical(last(x, 3), x[3:5,])
  expect_equivalent(last(x, 3, keep = TRUE), x[3:5,])

  ## Conditions:
  expect_identical(last(x, 6), x)
  expect_message(expect_identical(last(x, 6, strict = 1), x))
  expect_warning(expect_identical(last(x, 6, strict = 2), x))
  expect_error(last(x, 6, strict = 3))

  ## Columns //
  expect_identical(last(x, margin = 2), x[,2, drop = FALSE])
  expect_identical(last(x, 2, margin = 2), x[,1:2])
  expect_identical(last(x, 3, margin = 2), x[,1:2])

  expect_identical(last(x, margin = 2, drop = TRUE), x[,2])
  expect_identical(last(x, 2, margin = 2, drop = TRUE), x[,1:2])

  ## Conditions:
  expect_identical(last(x, 3, margin = 2), x)
  expect_message(expect_identical(last(x, 3, margin = 2, strict = 1), x))
  expect_warning(expect_identical(last(x, 3, margin = 2, strict = 2), x))
  expect_error(last(x, 3, margin = 2, strict = 3))

})

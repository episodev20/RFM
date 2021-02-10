test_that("add works", {
  expect_equal(addFirstTwo(c(2,2)), 4)
})

test_that("output is numeric", {
  expect_type(addFirstTwo(c(2,2)), "double")
})

test_that("length is 1 element", {
  expect_length(addFirstTwo(c(2,2)), 1)
})

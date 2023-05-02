test_that("example test", {

  expect_equal(2 * 2, 4)

})

test_that("output format meql", {

  # check correct output format with correct data
  data_meq <- meql(example_data)
  expect_true(ncol(data_meq) == 16)

})

test_that("Expect error with test dataset having wrong parameter names", {

  load(file = "test_data.Rda")
  expect_error(meql(test_data))

})


# test_that("", {
#
#
#
# })

# test_that("coordinates must be present", {
#
#   x_coordinate <-
#   export_error()
#
# })


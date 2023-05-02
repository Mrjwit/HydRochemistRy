test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("all columns are present", {

  data_stat <- DescriptiveStatistics(example_data)

  expect_true(ncol(data_stat) == 11)

})


test_that("Expect warning for missing data", {

  load(file = "test_data.Rda")

  expect_warning(DescriptiveStatistics(test_data))

  #data_stat <- DescriptiveStatistics(test_data)
  #expect_true(ncol(data_stat) == 11)

})

# test_that("", {
#
#   load(file = "test_data.Rda")
#
#   #data_stat <- DescriptiveStatistics(test_data)
#   #expect_true(ncol(data_stat) == 11)
#
# })

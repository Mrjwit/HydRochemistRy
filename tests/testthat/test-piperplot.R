test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# test toPercent
# test_that("Test if percentage calculation from meql works", {
#   load(file = "testdata.Rda")
#
#   data_meq <- meql(data)
#   data_perc <- toPercent(data_meq)
#
#   expect_true(rowSums(Na, Ca, Mg, K) == 100)
#
# })

test_that("expect error for missing parameter", {

  load(file = "test_data.Rda")
  expect_error(PiperPlot(test_data))

})



require(makeJAGSmask)

context("exampleData")
# =============================

test_that("exampleData is unchanged",  {
  data(exampleData)
  expect_s3_class(patch, "data.frame")
  expect_equivalent(round(colMeans(patch)), c(157571, 787443))
  expect_s3_class(traps, c("traps", "data.frame"))
  expect_equivalent(round(colMeans(traps)), c(159426, 788927))
  expect_is(JAGSoutput, "array")
  expect_equivalent(dim(JAGSoutput), c(1350, 10, 2))
  expect_equivalent(round(mean(JAGSoutput, na.rm=TRUE), 4), 12.5767)
  expect(sum(is.na(JAGSoutput)), 12086)
} )

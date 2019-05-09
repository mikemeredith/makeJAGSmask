

require(makeJAGSmask)

context("simSCR")
# =============================

test_that("simSCR is unchanged",  {
  data(simSCR)
  expect_s3_class(simSCR$patchDF, "data.frame")
  expect_s4_class(simSCR$patchSP, "SpatialPolygons")
  expect_s4_class(simSCR$patchR, "RasterLayer")
  expect_equivalent(round(colMeans(simSCR$patchDF)), c(157571, 787443))
  expect_s3_class(simSCR$traps, c("traps", "data.frame"))
  expect_equivalent(round(colMeans(simSCR$traps)), c(156239, 789891))
  expect_is(simSCR$sims.list$S, "array")
  expect_equivalent(dim(simSCR$sims.list$S), c(1500, 30, 2))
  expect_equivalent(round(mean(simSCR$sims.list$S), 4), 32.1058)
  expect_equivalent(sum(is.na(simSCR$sims.list$S)), 0)
} )

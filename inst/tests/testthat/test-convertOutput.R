

require(makeJAGSmask)
# library(testthat)

context("convertOutput")
# =============================

test_that("convertOutput gives same output",  {
  data(simSCR)
  secrmask <- with(simSCR, secr::make.mask(traps, spacing=2000, type='polygon',
    poly=patchSP, cell='any'))
  mymask <- convertMask(secrmask, simSCR$traps, plot=FALSE)

  res <- convertOutput(simSCR$sims.list$S, JAGSmask = mymask)
  expect_is(res, "array")
  expect_equal(dim(res), dim(simSCR$sims.list$S))
  expect_equivalent(sum(is.na(res)), sum(is.na(simSCR$sims.list$S)))
  expect_equivalent(round(mean(res)), 505490)

  # list input
  inp <- list(y = simSCR$sims.list$S[, , 2],
              x = simSCR$sims.list$S[, , 1])  # needs new version of convertOutput
  res2 <- convertOutput(inp, JAGSmask = mymask)
  expect_is(res2, "list")
  expect_equal(length(res2), 2)
  expect_equivalent(sum(is.na(res2[[1]])), sum(is.na(res[,,1])))
  expect_equivalent(round(mean(res2[[1]], na.rm=TRUE)),
    round(mean(res[, , 1], na.rm=TRUE)))
  expect_equivalent(round(mean(res2[[2]], na.rm=TRUE)),
    round(mean(res[, , 2], na.rm=TRUE)))

  # Error messages
  expect_error(convertOutput(JAGSoutput),
    "argument \"JAGSmask\" is missing")
  expect_error(convertOutput(trees, mymask),
    "invalid input")
  expect_error(convertOutput(JAGSoutput, trees),
    "'trees' is not a valid 'JAGSmask' object")
 } )



require(makeJAGSmask)

context("convertRaster")
# =============================

test_that("convertRaster gives same output",  {
  data(simSCR)
  mymask2 <- convertRaster(simSCR$patchR, simSCR$traps, plot=FALSE)
  expect_silent(plot(mymask2, verify=TRUE))

  expect_s3_class(mymask2, "JAGSmask")
  expect_setequal(names(mymask2),
    c("habMat", "covMat", "trapMat", "upperLimit", "area", "pixelWidth"))
  # habMat
  expect_is(mymask2$habMat, "matrix")
  expect_equal(dim(mymask2$habMat), c(65, 59))
  expect_equal(range(mymask2$habMat), c(0, 1))  # Fails if any NAs
  expect_equal(sum(mymask2$habMat), 1996)
  # trapMat
  expect_is(mymask2$trapMat, "matrix")
  expect_equal(dim(mymask2$trapMat), c(89, 2))
  expect_setequal(rownames(head(mymask2$trapMat)),
    c("1.2", "1.3", "1.4", "1.5", "1.6", "1.7"))
  expect_gt(min(mymask2$trapMat), 1)  # Fails if any NAs
  expect_lt(max(mymask2$trapMat), 65)
  expect_equivalent(round(colMeans(mymask2$trapMat), 3), c(32.881, 27.954))
  # upperLimit
  expect_is(mymask2$upperLimit, "numeric")
  expect_equivalent(mymask2$upperLimit, c(66, 60))
  # area
  expect_is(mymask2$area, "numeric")
  expect_equal(mymask2$area, 1.996e9)
  # pixelWidth
  expect_is(mymask2$pixelWidth, "numeric")
  expect_equal(mymask2$pixelWidth, 1000)
  # boundingbox
  bbox <- attr(mymask2, "boundingbox")
  expect_is(bbox, "matrix")
  expect_equal(dim(bbox), c(4, 2))
  expect_equivalent(colMeans(bbox), c(156858, 792437))
  # origin
  ogin <- attr(mymask2, "origin")
  expect_is(ogin, "numeric")
  expect_equivalent(ogin, c(123358, 761937))

  # Error messages
  expect_error(convertRaster(simSCR$patchR),
    "argument \"traps\" is missing")
  expect_error(convertRaster(letters, traps),
    "'letters' is not a valid 'raster' object")
  # expect_error(convertRaster(simSCR$patchR, trees),
    # "'trees' is not a valid 'traps' object")
 } )

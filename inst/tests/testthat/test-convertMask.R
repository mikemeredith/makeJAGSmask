

require(makeJAGSmask)

context("convertMask")
# =============================

test_that("convertMask gives same output",  {
  data(exampleData)
  secrmask1 <- secr::make.mask(traps, spacing=2000, type='polygon', poly=patch)
  mymask1 <- convertMask(secrmask1, traps, plot=FALSE)
  expect_output(plot(mymask1, verify=TRUE),
    "The following traps appear to be in bad habitat")
  expect_silent(plot(mymask1, verify=FALSE))

  secrmask2 <- secr::make.mask(traps, spacing=2000, type='polygon',
    poly=patch, cell='any')
  mymask2 <- convertMask(secrmask2, traps, plot=FALSE)
  expect_silent(plot(mymask2, verify=TRUE))

  expect_s3_class(mymask2, "JAGSmask")
  expect_setequal(names(mymask2),
    c("habMat", "trapMat", "upperLimit", "area", "pixelWidth"))
  # habMat
  expect_is(mymask2$habMat, "matrix")
  expect_equal(dim(mymask2$habMat), c(32, 30))
  expect_equal(range(mymask2$habMat), c(0, 1))  # Fails if any NAs
  expect_equal(sum(mymask2$habMat), 585)
  # trapMat
  expect_is(mymask2$trapMat, "matrix")
  expect_equal(dim(mymask2$trapMat), c(107, 2))
  expect_setequal(rownames(head(mymask2$trapMat)),
    c("1.A3", "1.B2", "1.B3", "1.C2", "1.C3", "2.A1"))
  expect_gt(min(mymask2$trapMat), 1)  # Fails if any NAs
  expect_lt(max(mymask2$trapMat), 31)
  expect_equivalent(round(colMeans(mymask2$trapMat), 3), c(18.584, 14.314))
  # upperLimit
  expect_is(mymask2$upperLimit, "numeric")
  expect_equivalent(mymask2$upperLimit, c(33, 31))
  # area
  expect_is(mymask2$area, "numeric")
  expect_equal(mymask2$area, 2.34e9)
  # pixelWidth
  expect_is(mymask2$pixelWidth, "numeric")
  expect_equal(mymask2$pixelWidth, 2000)
  # boundingbox
  bbox <- attr(mymask2, "boundingbox")
  expect_is(bbox, "matrix")
  expect_equal(dim(bbox), c(4, 2))
  expect_equivalent(colMeans(bbox), c(156258, 792298))
  # origin
  ogin <- attr(mymask2, "origin")
  expect_is(ogin, "numeric")
  expect_equivalent(ogin, c(122258, 760298))

  # Error messages
  expect_error(convertMask(secrmask2),
    "argument \"secrtraps\" is missing")
  expect_error(convertMask(letters, traps),
    "'letters' is not a valid 'mask' object")
  expect_error(convertMask(secrmask2, trees),
    "'trees' is not a valid 'traps' object")
  
  
 } )

library(MyoManager)

test_that("valid input file and frame number", {

  mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
  mFib = selectFrame(mouse,2)
  mNuc = selectFrame(mouse,3)

  expect_type(mFib, "double")
  expect_type(mNuc, "double")
  expect_s4_class(mFib, "Image")
  expect_s4_class(mNuc, "Image")
  # check that processed image is a single frame
  expect_equal(length(dim(mFib)), 2)
  expect_equal(length(dim(mNuc)), 2)
})

test_that("invalid input file", {

  # find a way to test trace back error?
  #"EBImage object must be an array of pixel values." from validImage()
  expect_error(selectFrame(bird,2))
})

test_that("missing or invalid frame number", {

  mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))

  expect_error(selectFrame(mouse, 0), "frame_number must be between 1 and  3")
  expect_error(selectFrame(mouse, 5), "frame_number must be between 1 and  3")
  expect_error(selectFrame(mouse, 1.5), "frame_number must be an integer.")
  expect_error(selectFrame(mouse, 'one'), "frame_number must be an integer.")
  expect_error(selectFrame(mouse), "argument \"frame_number\" is missing, with no default")

})

test_that("invalid brush_size or brush_shape in blurImage", {

  rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))

  expect_error(blurImage(rabbit, 'five', 'line'), "Please enter a valid numeric brush_size")
  expect_error(blurImage(rabbit, 5, 'square'))

})

test_that("invalid input in intensityCtrl", {

  rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))

  expect_error(intensityCtrl(rabbit, '2', 'three'), "brightness and contrast must be numeric")

})
#End

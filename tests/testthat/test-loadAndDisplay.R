context("Loading and displaying images")
library(TestingPackage)

test_that("valid file from path", {

  human <- loadImage(system.file('extdata/Human_01.tiff', package = 'MyoManager'))

  expect_type(human, "double")
  expect_s4_class(human, "Image")
})

test_that("valid file from source link", {

  human <- loadImage(system.file('extdata/Human_01.tiff', package = 'MyoManager'))

  expect_type(human, "double")
  expect_s4_class(human, "Image")
})

test_that("multiple valid files from path", {

  human <- system.file('extdata/Human_01.jpg', package = 'MyoManager')
  mouse <- system.file('extdata/Mouse_01.jpg', package = 'MyoManager')
  rabbit <- system.file('extdata/Rabbit_01.jpg', package = 'MyoManager')
  x <- list(human, mouse, rabbit)
  img_set <- loadImage(x)

  expect_type(img_set, "list")
  expect_length(img_set, 3)
  expect_type(img_set[[1]], "double")
  expect_s4_class(img_set[[1]], "Image")
})

context("Checking for invalid user input for load and display functions")
test_that("invalid source path", {

  fish <- system.file('extdata/zebrafish_01.tiff', package = 'MyoManager')

  expect_error(loadImage(fish), "input is an invalid file path.")
})

test_that("one or more invalid source path", {

  human <- system.file('extdata/Human_01.jpg', package = 'MyoManager')
  mouse <- system.file('extdata/Mouse_01.jpg', package = 'MyoManager')
  fish <- system.file('extdata/zebrafish_01.jpg', package = 'MyoManager')
  x <- list(human, mouse, fish)

  expect_error(loadImage(x), "input is an invalid file path.")
})
# [END]

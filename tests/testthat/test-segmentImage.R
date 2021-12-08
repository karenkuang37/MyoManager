library(MyoManager)

test_that("invalid / missing inputs", {

  mouse <- loadImage(system.file('extdata/Mouse_02.tif', package = 'MyoManager'))

  expect_error(segmentImage(mouse, 2, 3, "cell wall"), "show_structure must be 'cell', 'nuclei', or 'both'.")
  expect_error(segmentImage(mouse, 2), "Please indicate frame numbers of cell AND nuclei")
  expect_error(segmentImage(mouse, -1, 5, "cell")) # Error in selectFrame(img, cell_frame) :
                                                   # frame_number must be between 1 and 3
  expect_error(segmentImage(mouse, 2, 'cell')) # Error in selectFrame(img, nuc_frame) : frame_number must be an integer.

})

#End

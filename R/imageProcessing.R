#' Frame selection.
#'
#' The following function selects 1 frame from a composite image to proceed with analysis.
#'
#' Microscopy data typically comes in multiple frames (or channels), each showing
#' a different color of fluorescence/immuno-staining (e.g. nuclei, cell body, protein
#' markers). The composite image is often shown in colored to differentiate cellular
#' structures, while individual frames are typically shown in greyscale.
#'
#' @param img An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#' @param frame_number A \code{numeric} value specifying a frame in img.
#'
#' @return A single-frame grayscale \code{Image} indicated by frame_number.
#'
#' @examples
#' # Example 1
#' # visualize the nuclei channel in a tiff file
#' human = loadImage(system.file('extdata/Human_01.tiff', package='MyoManager'))
#' nuc = selectFrame(human, 3)
#' viewImage(nuc)
#'
#'
#' # Example 2
#' # visualize the fiber(cell body) channel in a tif image
#' mouse = loadImage(system.file('extdata/Mouse_02.tif', package='MyoManager'))
#' fib = selectFrame(mouse,2)
#' viewImage(nuc)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{(https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @export
selectFrame <- function(img, frame_number){

  # check image file is of suitable type
  validImage(img)

  x = frame_number

  # check image dimensions for total number of frames
  if(length(dim(img)) == 2){
    total_frame_number = 1
  }else{
    total_frame_number = dim(img)[3]
  }

  # check if input is an integer
  if(!is.wholenumber(x)){
    stop(
      paste("frame_number must be an integer.")
    )
  }


  #check if input is a valid frame number
  if(x<1 || x>total_frame_number){
    stop(
      paste("frame_number must be between 1 and ", total_frame_number)
    )
  }

  if(total_frame_number > 1){
    return(img[,,x])
  }else{
    return(img)
  }
}
#'
#' Private Helper
#'
#' R's base function is.integer(x) does not test if x contains integer numbers.
#' Hence the added helper to test for integer.
#'
#' @references
#' R Core Team (2021). R: A language and environment for statistical computing. R
#' Foundation for Statistical Computing, Vienna, Austria.
#' \href{https://www.R-project.org/}{link}.
#'
#' @param x A numeric value to be tested upon.
#' @param tol A default tolerance level.
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  if(!is.numeric(x)){
     return(FALSE)
  }
  abs(x - round(x)) < tol
}
#'
#' Apply a blurring filter
#'
#' The following function uses \code{\link[EBImage]{makeBrush}} to generate
#' a filter of choice and applies it to the image. Blurring tends to be
#' useful prior to analyzing individual objects in an image with ill-defined
#' edges and/or uneven intensities.
#'
#' @param img An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#' @param brush_size A \code{numeric} value containing the size of the brush
#'  in pixels. This should be an odd number; even numbers are rounded to the
#'  next odd one, i.e., size = 4 has the same effect as size = 5. Default is 5.
#' @param brush_shape A \code{character} vector indicating the shape of the brush.
#' Can be box, disc, diamond,Gaussian or line. Default is box.
#' @param sigma An optional numeric containing the standard deviation of the
#' Gaussian shape.This argument is relevant only for the
#' Gaussian brush shape.Default is 0.3.
#'
#' @return A blurred version of the input image.
#'
#' @examples
#' # Example 1
#' # select nuclei channel in a tiff file
#' mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' mNuc = selectFrame(mouse,3)
#' viewImage(mNuc)
#'
#' # apply the blurring brush
#' img_blur <- blurImage(mNuc, 11, 'gaussian', sigma = 5)
#'
#' # visualize blurred nuclei - brightening is optional (for display purpose)
#' viewImage(intensityCtrl(img_blur, 0, 3))
#'
#' # Example 2
#' rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
#' rNuc = selectFrame(rabbit, 3)
#' viewImage(rNuc)
#' img_blur = blurImage(rNuc, 11, 'line')
#' viewImage(img_blur)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{(https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @import EBImage
#' @export
blurImage <- function(img, brush_size = 5, brush_shape=c('box', 'disc', 'diamond', 'Gaussian', 'line'), sigma = 0.3){

  # check image file is of suitable type
  validImage(img)

  # generates a 2D matrix containing the desired brush.
  if(!is.numeric(brush_size)){
    stop(
      paste("Please enter a valid numeric brush_size")
    )
  }
  w = EBImage::makeBrush(size = brush_size, shape = brush_shape, sigma = sigma)

  # apply the blurring filter on selected image
  img = EBImage::filter2(img, w)

  return(img)
}
#'
#' Simple image manipulations
#'
#' The following function manipulates the brightness and the degree of contrast
#' of an image.
#'
#' As EBImage objects are stored as matrices, they can be manipulated with
#' all R mathematical operators. This includes + to control the brightness,
#' * to control the degree of contrast, or ^ to control the gamma correction parameter.
#'
#' @param img An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#' @param brightness A \code{numeric} value containing the level of brightness to be
#' applied to the image. Default is 0. (no increased/decreased brightness)
#' @param contrast A \code{numeric} value (>0) containing the degree of contrast to be
#' applied to the image. Default is 1.(no increased/decreased contrast)
#'
#' @return A \code{Image} with adjusted brightness and/or contrast.
#'
#' @examples
#' # Example 1: decrease brightness and increase contrast
#' # (good for object identification)
#' mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' mNuc = selectFrame(mouse, 3)
#' viewImage(mNuc)
#'
#' mouse_enhanced = intensityCtrl(mNuc, -0.2, 3)
#' viewImage(mouse_enhanced)
#'
#' # Example 2: increase brightness and decrease contrast
#' human =loadImage(system.file('extdata/Human_01.tiff', package='MyoManager'))
#' hNuc = selectFrame(human, 3)
#' viewImage(hNuc)
#'
#' human_enhanced = intensityCtrl(hNuc, -0.2, 3)
#' viewImage(human_enhanced)
#' @importFrom methods is
#' @export
intensityCtrl <- function(img, brightness = 0, contrast = 1){

  # check image file is of suitable type
  validImage(img)

  # check if input is numeric
  if(!is.numeric(brightness)||!is.numeric(contrast)){
    stop(
      paste("brightness and contrast must be numeric.")
    )
  }

  # check if contrast is positive
  if(!missing(contrast)){
    if(contrast <= 0){
      stop(
        paste("contrast must be a numeric value > 0")
      )
    }
  }
  img = (img + brightness)*contrast
  return(img)
}
# [END]

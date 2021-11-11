#' (For multi-frame images), frame selection.
#'
#' The following function is a wrap around \code{\link[EBImage]{getFrame}} to
#' select 1 frame to proceed with analysis.
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
#' @examples
#' # Example 1
#' # visualize the nuclei channel in a tiff file
#' human = readImage(system.file('extdata/Human_01.tiff', package='MyoManager'))
#' nuc = getFrame(human, 3)
#' viewImage(nuc)
#'
#'
#' # Example 2
#' # visualize the fiber(cell body) channel in a jpg image
#' mouse = readImage(system.file('extdata/Mouse_01.jpg', package='MyoManager'))
#' fib = getFrame(mouse,2)
#' viewImage(nuc)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{(https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @importFrom EBImage getFrame numberOfFrames
#' @export
frameSelect <- function(img, frame_number){

  x = frame_number
  n = numberOfFrames(img)

  #check if input is missing
  if(missing(frame_number)){
    stop(
      paste("please specify frame_number.")
    )
  }

  # check if input is an integer
  if(!is.integer(x)){
    stop(
      paste("frame_number must be an integer.")
    )
  }

  #check if input is a valid frame number
  if(x<1 || x>n){
    stop(
      paste("frame_number must be between 1 and ", n)
    )
  }

  getFrame(img, x)
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
#' @param brush_shape A \cold{character} vector indicating the shape of the brush.
#' Can be box, disc, diamond,Gaussian or line. Default is box.
#' @param sigma An optional numeric containing the standard deviation of the
#' Gaussian shape.This argument is relevant only for the
#' Gaussian brush shape.Default is 0.3.
#'
#'
#' @examples
#' # Example 1
#' # select the nuclei channel in a tiff file
#' mouse = readImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' nuc = getFrame(mouse,3)
#'
#' # apply the blurring brush
#' img_blur = blurImage(nuc, size = 11, shape = 'gaussian', sigma = 5)
#'
#' # visualize blurred nuclei - brightening is optional (for display purpose)
#' display(img_blur * 2, method = "raster")
#'
#' # Example 2
#' rabbit = readImage(system.file('extdata/Rabbit_01.tiff', package='MyoManager'))
#'
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{(https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @importFrom EBImage makeBrush filter2
#' @export
blurImage <- function(img, brush_size, brush_shape, sigma = 0.3){

  # generates a 2D matrix containing the desired brush.
  w = makeBrush(size = 11, shape = 'gaussian', sigma = sigma)

  # apply the blurring filter on selected image
  img_blur = filter2(nuc, w)
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
#' @param
#'
#' @param
#'
#' @examples
#' # Example 1
#'
#' # Example 2
#'
#' @references
#'
#' @export
makePretty <- function(img, brightness, constrast){

  # check if input is numeric

  img_bright = img*degree
}
# [END]

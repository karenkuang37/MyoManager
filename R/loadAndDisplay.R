#' Loading images
#'
#' The following function is a wrap around \code{\link[magick]{image_read}} and
#' \code{\link[magick]{as_EBImage}} to load one or more images from files.
#'
#' Magick can handle for a variety of image formats (over 200), EBImage has a
#' superior display quality but only supports the three common image formats
#' jpg, png, and tiff. Hence by loading images with Magick and converting them
#' to EBImage objects for display, we can avoid unsupported image types while
#' maximizing display functionalities.
#'
#' @param image_path A \code{character} vector of one or more paths to image files
#'
#' @return LoadImage does not return. ViewImage displays an image object
#' processed by LoadImage using R graphics.
#'
#' @examples
#' # Example 1
#' #load sample tiff distributed with the package
#' library(MyoManager)
#' image <- loadImage(system.file('extdata/Human_01.tiff', package = 'MyoManager'))
#'
#' \dontrun{
#' # Example 2
#' #load sample jpeg from public link
#' library(MyoManager)
#' image <- loadImage("https://user-images.githubusercontent.com/60583839/141215629-f19d4a77-c5f0-491f-9262-b22cd59739e3.jpg")
#'
#'}
#' @references
#'Jeroen Ooms (2021). magick: Advanced Graphics and Image-Processing in R. R
#'package version 2.7.3. \href{https://CRAN.R-project.org/package=magick}{link}
#'
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{(https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @importFrom magick image_read as_EBImage
#' @export

loadImage <- function(image_path) {

  # load multiple images
  if(length(image_path) > 1) {
    lapply(image_path, function(x) {

      # stop and return error if file doesn't exist
      if(!file.exists(x)) {
        stop(
          paste("input is an invalid file path.")
        )
      }

      # load image
      img <- image_read(x)
      as_EBImage(img)
    })
  } else {
    # stop and return error if file doesn't exist
    if(!file.exists(image_path)) {
      stop(
        paste("input is an invalid file path.")
      )
    }
    # load images
    img <- image_read(image_path)
    as_EBImage(img)
  }
}
#'
#' Displays image
#'
#' Prompts R's graphic display window to open, this device supports single and multi-frame
#' images as well as different degrees of zoom, making it easy to visualize detailed microscopy data.
#'
#' @param image_obj An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#'
#' @example
#' #Example 1
#' #display a sample tiff distributed with the package
#' #for tiff images with multiple channels, click to scroll through the frames
#' library(MyoManager)
#' image <- loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' viewImage(image)
#'
#' #Example 2
#' #display a sample  jpeg from source link
#' library(MyoManager)
#' image <- loadImage("https://user-images.githubusercontent.com/60583839/141215629-f19d4a77-c5f0-491f-9262-b22cd59739e3.jpg")
#' viewImage(image)
#'
#' @importFrom EBImage display
#' @export
viewImage <- function(image_obj){

  # check image file is of suitable type
  validImage(image_obj)

  EBImage::display(image_obj)
}
#'
#' Private Helper
#'
#' checks whether 'i' is a suitable image
#'
#' @param image_obj An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
validImage <- function(image_obj) {
  if(is(image_obj, "Image"))
    TRUE
  else
    stop(paste(image_obj, 'EBI image object must be an array'))
}
# [END]

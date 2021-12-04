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
#' @param image_path A \code{character} vector of one or more paths or URLs to image files
#'
#' @return An object of \code{Image} class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#'
#' @examples
#' # Example 1
#' #load sample tiff distributed with the package
#' library(MyoManager)
#' image <- loadImage(system.file('extdata/Human_01.tiff', package = 'MyoManager'))
#'
#'
#' # Example 2
#' #load sample jpeg from public link
#' library(MyoManager)
#' image <- loadImage("https://user-images.githubusercontent.com/60583839/141215629-f19d4a77-c5f0-491f-9262-b22cd59739e3.jpg")
#'
#'
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
      img <- if(is.character(x)){
        if(file.exists(x)){
          img <- magick::image_read(x)
        } else if(grepl("^https?://", x)){
          img <- magick::image_read(x)
        }else {
        stop(
          paste("input is an invalid file path.")
        )
      }}

      # convert image to EBImage object
      return(magick::as_EBImage(img))
    })
  } else {

    # stop and return error if file doesn't exist
    img <- if(is.character(image_path)){
      if(file.exists(image_path)){
        img <- magick::image_read(image_path)
      } else if(grepl("^https?://", image_path)){
        img <- magick::image_read(image_path)
      }else {
        stop(
          paste("input is an invalid file path.")
        )
      }}

    # convert image to EBImage object
    return(magick::as_EBImage(img))
  }
}
#'
#' Interactive display of image
#'
#' Prompts R's graphic display window to open, this device supports single and multi-frame
#' images as well as different degrees of zoom, making it easy to visualize detailed microscopy data.
#'
#' Alternatively use plot(image) command to make a simple graph of the first frame.
#'
#' @param image_obj An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#' @param color_mode A \code{numeric} string containing the color mode of the image
#' which can be either \emph{0} for Grayscale or \emph{2} for Color. *Note it does
#' not change the pixel values of the image, only how R Graphics displays it.
#' If missing, default to Grayscale.
#' @param image_title A \code{character} string specifying the title of the display image.
#'
#' @return ViewImage does not return, it displays an image object
#' processed by LoadImage using R graphics.
#'
#' @examples
#' #Example 1
#' #display a sample tiff distributed with the package
#' #for tiff images with multiple channels, click to scroll through the frames
#' library(MyoManager)
#' mouse <- loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' viewImage(mouse, 2)
#'
#' #Example 2
#' #display a sample jpeg from source link
#' library(MyoManager)
#' rabbit <- loadImage("https://user-images.githubusercontent.com/60583839/141215629-f19d4a77-c5f0-491f-9262-b22cd59739e3.jpg")
#' viewImage(rabbit, 0)
#'
#' @import EBImage
#' @export
#'
viewImage <- function(image_obj, color_mode = c(0, 2), image_title = NULL){

  # check image file is of suitable type
  validImage(image_obj)

  # choosing the type of display
  if(!missing(color_mode)){

    # check for correct color mode choice
    test <- try(color_mode == 0||color_mode == 2, silent = TRUE)
    iserror <- inherits(test, "try-error")
    if(iserror){
      stop(
        paste("color_mode must be either 0 (Grayscale) or 2 (Color).")
      )
    }

    if(color_mode == 0||color_mode == 2){
      EBImage::colorMode(image_obj) <- color_mode
    } else {
      stop(
        paste("color_mode must be either 0 (Grayscale) or 2 (Color).")
        )
    }
  }
  suppressWarnings(EBImage::display(image_obj, title = image_title))
}
#'
#' Private Helper
#'
#' checks whether input object is a suitable image
#'
#' @param image_obj An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#'
#' @importFrom methods is
validImage <- function(image_obj) {
  test <- try(methods::is(image_obj, "Image"), silent = TRUE)
  iserror <- inherits(test, "try-error")
  if(iserror){
    stop(
      paste("EBImage object must be an array of pixel values.")
    )
  } else if(methods::is(image_obj, "Image")){
    TRUE
  }
  else{
    stop(paste(image_obj, 'EBImage object must be an array of pixel values.'))
  }
}
# [END]

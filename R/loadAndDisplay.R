#' Loading and displaying images
#'
#' The following functions handles several types of image data (jpeg, png, tiff)
#' by importing images and prompting a R graphics window for display.
#' The graphics device supports multiple frames and different degrees of zoom,
#' which makes it easy to visualize detailed microscopy data.
#'
#' @param image_path A \code{character} vector of one or more paths to image files
#'
#' @param image_obj An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#'
#' @return LoadImage does not return. ViewImage displays an image object
#' processed by LoadImage using R graphics.
#'
#' @examples
#' # Example 1
#'
#' \dontrun{
#' # Example 2
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
#' @importFrom EBImage display
#'

LoadImage <- function(image_path) {

  # load multiple images
  if(length(image_path) > 1) {
    lapply(image_path, function(x) {

      # stop and return error if file doesn't exist
      if(!file.exists(x)) {
        stop(
          paste(x, 'is invalide file path.')
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
        paste(image_path, 'is an invalid file path.')
      )
    }
    # load images
    img <- image_read(image_path)
    as_EBImage(img)
  }
}
#'
ViewImage <- function(image_obj){

  # check image file is of suitable type
  ValidImage(image_obj)

  EBImage::display(image_obj)
}
#'
#' Private Helper
#'
#' checks whether 'i' is a suitable image
#'
#' @inheritParams loadAndDisplay
#'
ValidImage <- function(image_obj) {
  if(is(image_obj, "Image"))
    TRUE
  else
    stop(paste(image_obj, 'EBI image object must be an array'))
}
# [END]

#' (For multi-frame images), frame selection.
#'
#' The following function is a wrap around \code{\link[EBImage]{getFrame}} to
#' select 1 frame to proceed with analysis.
#'
#' When it comes to counting cellular objects (most commonly nuclei),
#' The key steps are:
#' blur the image
#' apply a threshold to turn nuclei into 'blobs'
#' count the 'blobs'
#'
#' @param img An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#' @param frame_number A \code{numeric} value specifying a frame in img.
#'
#' @return
#'
#' @examples
#' # Example 1
#' #
#'
#'
#' # Example 2
#'
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{(https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @importFrom EBImage thresh opening fillHull bwlabel
#' @export
countNuclei <- function(img){

  # apply a threshold computes binary mask
  mask = thresh(img_blur, w=10, h=10, offset=0.05)

  # opening is an erosion followed by a dilation performed by the mask
  nmask = opening(mask, makeBrush(5, shape='disc'))

  # fillHull fills in holes in objects
  nmask = fillHull(nmask)

  # the bwlabel() function labels and 'counts' the blurred blobs
  nbnuclei <- max(bwlabel(nmask))

  # outputs the total count
  cat('Number of nuclei in this image =', nbnuclei,'\n')
  return(nbnuclei)
}
# [END]

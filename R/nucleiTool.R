#' Count the number of distinct objects in an image (nuclei)
#'
#' The following function uses several functions from \code{\link[EBImage]} to
#' perform counting of cell nuclei in selected image. *Frame of only nuclei is
#' recommended (typically frame #3), counting accuracy decreases if composite
#' image is used.
#'
#' When it comes to counting cellular objects (most commonly nuclei),
#' The key steps are:
#' - enhance image if objects have week signals or ill-defined edges
#'   (e.g. blurring turns nuclei into identifiable 'blobs')
#' - apply a threshold to turn Greyscale image binary so every pixel is either 0 or 1
#' - count objects in the foreground (with pixel value of 1)
#'
#' @param img An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#'
#' @return Returns a \code{numeric} value of the number of nuclei counted.
#'
#' @examples
#' # Example 1
#' rabbit = readImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
#' rNuc = selectFrame(rabbit, 3)
#' countNuclei(rNuc)
#' viewImage(rNuc)
#'
#' # Example 2
#' mouse = readImage(system.file('extdata/Mouse_01.tif', package='MyoManager'))
#' mNuc = selectFrame(mouse, 3)
#' countNuclei(mNuc)
#' viewImage(mNuce)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#'Xiaolu Yang, Xuanjing Shen, Jianwu Long, Haipeng Chen,
#'(2012): An Improved Median-based Otsu Image Thresholding Algorithm,
#'\emph{AASRI Procedia},Volume 3, pp. 468-473,
#'\href{https://doi.org/10.1016/j.aasri.2012.11.074}{link}
#'\url{https://www.sciencedirect.com/science/article/pii/S2212671612002338}
#'
#' @importFrom EBImage thresh opening fillHull bwlabel
#' @export

countNuclei <- function(img){

  # check image file is of suitable type
  validImage(img)

  # use alternative adaptive threshold method if optimal otsu option fails
  test <- try(otsu(img, range = c(-1, 2)), silent = TRUE)
  iserror <- inherits(test, "try-error")
  if(iserror){
    cat(paste("Optimal method cannot support the modified image,",
              "switched to alternative method.",
              "Consider using original greyscale image to improve counting" ,sep="\n"))

    mask = thresh(img, w=10, h=10, offset=0.5)
  } else {
    # apply the binary threshold calculated by otsu
    mask = img > otsu(img, range = c(-1, 2))
  }

  # fillHull fills in holes in objects
  nmask = fillHull(mask)

  # the bwlabel() function labels and 'counts' all connected objects in the foreground
  nbnuclei <- max(bwlabel(nmask))

  # outputs the total count
  cat('\n', 'Number of nuclei in this image =', nbnuclei,'\n')
  return(nbnuclei)
}
# [END]

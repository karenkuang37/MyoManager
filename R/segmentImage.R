#' Generate segmented images of cellular structures
#'
#' The following function uses several functions from \code{EBImage} to
#' perform image segmentation with the provided frame numbers of cell and nuclei
#' channels.
#'
#' Segmentation of individual image channels is often useful when it is difficult
#' to distinguish structures by eye. (e.g. Visualizing segmentation helps users
#' locate nuclei within a mesh of muscle fibers)
#'
#' @param img An object of Image class specific to EBImage, stored as multi-
#' dimensional arrays containing the pixel intensities.
#' @param cell_frame A \code{numeric} value indicating the frame in the composite image
#' that contains stained cell (fiber)
#' @param nuc_frame A \code{numeric} value indicating the frame in the composite image
#' that contains stained nuclei
#' @param show_structure A \code{character} vector specifying which segmented structure
#' to show. Can be cell, nuclei, or both (segmented nuclei overlaying segmented cells).
#' Default is both.
#'
#' @return Returns an \code{Image} containing the segmented structure(s) outlined.
#'
#' @examples
#' # Example 1
#' rabbit <- loadImage(system.file('extdata/Rabbit_01.tif', package = 'MyoManager'))
#' # view image in Grayscale to find out which Frame stores cell body / nuclei
#' viewImage(rabbit, 0)
#' segmented_nuc <- segmentImage(rabbit, 1, 3, 'nuclei')
#' # only nuclei segmentation is generated
#' viewImage(segmented_nuc)
#'
#'
#' # Example 2
#' mouse <- loadImage(system.file('extdata/Mouse_01.tiff', package = 'MyoManager'))
#' viewImage(mouse, 0)
#' segmented_nuc <- segmentImage(mouse, 2, 3)
#' # cell segmentation with highlighted nuclei is generated
#' viewImage(segmented_nuc)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#'Carpenter, A.E., Jones, T.R., Lamprecht, M.R. et al. (2006) CellProfiler: image
#'analysis software for identifying and quantifying cell phenotypes. \emph{Genome Biol}
#'R100:7. \href{https://doi.org/10.1186/gb-2006-7-10-r100}{link}
#'CellProfiler software: \url{http://www.cellprofiler.org}
#'
#'
#' @import EBImage
#' @export
segmentImage <- function(img,
                         cell_frame,
                         nuc_frame,
                         show_structure = c('cell', 'nuclei', 'both')){

  # check image file is of suitable type
  validImage(img)

  # select frames
  cel = selectFrame(img, cell_frame)
  nuc = selectFrame(img, nuc_frame)

  # first segment nuclei

  # ensure nuclei and cell images are in Grayscale
  if (EBImage::colorMode(nuc)!=0){
    EBImage::colorMode(nuc) <- 0
  }
  if(EBImage::colorMode(cel)!=0){
    EBImage::colorMode(cel) <- 0
  }

  # apply the binary threshold calculated by otsu
  nmask = nuc > EBImage::otsu(nuc, range = c(-1, 2))
  # fills in holes in the objects
  nmask = EBImage::fillHull(nmask)
  # labels all connected objects in the foreground
  nmask = EBImage::bwlabel(nmask)

  # then segment cells
  # opening performs an erosion followed by a dilation
  ctmask = EBImage::opening(cel>0.1, makeBrush(5, shape='disc'))
  # propagate uses identified nuclei as 'seeds' to find boundaries between adjacent regions in an image
  cmask = EBImage::propagate(cel, seeds=nmask, mask=ctmask)

  # ensure original image is in Color
  if(EBImage::colorMode(img)!=2){
    EBImage::colorMode(img) <- 2
  }

  # generate segmented image based on user choice
  if(!missing(show_structure)){
    if(show_structure == 'cell'){
      segmented = EBImage::paintObjects(ctmask, img, col='#ff00ff', thick = TRUE)
      cat(paste("only cell segmentation is generated"))
    } else if(show_structure == 'nuclei'){
      segmented = EBImage::paintObjects(nmask, img, col='#ffff00', thick = TRUE)
      cat(paste("only nuclei segmentation is generated"))
    } else if(show_structure == 'both'){
      segmented = EBImage::paintObjects(cmask, img, col='#ff00ff')
      segmented = EBImage::paintObjects(nmask, segmented, col='#ffff00', thick = TRUE)
      cat(paste("cell segmentation with highted nuclei is generated"))
    } else {
      stop(
        paste("show_structure must be 'cell', 'nuclei', or 'both'.")
      )
    }
  } else {
    # if not specified, both cell and nuclei segmentation will be generated
    segmented = EBImage::paintObjects(cmask, img, col='#ff00ff')
    segmented = EBImage::paintObjects(nmask, segmented, col='#ffff00', thick = TRUE)
    cat(paste("cell segmentation with highlighted nuclei is generated"))
  }

  return(segmented)
}
#' # [END]

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
#' @param img A single-frame Grayscale \code{Image} containing ONLY the nuclei.
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

  # ensure nuclei image is in Grayscale
  if (colorMode(img)!=0){
    colorMode(img) <- 0
  }

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
#' @param img A single-frame Grayscale \code{Image} containing ONLY the nuclei.
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
getFeatureData <- function(img){

  # check image file is of suitable type
  validImage(img)

  # ensure nuclei image is in Grayscale
  if (colorMode(nuc)!=0){
    colorMode(nuc) <- 0
  }
  # Retrieve feature data from a binary mask of nuclei objects
  nmask = nuc > otsu(img, range = c(-1, 2))
  nmask = fillHull(nmask)
  nmask = bwlabel(nmask)
  # get shape feature: eccentricity
  general <- computeFeatures(nmask, img)
  shape <- as.data.frame(rNuc_general[,4])
  colnames(rNuc_shape) <- 's.eccentricity'
  # get size features: area, perimeter, and mean radius
  size <- computeFeatures.shape(nmask)
  size <- as.data.frame(rNuc_size[,-4:-6])

  # combine shape and size together
  ss <- cbind(rNuc_size, rNuc_shape)
  # filter out objects that are too small
  ss_filtered <- subset(ss_filtered, ss_filtered$s.area>1)
  # re-arrange nuclei data by area (small -> large)
  ss_filtered <- arrange(rNuc_filtered, rNuc_filtered$s.area)

  return(ss_filtered)
  # Retrieve feature data from a binary mask of nuclei objects
  nmask = nuc > otsu(img, range = c(-1, 2))
  nmask = fillHull(nmask)
  nmask = bwlabel(nmask)
  # get shape feature: eccentricity
  general <- computeFeatures(nmask, img)
  shape <- as.data.frame(rNuc_general[,4])
  colnames(rNuc_shape) <- 's.eccentricity'
  # get size features: area, perimeter, and mean radius
  size <- computeFeatures.shape(nmask)
  size <- as.data.frame(rNuc_size[,-4:-6])

  # combine shape and size together
  ss <- cbind(rNuc_size, rNuc_shape)
  # filter out objects that are too small
  ss_filtered <- subset(ss_filtered, ss_filtered$s.area>1)
  # re-arrange nuclei data by area (small -> large)
  ss_filtered <- arrange(rNuc_filtered, rNuc_filtered$s.area)

  return(ss_filtered)
}
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
#' @param img A single-frame Grayscale \code{Image} containing ONLY the nuclei.
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
plotFeature <- function(featureDF, feature = c("area", "perimeter", "radius", "roundness")){

  # check if features are in a data frame
  if(!is.data.frame(feature_df)||length(feature_df)!=4){
    stop(
      paste("featureDF must be a data frame of features generated by featureData()")
    )
  }

  # check feature input is of correct string
  if(missing(feature)){
    stop(
      paste("please specify a feature, can be 'area', 'perimeter', 'radius', or 'roundness'.")
    )
  } else if (feature %in% list("area", "perimeter", "radius", "roundness")){
    stop(
      paste("feature must be one of 'area', 'perimeter', 'radius', or 'roundness'.")
    )
  }

  # Plotting the selected feature
  if(feature=="area"){
    plot <- featureDF %>%
      ggplot( aes(x = s.area, y = ..density..)) + labs(x = "object area") +
      geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
      ggtitle("Distribution of nuclei surface area") +
      theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  } else if(feature=="perimeter"){
    plot <- featureDF %>%
      ggplot( aes(x = s.perimeter, y = ..density..)) + labs(x = "object perimeter") +
      geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
      ggtitle("Distribution of nuclei perimeter") +
      theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  } else if(feature=="radius"){
    plot <- featureDF %>%
      ggplot( aes(x = s.radius, y = ..density..)) + labs(x = "object mean radius") +
      geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
      ggtitle("Distribution of nuclei mean radius") +
      theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  } else if(feature=="roundness"){
    plot <- featureDF %>%
      ggplot( aes(x = s.eccentricity, y = ..density..)) +
      labs(x = "object ecc value", subtitle = "(0 = perfect circle)") +
      geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
      ggtitle("Distribution of nuclei eccentricity") +
      theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  }
  plot
}
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
#' @param img A single-frame Grayscale \code{Image} containing ONLY the nuclei.
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
plotFeatureMatrix <- function(featureDF){

  # check if features are in a data frame
  if(!is.data.frame(feature_df)||length(feature_df)!=4){
    stop(
      paste("featureDF must be a data frame of features generated by featureData()")
    )
  }

  # plot pairwise scatter matrix with correlation labels
  plot <- featureDF %>%
    ggpairs(columns = c("s.area", "s.perimeter", "s.radius.mean", "s.eccentricity"),
            title = "Nuceli Morphology Matrix",
            columnLabels = c("Area","Perimeter","Radius","Eccentricity"),
            upper = list(continuous = wrap('cor', size=4)))
  plot
}
# [END]

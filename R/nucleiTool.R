utils::globalVariables(c("colorMode", "aes", "s.area", "..density..", "margin", "element_rect", "s.perimeter", "s.radius.mean", "s.eccentricity", "%>%", "wrap"))
#' Count the number of distinct objects in an image (nuclei)
#'
#' The following function uses several functions from \code{EBImage} to
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
#' @param img A single-frame grayscale \code{Image} containing ONLY nuclei signals.
#'
#' @return Returns a \code{numeric} value of the number of nuclei counted.
#'
#' @examples
#' # Example 1
#' rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
#' rNuc = selectFrame(rabbit, 3)
#' countNuclei(rNuc)
#' viewImage(rNuc)
#'
#' # Example 2
#' mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' mNuc = selectFrame(mouse, 3)
#' countNuclei(mNuc)
#' viewImage(mNuc)
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
#' @import EBImage
#' @export
countNuclei <- function(img){

  # check image file is of suitable type
  validImage(img)

  # ensure nuclei image is in Grayscale
  if (EBImage::colorMode(img)!=0){
    EBImage::colorMode(img) <- 0
  }

  # use alternative adaptive threshold method if optimal otsu option fails
  test <- try(EBImage::otsu(img, range = c(-1, 2)), silent = TRUE)
  iserror <- inherits(test, "try-error")
  if(iserror){
    cat(paste("Optimal method cannot support the modified image,",
              "switched to alternative method.",
              "Consider using original greyscale image to improve counting" ,sep="\n"))

    mask = EBImage::thresh(img, w=10, h=10, offset=0.5)
  } else {
    # apply the binary threshold calculated by otsu
    mask = img > EBImage::otsu(img, range = c(-1, 2))
  }

  # fillHull() fills in holes in objects
  nmask = EBImage::fillHull(mask)
  # bwlabel() converts the image to binary (with 0 or 1 pixel value)
  nmask = EBImage::bwlabel(nmask)

  # extract a list of nuclei object areas
  size <- EBImage::computeFeatures.shape(nmask)
  area <- as.list(size[,-2:-6])

  # find indices of background noise disguised as small objects (area < 10 pixels)
  # note threshold is hard coded, may not be ideal for adaption
  l <- list()
  i <- 1
  for(v in area){
    if(v <= 10){
      l <- append(l, i)
    }
    i <- i + 1
  }

  # remove objects by indexing
  for(i in 1:length(l)){
    nmask <- EBImage::rmObjects(nmask, l[i], reenumerate = FALSE)
  }

  # bwlabel() labels and 'counts' all connected objects in the foreground
  nbnuclei <- max(EBImage::bwlabel(nmask))

  # outputs the total count
  cat('\n', 'Number of nuclei in this image =', nbnuclei,'\n')
  return(nbnuclei)
}
#' Generate a data frame of nuclei shape and size features
#'
#' The following function uses \code{computeFunction} and \code{computeFunction.shape}
#' from \code{EBImage} tp produce a data frame containing the following features:
#' area, perimeter, mean radius, and eccentricity. ( elliptical eccentricity defined by
#' sqrt(1-minoraxis^2/majoraxis^2). This value approaches 0 for rounder objects and 1 for
#' elongated objects.
#'
#' *Note Frame of only nuclei (typically frame #3 of composite image) is needed, since
#' feature data extraction can only be performed on single-frame Grayscale images with
#' defined binary objects.
#'
#' @param img A single-frame grayscale \code{Image} containing ONLY the nuclei.
#'
#' @return Returns a \code{data frame} containing area, perimeter, mean radius, and
#' eccentricity of each nuclei objects in the \code{Image}.
#'
#' @examples
#' # Example 1
#' rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
#' rNuc = selectFrame(rabbit, 3)
#' rNucFeatures_df = getFeatureData(rNuc)
#'
#' # Example 2
#' mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' mNuc = selectFrame(mouse, 3)
#' mNucFeatures_df = getFeatureData(mNuc)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @import EBImage
#' @importFrom dplyr arrange
#' @export
getFeatureData <- function(img){

  # check image file is of suitable type
  validImage(img)

  # ensure nuclei image is in Grayscale
  if (EBImage::colorMode(img)!=0){
    EBImage::colorMode(img) <- 0
  }
  # Retrieve feature data from a binary mask of nuclei objects
  nmask = img > EBImage::otsu(img, range = c(-1, 2))
  nmask = EBImage::fillHull(nmask)
  nmask = EBImage::bwlabel(nmask)
  # get shape feature: eccentricity
  general <- EBImage::computeFeatures.moment(nmask)
  shape <- as.data.frame(general[,4])
  colnames(shape) <- 's.eccentricity'
  # get size features: area, perimeter, and mean radius
  size <- EBImage::computeFeatures.shape(nmask)
  size <- as.data.frame(size[,-4:-6])

  # combine shape and size together
  ss <- cbind(size, shape)
  # filter out objects that are too small
  ss_filtered <- subset(ss, ss$s.area>1)
  # re-arrange nuclei data by area (small -> large)
  ss_filtered <- dplyr::arrange(ss_filtered, ss_filtered$s.area)

  return(ss_filtered)
}
#' Plot a shape/size feature of nuclei objects.
#'
#' The following function plots the density distribution of one of four shape/size features
#' of cell nuclei computed from getFeatureData(): area, perimeter, radius, or eccentricity.
#'
#' @param featureDF A \code{data frame} containing four columns of shape/size features for
#' each nuclei objects in an \code{Image}.
#' @param feature A \code{character} vector indicating which feature to plot, can be
#' area, perimeter, radius, or roundness.
#'
#' @return A density plot of indicating the distribution of quantified shape/size feature
#' of cell nulcei.
#'
#' @examples
#' # Example 1
#' rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
#' rNuc = selectFrame(rabbit, 3)
#' df = getFeatureData(rNuc)
#' plotFeature(df, "area")
#'
#' # Example 2
#' plotFeature(df, "roundness")
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @import ggplot2
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @export
plotFeature <- function(featureDF, feature = c("area", "perimeter", "radius", "roundness")){

  # check if features are in a data frame
  if(!is.data.frame(featureDF)||length(featureDF)!=4){
    stop(
      paste("featureDF must be a data frame of features generated by featureData()")
    )
  }

  # check feature input is of correct string
  if(missing(feature)){
    stop(
      paste("please specify a feature, can be 'area', 'perimeter', 'radius', or 'roundness'.")
    )
  } else if (!feature %in% list("area", "perimeter", "radius", "roundness")){
    stop(
      paste("feature must be one of 'area', 'perimeter', 'radius', or 'roundness'.")
    )
  }

  # Plotting the selected feature
  if(feature=="area"){
    plot <- featureDF %>%
      ggplot2::ggplot( aes(x = s.area, y = (..count..)/sum(..count..))) +
      ggplot2::labs(x = "object area", y = "Percentage") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 10000)) +
      ggplot2::geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
      ggplot2::ggtitle("Distribution of nuclei surface area") +
      ggplot2::theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  } else if(feature=="perimeter"){
    plot <- featureDF %>%
      ggplot2::ggplot( aes(x = s.perimeter, y = (..count..)/sum(..count..))) +
      ggplot2::labs(x = "object perimeter", y = "Percentage") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 10000)) +
      ggplot2::geom_density(fill="#9ecae1", color="#9ecae1", alpha=0.8) +
      ggplot2::ggtitle("Distribution of nuclei perimeter") +
      ggplot2::theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  } else if(feature=="radius"){
    plot <- featureDF %>%
      ggplot2::ggplot( aes(x = s.radius.mean, y = (..count..)/sum(..count..))) +
      ggplot2::labs(x = "object mean radius", y = "Percentage") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 10000)) +
      ggplot2::geom_density(fill="#feb24c", color="#feb24c", alpha=0.8) +
      ggplot2::ggtitle("Distribution of nuclei mean radius") +
      ggplot2::theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  } else if(feature=="roundness"){
    plot <- featureDF %>%
      ggplot2::ggplot( aes(x = s.eccentricity, y = (..count..)/sum(..count..))) +
      ggplot2::labs(x = "object ecc value", y = "Percentage", subtitle = "(0 = perfect circle)") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 10000)) +
      ggplot2::geom_density(fill="#fa9fb5", color="#fa9fb5", alpha=0.8) +
      ggplot2::ggtitle("Distribution of nuclei eccentricity") +
      ggplot2::theme(plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
            plot.background = element_rect(
              fill = "white",
              size = 1))
  }
  return(plot)
}
#' Plot a matrix of pairwise shape/size feature of nuclei objects.
#'
#' The following function plots a matrix of pairwise scatter plots and density distribution
#' of the four shape/size features of cell nuclei computed from getFeatureData():
#' area, perimeter, radius, and eccentricity. Correlation value of the four
#' features is also calculated.
#'
#' @param featureDF A \code{data frame} containing four columns of shape/size features for
#' each nuclei objects in an \code{Image}.
#'
#' @return A matrix composed of pairwise scatter plots, density distribution, and correlation
#' values of the four shape/size features of cell nuclei.
#'
#' @examples
#' # Example 1
#' rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
#' rNuc = selectFrame(rabbit, 3)
#' rNuc_df = getFeatureData(rNuc)
#' plotFeatureMatrix(rNuc_df)
#'
#' # Example 2
#' mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
#' mNuc = selectFrame(mouse, 3)
#' mNuc_df = getFeatureData(mNuc)
#' plotFeatureMatrix(mNuc_df)
#'
#' @references
#'Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
#'(2010): EBImage - an R package for image processing with applications to
#'cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
#'\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
#'\url{https://bioconductor.org/packages/release/bioc/html/EBImage.html}
#'
#' @importFrom GGally ggpairs wrap
#' @importFrom magrittr %>%
#' @export
plotFeatureMatrix <- function(featureDF){

  # check if features are in a data frame
  if(!is.data.frame(featureDF)||length(featureDF)!=4){
    stop(
      paste("featureDF must be a data frame of features generated by featureData()")
    )
  } else(
    # consider adding more detailed prints of what image displays
    cat(paste("Generating a matrix of pairwise scatter and density plots.",
              "See upper panel for correlation values of nuclei shape/size features.",
              " "
              ,sep="\n"))
  )

  # plot pairwise scatter matrix with correlation labels
  fMatrix <- featureDF %>%
    GGally::ggpairs(columns = c("s.area", "s.perimeter", "s.radius.mean", "s.eccentricity"),
            title = "Nuceli Morphology Matrix",
            columnLabels = c("Area","Perimeter","Radius","Eccentricity"),
            upper = list(continuous = GGally::wrap('cor', size=4)))

  return(fMatrix)
}
# [END]

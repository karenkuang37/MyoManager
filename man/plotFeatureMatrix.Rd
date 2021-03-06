% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nucleiTool.R
\name{plotFeatureMatrix}
\alias{plotFeatureMatrix}
\title{Plot a matrix of pairwise shape/size feature of nuclei objects.}
\usage{
plotFeatureMatrix(featureDF)
}
\arguments{
\item{featureDF}{A \code{data frame} containing four columns of shape/size features for
each nuclei objects in an \code{Image}.}
}
\value{
A matrix composed of pairwise scatter plots, density distribution, and correlation
values of the four shape/size features of cell nuclei.
}
\description{
The following function plots a matrix of pairwise scatter plots and density distribution
of the four shape/size features of cell nuclei computed from getFeatureData():
area, perimeter, radius, and eccentricity. Correlation value of the four
features is also calculated.
}
\examples{
# Example 1
rabbit = loadImage(system.file('extdata/Rabbit_01.tif', package='MyoManager'))
rNuc = selectFrame(rabbit, 3)
rNuc_df = getFeatureData(rNuc)
plotFeatureMatrix(rNuc_df)

# Example 2
mouse = loadImage(system.file('extdata/Mouse_01.tiff', package='MyoManager'))
mNuc = selectFrame(mouse, 3)
mNuc_df = getFeatureData(mNuc)
plotFeatureMatrix(mNuc_df)

}
\references{
Gregoire Pau, Florian Fuchs, Oleg Sklyar, Michael Boutros, and Wolfgang Huber
(2010): EBImage - an R package for image processing with applications to
cellular phenotypes. \emph{Bioinformatics}, 26(7), pp. 979-981,
\href{https://pubmed.ncbi.nlm.nih.gov/20338898/}{link}
\url{https://bioconductor.org/packages/release/bioc/html/EBImage.html}
}

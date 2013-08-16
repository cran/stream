\name{DSC_DBSCAN}
\alias{DSC_DBSCAN}
\title{DBSCAN Macro-clusterer}
\description{
    Class implements the DBSCAN algorithm for reclustering
         micro-clusterings.
}

\usage{
	DSC_DBSCAN(eps, MinPts = 5, weighted = TRUE)
}

\arguments{
	\item{eps}{Reachability Distance.}
	\item{MinPts}{Reachability minimum no. of points.}
  \item{weighted}{logical indicating if a weighted version of DBSCAN should be used. }
}

\details{
DBSCAN is a weighted extended version of the inplementation in \pkg{fpc} where
each micro-cluster center considered a pseudo point. For weighting we use
in the MinPts comparison the sum of weights of the micro-cluster instead of the 
number.
}

\value{
An object of class \code{DSC_DBSCAN} (a subclass of \code{DSC}, 
\code{DSC_R}, \code{DSC_Macro}).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

# Use DBSCAN to recluster micro clusters
dsc <- DSC_ClusTree()
cluster(dsc, dsd, 500)

DBSCAN <- DSC_DBSCAN(eps = .05)
recluster(DBSCAN,dsc)
plot(DBSCAN, dsd)

# cluster some data with DBSCAN directly
DBSCAN <- DSC_DBSCAN(eps = .05)
cluster(DBSCAN, dsd, 500)
plot(DBSCAN, dsd)
}

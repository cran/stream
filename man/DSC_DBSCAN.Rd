\name{DSC_DBSCAN}
\alias{DSC_DBSCAN}
\title{DBSCAN Macro-clusterer}
\description{
    Class implements the DBSCAN algorithm for reclustering
         micro-clusterings.
}

\usage{
	DSC_DBSCAN(eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw", "dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)
}

\arguments{
	\item{eps}{Reachability Distance.}
	\item{MinPts}{Reachability minimum no. of points.}
	\item{scale}{scale the data}
	\item{method}{"dist" treats data as distance matrix (relatively fast
	but memory expensive), "raw" treats data as raw data and avoids
	calculating a distance matrix (saves memory but may be slow), "hybrid"
	expects also raw data, but calculates partial distance matrices (very
	fast with moderate memory requirements ).}
	\item{seeds}{FALSE to not include the seed-vector in the
	DBSCAN-object.}
	\item{showplot}{0 = no plot, 1 = plot per iteration, 2 = plot per
	sub-iteration.}
	\item{countmode}{NULL or vector of point numbers at which to report
	progress.}
}

\details{
Please refer to package \code{DBSCAN} for more details on the behavior of
\code{DBSCAN}.
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

DBSCAN <- DSC_DBSCAN(eps = .02)
recluster(DBSCAN,dsc)
plot(DBSCAN, dsd)

# cluster some data with DBSCAN directly
DBSCAN <- DSC_DBSCAN(eps = .02)
cluster(DBSCAN, dsd, 500)
plot(DBSCAN, dsd)
}

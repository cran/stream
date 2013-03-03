\name{DSC_KmeansW}
\alias{DSC_KmeansW}
\title{Weighted k-Means Macro-clusterer}
\description{
     Class implements the weighted k-means algorithm for reclustering 
     micro-clusterings. The weights are the micro-cluster weights.
}
\usage{
	DSC_KmeansW(k, iter.max = 10, nstart = 1)
}

\arguments{
	\item{k}{either the number of clusters, say k, or a set of initial (distinct) cluster centers. If a number, a random set of (distinct) rows in x is chosen as the initial centers.}
	\item{iter.max}{the maximum number of iterations allowed.}
	\item{nstart}{if centers is a number, how many random sets should be chosen?}
}

%\details{
%}

\value{
An object of class  \code{DSC_Kmeans} (subclass of \code{DSC}, 
\code{DSC_R}, \code{DSC_Macro}).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}},
	\code{\link{DSC_Kmeans}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0)

# recluster a micro-clusters
dsc <- DSC_BIRCH(radius=0.01)
cluster(dsc, dsd, 500)

kmeans <- DSC_KmeansW(3)
recluster(kmeans,dsc)
plot(kmeans, dsd)

# cluster a data directly
kmeans <- DSC_KmeansW(3)
cluster(kmeans, dsd, 500)
plot(kmeans, dsd)
}

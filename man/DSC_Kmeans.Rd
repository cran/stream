\name{DSC_Kmeans}
\alias{DSC_Kmeans}
\title{Kmeans Macro-clusterer}
\description{
    Class implements the k-means algorithm for reclustering micro-clusterings.
    }

\usage{
    DSC_Kmeans(k, iter.max = 10, nstart = 1,
	    algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
}

\arguments{
	\item{k}{either the number of clusters, say k, or a set of initial
	(distinct) cluster centers. If a number, a random set of (distinct)
	rows in x is chosen as the initial centers.}
	\item{iter.max}{the maximum number of iterations allowed.}
	\item{nstart}{if centers is a number, how many random sets should be
	chosen?}
	\item{algorithm}{character: may be abbreviated.}
}

\details{
Please refer to function \code{kmeans} in \pkg{stats} for more details
on the algorithm.
}

\value{
    An object of class \code{DSC_Kmeans} (subclass of \code{DSC}, 
    \code{DSC_R}, \code{DSC_Macro})
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}},
	\code{\link{DSC_KmeansW}} for a weighted version of k-means.
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0)

dsc <- DSC_BIRCH(radius=0.01)
cluster(dsc, dsd, 500)

# recluster micro-clusters
kmeans <- DSC_Kmeans(3)
recluster(kmeans, dsc)
plot(kmeans, dsd)

# Kmeans can also directly cluster data (original data are considered 
# micro-cluster centers)
kmeans <- DSC_Kmeans(3)
cluster(kmeans, dsd, 500)
plot(kmeans, dsd)
}
